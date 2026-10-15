"""Exercise the installer in temporary homes without contacting live services."""
from contextlib import ExitStack, redirect_stdout
import importlib.machinery
import importlib.util
import io
import json
from pathlib import Path
import plistlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
SOURCE = Path(__file__).resolve().parent.parent


def load_script(name, filename):
    loader = importlib.machinery.SourceFileLoader(name, str(SOURCE / "bin" / filename))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


installer = load_script("installer", "install-emacs-daemon")
launcher = load_script("launcher", "emacs-client")


class InstallerTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="emacs-desktop-tests-")
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(self.temporary.name) / "用户 home"
        self.root = self.home / ".emacs.d"
        self.root.mkdir(parents=True)
        shutil.copytree(SOURCE / "macos", self.root / "macos")
        self.app = self.home / "Applications/Emacs Test.app"
        for relative in ("Contents/MacOS/Emacs", "Contents/MacOS/bin/emacsclient"):
            binary = self.app / relative
            binary.parent.mkdir(parents=True, exist_ok=True)
            binary.touch()
        self.hs = self.home / ".hammerspoon/init.lua"
        self.hs.parent.mkdir()
        self.hs.write_text("-- other settings\n" + installer.OLD_HOTKEY + "\n")
        self.custom = self.root / "init-custom.el"
        self.custom.write_text(";;; Personal settings\n(setq test-value 42)\n")
        self.agent = self.home / "Library/LaunchAgents/local.emacs.daemon.plist"

    def run_installer(self, *arguments, state=None):
        with ExitStack() as stack:
            stack.enter_context(patch.object(installer, "REPOSITORY", self.root))
            stack.enter_context(patch.object(installer.Path, "home", return_value=self.home))
            stack.enter_context(patch.object(installer.sys, "platform", "darwin"))
            stack.enter_context(patch.object(sys, "argv", ["install-emacs-daemon", "--emacs-app", str(self.app), *arguments]))
            run = stack.enter_context(patch.object(installer.subprocess, "run", return_value=subprocess.CompletedProcess([], 1)))
            evaluate = stack.enter_context(patch.object(installer, "evaluate", return_value=state))
            stack.enter_context(redirect_stdout(io.StringIO()))
            installer.main()
            return run, evaluate

    def test_preview_writes_nothing_and_does_not_contact_emacs_or_launchd(self):
        original = self.hs.read_bytes()
        custom = self.custom.read_bytes()
        run, evaluate = self.run_installer("--dry-run")
        run.assert_not_called()
        evaluate.assert_not_called()
        self.assertFalse(self.agent.exists())
        self.assertFalse((self.root / "var").exists())
        self.assertFalse((self.home / "Applications/Emacs Client.app").exists())
        self.assertEqual(original, self.hs.read_bytes())
        self.assertEqual(custom, self.custom.read_bytes())

    def test_install_generates_paths_and_preserves_other_hammerspoon_settings(self):
        self.run_installer()
        agent = plistlib.loads(self.agent.read_bytes())
        self.assertEqual(agent["ProgramArguments"], [str(self.app / "Contents/MacOS/Emacs"), f"--init-directory={self.root}", "--fg-daemon"])
        self.assertEqual(agent["WorkingDirectory"], str(self.home))
        self.assertEqual(agent["StandardOutPath"], str(self.home / "Library/Logs/Emacs/daemon.log"))
        self.assertIn('-- other settings', self.hs.read_text())
        self.assertIn('dofile(os.getenv("HOME") .. "/.emacs.d/macos/hammerspoon-emacs.lua")', self.hs.read_text())
        self.assertNotIn("launchOrFocus", self.hs.read_text())
        self.assertEqual(self.custom.read_text(), ";;; -*- lexical-binding: nil; -*-\n;;; Personal settings\n(setq test-value 42)\n")
        executable = self.home / "Applications/Emacs Client.app/Contents/MacOS/emacs-client"
        self.assertTrue(executable.stat().st_mode & 0o111)
        # Execute only the generated wrapper against a harmless temporary probe.
        probe = self.root / "bin/emacs-client"
        probe.parent.mkdir()
        probe.write_text("import sys\nprint(repr(sys.argv[1:]))\n")
        result = subprocess.run([str(executable), "file with spaces.org"], text=True, capture_output=True, check=True)
        self.assertEqual(result.stdout.strip(), "['file with spaces.org']")
        before = self.hs.read_bytes()
        self.run_installer()
        self.assertEqual(before, self.hs.read_bytes())
        self.assertEqual(self.custom.read_text().count("lexical-binding:"), 1)

    def test_existing_binding_declarations_are_preserved(self):
        for contents in (b";;; -*- lexical-binding: t -*-\n(setq x 1)\n",
                         b";;; -*- lexical-binding: nil -*-\n(setq x 1)\n"):
            self.assertEqual(installer.with_binding_cookie(contents), contents)

    def test_gui_server_collision_stops_before_writing_files(self):
        original = self.hs.read_bytes()
        with self.assertRaisesRegex(RuntimeError, "GUI Emacs owns"):
            self.run_installer(state="0")
        self.assertFalse(self.agent.exists())
        self.assertFalse((self.root / "var").exists())
        self.assertEqual(original, self.hs.read_bytes())

    def test_modified_daemon_stops_before_writing_files(self):
        with self.assertRaisesRegex(RuntimeError, "Save modified buffers"):
            self.run_installer("--restart", state="1")
        self.assertFalse(self.agent.exists())
        self.assertFalse((self.root / "var").exists())

    def test_hammerspoon_migrates_earlier_inline_launcher_once(self):
        old = ("local emacsClientTask\n"
               "hs.hotkey.bind({'ctrl', 'cmd'}, '.', function()\n"
               "  emacsClientTask = hs.task.new('old', function() end)\n"
               "end)\n-- following settings\n")
        new = installer.hammerspoon_config(old, self.root, self.home)
        self.assertEqual(new.count(installer.BEGIN), 1)
        self.assertIn("-- following settings", new)
        self.assertEqual(new, installer.hammerspoon_config(new, self.root, self.home))

    def test_hammerspoon_unknown_binding_is_not_overwritten(self):
        with self.assertRaisesRegex(RuntimeError, "Unrecognized"):
            installer.hammerspoon_config("hs.hotkey.bind({'cmd', 'ctrl'}, '.', anotherAction)", self.root, self.home)


class LauncherTests(unittest.TestCase):
    def launch(self, arguments, replies):
        with ExitStack() as stack:
            stack.enter_context(patch.object(sys, "argv", ["emacs-client", *arguments]))
            stack.enter_context(patch.object(launcher.Path, "is_file", return_value=True))
            stack.enter_context(patch.object(launcher.Path, "exists", return_value=False))
            stack.enter_context(patch.dict(launcher.os.environ, {"EMACS_APP": "/Applications/Emacs.app"}))
            run = stack.enter_context(patch.object(launcher.subprocess, "run", side_effect=[subprocess.CompletedProcess([], 0, value, "") for value in replies]))
            execute = stack.enter_context(patch.object(launcher.os, "execv"))
            stack.enter_context(patch.object(launcher.os, "chdir"))
            launcher.main()
            return run, execute

    def test_existing_window_is_only_focused(self):
        run, execute = self.launch([], ["t", "t"])
        self.assertEqual(run.call_count, 2)
        execute.assert_not_called()

    def test_absent_window_gets_a_new_client(self):
        _, execute = self.launch([], ["t", "nil"])
        self.assertIn("--create-frame", execute.call_args.args[1])

    def test_file_arguments_are_kept_in_an_existing_window(self):
        _, execute = self.launch(["a b.org"], ["t", "t"])
        command = execute.call_args.args[1]
        self.assertNotIn("--create-frame", command)
        self.assertEqual(command[-1], "a b.org")

    def test_desktop_directory_is_home_but_relative_file_requests_keep_their_directory(self):
        with tempfile.TemporaryDirectory(prefix="emacs-client-cwd-") as temporary:
            caller = Path(temporary) / "hammerspoon"
            caller.mkdir()
            app = Path(temporary) / "Emacs.app"
            client = app / "Contents/MacOS/bin/emacsclient"
            client.parent.mkdir(parents=True)
            client.write_text("#!" + sys.executable + "\n"
                              "import json, os, sys\n"
                              "args = sys.argv[1:]\n"
                              "if '--eval' in args:\n"
                              "    print('t' if '+my/desktop-ready' in args[-1] else 'nil')\n"
                              "else:\n"
                              "    print(json.dumps({'cwd': os.getcwd(), 'args': args}))\n")
            client.chmod(0o755)
            with patch.dict(launcher.os.environ, {"EMACS_APP": str(app)}):
                for arguments in ([], ["relative file.org"]):
                    result = subprocess.run([sys.executable, str(SOURCE / "bin/emacs-client"), *arguments],
                                            cwd=caller, capture_output=True, text=True, check=True)
                    output = json.loads(result.stdout)
                    self.assertEqual(Path(output['cwd']).resolve(),
                                     (caller if arguments else Path.home()).resolve())
                    self.assertIn('--create-frame', output['args'])
                    if arguments:
                        self.assertEqual(output['args'][-1], 'relative file.org')


if __name__ == "__main__":
    unittest.main()
