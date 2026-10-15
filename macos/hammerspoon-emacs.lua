-- The installer generates this entry point with this Mac's Python and Emacs paths.
local launcher = os.getenv("HOME") .. "/Applications/Emacs Client.app/Contents/MacOS/emacs-client"
local clientTask

hs.hotkey.bind({'ctrl', 'cmd'}, '.', function()
  if clientTask and clientTask:isRunning() then return end
  clientTask = hs.task.new(launcher,
    function(exitCode, stdout, stderr)
      clientTask = nil
      if exitCode ~= 0 then
        print("Emacs client: " .. stderr)
        hs.alert.show("Emacs client failed; see Hammerspoon Console")
      end
    end)
  if not clientTask:start() then
    clientTask = nil
    hs.alert.show("Could not start Emacs client")
  end
end)
