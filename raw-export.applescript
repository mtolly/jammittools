#!/usr/bin/osascript
on run argv
  do shell script "\"" & (POSIX path of ((path to me as text) & "::")) & "jammittools\" --raw -b \"" & (POSIX path of ((path to me as text) & "::")) & "\""
end run
