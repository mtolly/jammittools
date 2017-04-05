#!/usr/bin/osascript
on run argv
	do shell script "\"" & (POSIX path of ((path to me as text) & "::")) & "jammittools\" -b \"" & (POSIX path of ((path to me as text) & "::")) & "\""
end run
