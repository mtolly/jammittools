release := jammittools-0.5.5

.PHONY: default mac win

default:
	@echo "usage:"
	@echo "  make mac"
	@echo "  make win"

mac: release/${release}-macos-x64.zip
win: release/${release}-win32-x64.zip

release/${release}-macos-x64.zip:
	stack setup
	stack build
	cp .stack-work/install/x86_64-osx/*/*/bin/jammittools jammittools
	strip jammittools
	cp README.md README.txt
	cp CHANGELOG.md CHANGELOG.txt
	cp LICENSE LICENSE.txt
	zip $@ jammittools README.txt CHANGELOG.txt LICENSE.txt easy-export.applescript raw-export.applescript
	rm jammittools README.txt CHANGELOG.txt LICENSE.txt

release/${release}-win32-x64.zip:
	stack setup
	stack build
	cp .stack-work/install/*/bin/jammittools.exe jammittools.exe
	strip jammittools.exe
	cp README.md README.txt
	cp CHANGELOG.md CHANGELOG.txt
	cp LICENSE LICENSE.txt
	zip $@ jammittools.exe README.txt CHANGELOG.txt LICENSE.txt easy-export.bat raw-export.bat
	rm jammittools.exe README.txt CHANGELOG.txt LICENSE.txt
