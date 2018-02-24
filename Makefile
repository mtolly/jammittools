# Builds win/mac/linux binaries from a Mac with Vagrant.

program := jammittools
release := $(shell release/version)

.PHONY: all osx linux win32
all: osx linux win32
osx: release/${release}-osx-x64.zip
linux: release/${release}-linux-x64.tar.gz
win32: release/${release}-win32-x86.zip

release/${release}-osx-x64.zip:
	stack setup
	stack build
	cp .stack-work/install/x86_64-osx/*/*/bin/jammittools jammittools
	strip jammittools
	zip $@ jammittools README.md LICENSE easy-export.applescript raw-export.applescript
	rm jammittools

release/${release}-linux-x64.tar.gz:
	vagrant up linux
	vagrant ssh linux -c "cd /vagrant && stack setup && stack build"
	cp .stack-work/install/x86_64-linux/*/*/bin/jammittools jammittools
	vagrant ssh linux -c "cd /vagrant && strip jammittools"
	tar -cvzf $@ jammittools README.md LICENSE
	rm jammittools

release/${release}-win32-x86.zip:
	vagrant up wine
	vagrant ssh wine -c "cd /vagrant && wine stack setup && wine stack build"
	cp .stack-work/install/*/bin/jammittools.exe jammittools.exe
	zip $@ jammittools.exe README.md LICENSE easy-export.bat raw-export.bat
	rm jammittools.exe
