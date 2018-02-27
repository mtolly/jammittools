## v0.5.4

  * Program will always also look for songs in the folder it's in
  * Add option and script to export all backing tracks without combining them

## v0.5.3.2

Mac-only release, fixes some issues on Sierra

## v0.5.3

Added support for "Bass 2", "Vocals" (instead of "Vocal"),
and "Vocal 1"/"Vocal 2" (instead of "Vocal"/"B Vocals").

## v0.5.2

Added the ability to export the metronome click track.
Available via the `-m` option, or via the `-x` or `-b` full backup options
(including the `easy-export` script on Windows/Mac).

## v0.5.1

Added a `-b` option for backing up an entire library of songs.

## v0.5.0.3

Fixed some bugs in both audio and image decoding.

## v0.5.0.1

Fixed a bug in reading the `beats.plist` file
(not actually used in the executable).

## v0.5

  * Fixed a bug where `"Song Title "` wasn't considered the same as `"Song Title"`
  * Much internal cleanup, smaller executable size
  * Library: added some more Jammit types (beats, ghost beats, sections)

## v0.4.1.1

Fixes for GHC 7.10 (no new functionality).

## v0.4.1

Uses [`conduit-audio`](http://hackage.haskell.org/package/conduit-audio),
and exposes the audio file as an
[`AudioStream`](http://hackage.haskell.org/package/conduit-audio-0.1/docs/Data-Conduit-Audio.html#t:AudioSource).

## v0.4

  * Doesn't require ImageMagick or SoX anymore!
    Thanks to [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels),
    [HPDF](https://hackage.haskell.org/package/HPDF),
    [conduit](https://hackage.haskell.org/package/conduit),
    and audio code ported from [aifc2wav](http://sed.free.fr/aifc2wav.html).
  * Prints better instructions.
  * Prints what it is doing during each step of the `-x dir` export option.
  * Error if the search parameters don't narrow down the library
    to exactly one song.

## v0.3.2

Fixes for Windows, other platforms are unchanged:

  * Use `wchar_t` for C functions to support non-ASCII filenames
  * Look for SoX in Program Files (the installer doesn't put it in the `PATH`)

## v0.3.1

  * Added support for Drums 1, Drums 2 and Organ parts
  * Better argument handling (quits on unrecognized argument)

## v0.3

Split the package into a library and an executable.
No functional changes to the executable.

## v0.2.0.1

Documentation, packaging, and version bounds improvements.

## v0.2

  * Proper Mac support (knows the official library location).
  * Changed the long forms of some commands for consistency.
  * Removed `-u` and `-b`, replaced with a simple `-c` option which does a dry
    run of audio extraction. That is, it does no audio conversion, but exits
    with a code of 0 only if all specified audio parts are found.
  * Fixed a crash on very large libraries.

## v0.1.1

  * `-u` and `-b` audio options for
    [`onyxite-customs`](https://github.com/mtolly/onyxite-customs) project
  * `-T` and `-R` perform exact title/artist search,
    instead of existing `-t` and `-r` "case-insensitive substring" search

## v0.1

Binary compiled with GHC 7.6.3 32-bit on Windows 7 64-bit.
Should also work on Wine.
You must install [ImageMagick](http://www.imagemagick.org/script/index.php)
and [SoX](http://sox.sourceforge.net/)
for sheet music and audio export respectively.
