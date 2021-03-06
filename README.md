# `jammittools`

[![Available on Hackage](https://img.shields.io/hackage/v/jammittools.svg)](http://hackage.haskell.org/package/jammittools)

A command-line tool for exporting sheet music and audio from the (defunct) Windows/Mac app [Jammit].
It should go without saying, but please do not distribute content from songs you have purchased—it is for your use only!

[Jammit]: https://www.jammit.com/

## Easy export instructions

1. Download songs through the Windows or Mac Jammit app.

2. Download `jammittools` from the [releases page](https://github.com/mtolly/jammittools/releases).

3. Unzip somewhere, and run the included `easy-export` script. Song folders will start being extracted in that same folder.

## Command-line instructions

    jammittools -?
    # Print usage info.

    jammittools -d
    # Displays the entire library.

    jammittools -t "My Song" -r "Some Artist" -x dir
    # Easiest export option: exports all sheet music and audio for a song.

    jammittools -t "My Song" -r "Some Artist" -s file.pdf -y <parts>
    # Exports a single sheet music file with a list of parts, interleaved.
    # See below for the syntax of instrument parts.

    jammittools -t "My Song" -r "Some Artist" -a file.wav -y <parts>
    # Exports a single audio file with a list of parts, mixed together.
    # See below for the syntax of instrument parts.

    jammittools -t "My Song" -r "Some Artist" -m file.wav
    # Exports the metronome click track for the song.

    jammittools -t "My Song" -r "Some Artist" -c -y <parts>
    # Do a "dry run" of audio extraction, which checks if the parts exist
    # but does not do any conversion. Exits with a non-zero code if any
    # part does not exist.

    jammittools -b dir
    # Full library backup: runs -x for each song in your library.
    # Each song is extracted to its own folder within "dir".

Other lesser-used flags:

    -j /path/to/jammit/lib
    # On Windows and OS X, the official app's library location is used
    # if you do not specify this flag.
    # You can also specify the environment variable JAMMIT.
    # Songs in the same folder as the program will also always be found.

    -T "My Song"
    # Exact search on title, instead of -t which is case-insensitive substring.

    -R "Some Artist"
    # Exact search on artist, instead of -r which is case-insensitive substring.

    -n <parts>
    # Allows you to invert certain audio parts when exporting a WAV file.
    # This can be used to access "hidden" parts that aren't a part of any
    # transcribed instrument part, see below.

    -l <number>
    # Select the number of sheet music systems per page.
    # One system contains a line from each individual part.
    # If this flag is not given, an appropriate number of systems will be chosen
    # to get close to an 8.5" by 11" page ratio.

    --raw
    # For -x and -b options, exports all original backing tracks separately
    # instead of producing a combined one.

Instrument parts are given by this somewhat terse syntax:

    g - Guitar (1)
    r - Guitar 2
    b - Bass (1)
    a - Bass 2
    d - Drums (1)
    m - Drums 2
    k - Keyboard (1)
    y - Keyboard 2
    p - Piano
    s - Synth
    o - Organ
    v - Vocals (Lead)
    x - Vocals (Backing)
    GRBA - in sheet music, tab instead of notation
    GBDKV - in audio, the backing track for an instrument

So, for example, to make a backing track consisting of just drums and bass:

    jammittools <search parameters> -a out.wav -y db

To make a sheet music file with Guitar 1's notation and tab interleaved:

    jammittools <search parameters> -s out.pdf -y gG

## Accessing hidden parts

Not all the audio in a song has been transcribed and sold by Jammit. Things such
as sound-effects, other instruments like violin, and occasionally third guitar
or keyboard parts are "hidden" inside the backing tracks for every instrument
package that you can purchase. You can listen to these parts by performing
audio subtraction, where you take two audio files, invert one of them, and then
mix them together.

Let's say you own all 5 instruments for a song. You can access the "hidden"
track with a command similar to the following:

    jammittools <search parameters> -a out.wav -y D -n grbkyvx

This uses the `-n` flag to mix many audio files in after inverting them.
The `D` part is the backing track for the drums package. What this does is
subtract the non-drums instrument parts from this backing track, leaving you
with just the portion of the song that isn't present in any of the transcribed
parts. If you use the `-x` or `-b` options to export all parts of a song, this
process will be done for you and placed in a file called `backing.wav`.

The resulting file has a thin layer of noise, because all the audio used in
Jammit is lossily encoded, which means the backing tracks are not quite perfect
mixes of their constituent parts. But the results are generally pretty good,
and you can easily use something like [Audacity]'s noise filter to clean it up
further. Using the drums backing track as a base is preferred, because drums
leaves the most audible noise when you remove it from a different backing track.

[Audacity]: http://audacity.sourceforge.net/
