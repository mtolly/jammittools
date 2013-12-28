# jammittools

A command-line tool for exporting sheet music and audio from the Windows/Mac app Jammit.
It should go without saying, but please do not distribute content from songs you have purchased --
it is for your use only!

## Requirements

This is basically a fancy shell script but written in Haskell.
[ImageMagick](http://www.imagemagick.org) and [Sox](http://sox.sourceforge.net/)
are used to do the actual conversion work, so install those and put them in your path.
If you're on Windows, ImageMagick `convert` infamously conflicts with a system tool of the same name,
but `jammittools` tries looking in your Program Files so you shouldn't have to do anything.

## Usage

    jammittools -? # print usage
    jammittools -d # display library
    jammittools -x <dir> # export all parts sheet music & audio to directory
    jammittools -s <file.pdf> -y <parts> # export sheet music
    jammittools -a <file.wav> -y <parts> -n <parts inverted> # export audio

Other flags:

    -t <title to search>
    -r <artist to search>
    -j <jammit library dir> # by default, looks for platform's default location
    -l <# of systems per sheet music page>

Instrument parts are given by this somewhat terse syntax:

    g - Guitar (1)
    r - Guitar 2
    b - Bass
    d - Drums
    k - Keyboard (1)
    y - Keyboard 2
    p - Piano
    s - Synth
    v - Vocals (1)
    x - Vocals (2/Backing)
    GRB - in sheet music, tab instead of notation
    GBDKV - in audio, the backing track for an instrument

If you own multiple instruments for a song, you can remove all the instruments by taking
the backing track for one of them (preferably drums), and removing all the other instruments
by inverting them and mixing. This leaves a thin layer of noise due to the lossy encoding
but is generally pretty good, and you can easily use something like Audacity's noise filter
to clean it up. The drums backing track is preferred because drums leaves the most audible
noise when you remove it from a different backing track.

    jammittools -a out.wav -y D -n grbkyvx

## Todo

* Mac support (to auto-find Jammit library -- but you can still use `-j` above)
