GCodeHS
=======

Installing
----------

* Install stack https://docs.haskellstack.org/en/stable/README/
* git clone https://github.com/hackerspace/gcodehs/
* cd gcodehs
* stack build
* stack install

Usage
-----

To pretty-print `sample.gcode`::

  gcodehs -p -i sample.gcode

To parse to JSON::

  gcodehs -j -i sample.gcode

See `gcodehs --help` for usage information.

Examples
--------

Filtering and Manipulation::

  stack runghc examples/Main.hs sample.gcode

Simple parse and print::

  stack runghc examples/Simple.hs sample.gcode

Pretty printing::

  stack runghc examples/Pretty.hs sample.gcode

JSON output::

  stack runghc examples/JSON.hs sample.gcode

Naive non-streaming example::

  stack runghc examples/Naive.hs sample.gcode


Development status
------------------

Parser won't handle modal GCode such as::

  G0 X10 Y20
  X0 Y10


Pretty printing is slow due to conversion
to text but we do have colors!

Fast pretty printer is needed that operates
with ByteStrings directly.

JSON support is only experimental, output is pretty ugly and
fromJSON doesn't work, probably bad fromJSON instance for Axes/Params mapdoesn't work, probably bad fromJSON instance for Axes/Params maps.

Bash completion
---------------

Generating bash completion::

  gcodehs --bash-completion-script `which gcodehs` &> gcodehs-completion.sh

or sourcing directly::

  source <(gcodehs --bash-completion-script `which gcodehs`)

