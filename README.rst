gcodehs
=======

Installing
----------

* `git clone https://github.com/distrap/gcodehs/`
* `cd gcodehs`
* `nix-build` or `nix-shell`

Usage
-----

To pretty-print `sample.gcode`::

  gcodehs pretty sample.gcode

See `gcodehs --help` for usage information.

Development status
------------------

Pretty printing is slow due to conversion
to text but we do have colors!

Fast pretty printer is needed that operates
with ByteStrings directly.

Bash completion
---------------

Generating bash completion::

  gcodehs --bash-completion-script `which gcodehs` &> gcodehs-completion.sh

or sourcing directly::

  source <(gcodehs --bash-completion-script `which gcodehs`)
