# gcodehs

## Installing

* `git clone https://github.com/distrap/gcodehs/`
* `cd gcodehs`
* `nix-build` or `nix-shell`

## Usage

To pretty-print `sample.gcode`::

```bash
gcodehs pretty sample.gcode
```

See `gcodehs --help` for usage information.

## Development status

Pretty printing is slow due to conversion
to text but we do have colors!

Fast pretty printer is needed that operates
with ByteStrings directly.

## Bash completion

Generating bash completion::

```bash
gcodehs --bash-completion-script `which gcodehs` &> gcodehs-completion.sh
```

or sourcing directly::

```bash
source <(gcodehs --bash-completion-script `which gcodehs`)
```

## Examples

### Generating GCode

* [with monoid](src/Data/GCode/Canon/Generate/Examples.hs)
* [with monad](src/Data/GCode/Canon/Generate/ExamplesMonad.hs)
