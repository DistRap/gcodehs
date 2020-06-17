# Version [0.1.2.0](https://github.com/distrap/gcodehs/compare/0.1.1.0...0.1.2.0) (2020-06-17)

* Changelog started. Previous release was `0.1.1.0`.

* Major rework, only core functionality and types unchanged
  * Little changes to parser and pretty printer, APIs unchaged

* Additions
  * Canonical representation `Data.GCode.Canon`
  * Interepreters for both `GCode` and canonical representation
  * Line output
  * Tests
  * Helpers and monad for generating GCode
  * Pipes now exposed from library via `Data.GCode.Pipes`
  * `Data.GCode.RS274.Types` module with command decriptions

---

`gcodehs` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

