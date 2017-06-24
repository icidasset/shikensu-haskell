# Changelog

### v0.3.7

- Windows support

### v0.3.6

- Use Stackage Nightly

### v0.3.5

- Fix package.yaml file (nothing was exported)

### v0.3.4

- Some light refactoring
- Types are now in an internal module and __re-exported in the main Shikensu module__

### v0.3.3

- Rename `(⚡)` and `(⚡⚡)` to `(~>)` and `(!~>)` respectively

### v0.3.2

- Fix code in comment 🤦‍♂️

### v0.3.1

- Adjust `base` version range to fix cabal compile failures on older versions of GHC (pre 7.10)

### v0.3.0

- Added `listRelative` and it's flipped relative
- Updated dependencies

### v0.2.1

- Fix order of the `list` function parameters

### v0.2.0

- Split up `Utilities` to internal and non-internal
- Add more `Utilities` (sequence utils, metadata helpers, etc.)
- Clean up code

### v0.1.3

- Fix `Contrib.prefixDirname` which didn't change `pathToRoot` and `parentPath` like it should

### v0.1.2 - v0.1.1

- Fix code samples in docs

### v0.1.0

- Initial release 🎉
