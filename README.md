libget
======

An extremely high-level package manager for just about anything


Usage
-----

```bash
$ libget --help
```

See `sample.json` for a sample dependency specification for your project.

Currently, `libget` only supports copying files from a base package path where
each dependency has a directory with its name and subdirectories for each version.

For example, if you have a dependency specification in `spec.json`:

```json
{
  "version": "1",
  "dependencies": {
    "lib/openssl": { "name": "openssl", "version": "1.0" }
  }
}
```

then you must have a directory somewhere like this `/packages/openssl/1.0`.

Then you can install from this package source like this

```bash
cat spec.json | libget -p "/packages"
```


Building
--------

You must have

  * [Glasgow Haskell Compiler](http://www.haskell.org/ghc/)
  * [Cabal](http://www.haskell.org/cabal/) (which comes with the [Haskell Platform](http://www.haskell.org/platform/))

Then you can

```bash
$ cd libget
$ cabal install
```


License
-------

Copyright &copy; Covenant Eyes 2014

This package is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php)
(see `LICENSE`).
