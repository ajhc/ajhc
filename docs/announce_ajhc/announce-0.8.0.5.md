# Ajhc 0.8.0.5 Release Notes

We are happy to announce Ajhc 0.8.0.5.
Major change on this release is supporting haddock.
You can see Ajhc's internal using web browser.

Ajhc's project web site is found at http://ajhc.metasepi.org/.
You can get the source code from https://github.com/ajhc/ajhc/tags.

## Changes

* Support haddock. You can read haddock on http://hackage.haskell.org/package/ajhc, hopefully.
* Split library from Ajhc program.
* Better NAIVEGC.
* Add arity "arena" to all functions compiled by Ajhc. For supporting reentrant in future.
* Run SelfTest and rtstest on travis-ci.
* Add jahm command to download file from Hackage DB, because sometime perl script occurs error.

- - -
Metasepi team
