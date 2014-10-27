DocsDuck
=========

An ugly lil' Haskell program to deal with the even uglier DocsDB.

Installation
------------

It should just install with cabal. You will probably want to install
the [Haskell Platform](https://www.haskell.org/platform/).

    cabal install --bindir=/path/to/where/you/want/the/bin

If you have [nix](http://nixos.org/nix/) you can use `nix-shell` to
run the cabal install, and it will make sure any extra dependencies
have been installed.

Usage
-----

Download a grades sheet containing one set of marks from Moodle in the plain text CSV format, and then run DocsDuck as follows:

    DocsDuck ccid docsdb-pass course-number grades.csv "docsdbassignmentname"

DocsDuck will then upload all of the marks to DocsDB and spit out the horrible HTML response so you can see if something went wrong. Make sure that the assignments are out of 100 marks on DocsDB.

Issues
------

This currently does not work for updating marks because I can't be bothered!
