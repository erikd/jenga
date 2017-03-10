# Jenga


[![Build Status](https://secure.travis-ci.org/erikd/jenga.svg?branch=master)](http://travis-ci.org/erikd/jenga)

![jenga](doc/jenga.jpg)

A trivial Haskell program that hopefully makes a
[Haskell Stack](https://docs.haskellstack.org/en/stable/README/) project with
poorly defined package dependencies buildable with standard tools like cabal
or [mafia](https://github.com/ambiata/mafia/).


# Using it

For example, I wanted to build
[haskellnews](https://github.com/haskellnews/haskellnews/) but its cabal file
had close to zero dependency bounds. With `jenga` at least I could install the
basic dependencies:

```
git clone https://github.com/haskellnews/haskellnews/
cd haskellnews
jenga -i haskellnews.cabal > cabal.config
# Manually remove the dependency on `base`
cabal sandbox init
cabal install --dependencies-only
```

It wasn't enough to actually build the project but it got me a lot closer.


# How it works

You run it in Haskell project directory that contains both the projects cabal
file and the `stack.yaml` file. It then:

1. Reads the cabal file to extract the dependent library names.
2. Reads the `stack.yaml` file to extract the Stack resolver version.
3. Queries the Stackage server with the resolver version to get a JSON blob
   containing the packages and the versions for that resolver version and
   converts it into a `Map` from package name to package info.
4. For each of the package names read in step 1. it looks up the package name in
   the `Map` from step 3.
5. Prints the package and version info from step 4. to `stdout` in the form of
   a cabal freeze file (should be named `cabal.config` for cabal to recognise
   it).
