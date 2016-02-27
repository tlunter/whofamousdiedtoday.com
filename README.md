# whofamousdiedtoday.com

A project that'll find who died today by watching [reddit.com](https://reddit.com)'s new listings

## Backend

The backend will be written in [Haskell](https://www.haskell.org/).

Libraries:

* reddit -> To grab new posts from reddit
* parsec -> To parse titles of reddit posts
* hedis -> To store todays deaths in a database

Tools:

* stack -> To make working with cabal projects easier

### Build

If you have stack, run `stack build`.

If you have cabal, run `cabal build`.  You might have to run `cabal update` first.

## Frontend

The frontend will be written in [Elm](http://elm-lang.org/).
