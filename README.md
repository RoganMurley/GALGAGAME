# Hubris
Hubris is a browser-based multiplayer digital card game written in the purely functional programming languages Haskell and Elm. It is a WIP under active development.

Try it at https://www.playhubris.com.

[![CircleCI](https://circleci.com/gh/RoganMurley/hubris.svg?style=shield)](https://circleci.com/gh/RoganMurley/hubris)


## Dependencies:
* [npm](https://www.npmjs.com/get-npm)
* [gulp](https://www.npmjs.com/package/gulp)
* [Elm](https://guide.elm-lang.org/install.html)
* [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
* [Docker](https://docs.docker.com/engine/installation/)

## Dev setup

### Build server docker image
* `cd server`
* `stack image container`

### Build client code
* `cd client`
* `npm install`
* `elm-package install`
* `gulp build`

### Run locally
* `docker-compose -f docker-compose.dev.yml up`

App will be served at https://localhost:4430.
