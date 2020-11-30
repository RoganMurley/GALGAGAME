# GALGA
<img width="512" alt="galga" src="https://user-images.githubusercontent.com/3668870/100647436-696e8280-3337-11eb-80ca-0c3a263a0d0d.png">


GALGA is a browser-based multiplayer digital card game written in the purely functional programming languages Haskell and Elm. It is a WIP under active development.

Try it out at https://www.galgagame.com.

Join the [discord](https://discord.gg/SVXXej4).

[![CircleCI](https://circleci.com/gh/RoganMurley/GALGAGAME.svg?style=shield)](https://circleci.com/gh/RoganMurley/GALGAGAME)

## Dependencies:
* [npm](https://www.npmjs.com/get-npm)
* [gulp](https://www.npmjs.com/package/gulp)
* [Elm](https://guide.elm-lang.org/install.html)
* [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
* [Docker](https://docs.docker.com/engine/installation/)

## Dev setup

### Build client
* `cd client`
* `npm install`
* `elm-package install`
* `gulp build`

## Build server
* `./scripts/makeFakeCertificates`
* `docker-compose build`

## Run locally
* `docker-compose up`

App will be served at https://localhost:4430.
