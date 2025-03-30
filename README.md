# GALGA
![GALGA gif](https://user-images.githubusercontent.com/3668870/145656414-8a6c26a0-3fa2-4e22-955f-29719a02e04a.gif)

GALGA is a browser-based multiplayer digital card game written in the purely functional programming languages Haskell and Elm. It is a WIP under active development.

Try it out at https://www.galgagame.com.

Watch the trailer on [YouTube](https://youtu.be/W_OH2loa1nc).

Join the [discord](https://discord.gg/SVXXej4).

[![CircleCI](https://circleci.com/gh/RoganMurley/GALGAGAME.svg?style=shield)](https://circleci.com/gh/RoganMurley/GALGAGAME)


![GALGA gif](https://user-images.githubusercontent.com/3668870/145655826-3e02cb61-300a-42da-8454-aa8d0d3b6ba5.gif)


## Dependencies:
* [nvm](https://github.com/nvm-sh/nvm)
* [gulp](https://www.npmjs.com/package/gulp)
* [Elm](https://guide.elm-lang.org/install/elm.html)
* [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
* [Docker](https://docs.docker.com/engine/installation/)

![Yet another GALGA gif](https://user-images.githubusercontent.com/3668870/145656874-6f0223d8-d918-4043-9001-e3a3844c65fc.gif)

## Dev setup

### Build client
* `cd client`
* `nvm use`
* `npm install`
* `elm make`
* `npm install -g gulp`
* `gulp build`

## Build Images
* `./scripts/makeFakeCertificates`
* `docker compose build`

## Run locally
* `docker compose up`
* `ssh dev`
* `cd opt/build`
* `stack repl`
* `main`

App will be served at https://localhost:4430.
