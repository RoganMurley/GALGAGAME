# Ring of Worlds

|
<img src="https://i.imgur.com/AHQspU5.gif" height="200"> | <img src="https://i.imgur.com/1ShIxQR.gif" height="200"> |
|----|----|

Ring of Worlds is a browser-based multiplayer digital card game written in the purely functional programming languages Haskell and Elm. It is a WIP under active development.

Try it out at https://www.ringofworlds.com.

Join the [discord](https://discord.gg/SVXXej4).

Read the [devlog](https://forums.tigsource.com/index.php?topic=66122.0).

[![CircleCI](https://circleci.com/gh/RoganMurley/Ring-of-Worlds.svg?style=shield)](https://circleci.com/gh/RoganMurley/Ring-of-Worlds)

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
* `docker-compose up`

App will be served at https://localhost:4430.
