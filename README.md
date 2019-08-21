# Ring of Worlds

|
<img src="https://i.imgur.com/z9Jo00u.gif" height="200"> | <img src="https://i.imgur.com/PgOAsJc.gif" height="200"> |
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

### Build client
* `cd client`
* `npm install`
* `elm-package install`
* `gulp build`

### Build server and run
* `docker-compose build`
* `docker-compose up`

App will be served at https://localhost:4430.
