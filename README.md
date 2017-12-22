# Hubris
Hurbris is a browser-based multiplayer digital card game written in the purely functional programming languages Haskell and Elm.

Try it at https://www.playhubris.com.

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
* `docker-compose -f docker-compose.dev.yaml up`

App will be served at https://localhost:4430.
You'll need to put a self-signed SSL certificate in `client/ssl/`.


## Deploying
* `DIGITALOCEAN_ACCESS_TOKEN="<my-token>"`
* `./scripts/deploy.sh`
