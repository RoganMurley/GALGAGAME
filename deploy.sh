#!/bin/bash

name=$1
set -e

if [ $# -eq 0 ]; then
    echo "Server name required"
    exit 1
fi

echo "Creating game image..."
pushd server
stack image container
popd
docker save -o .images/game-stormcards game-stormcards

echo "Provisioning server..."
docker-machine create --driver=digitalocean --digitalocean-access-token=$DIGITALOCEAN_ACCESS_TOKEN --digitalocean-region=lon1 $name
eval "$(docker-machine env $name)"

echo "Copying image to server..."
docker-machine scp .images/game-stormcards $name:game-stormcards
docker load -i .images/game-stormcards

echo "Booting images..."
docker-compose up -d

echo "SUCCESS!"
