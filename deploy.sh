#!/bin/bash

name=$1
set -e

if [ $# -eq 0 ]; then
    name=$(git rev-parse HEAD)
fi

echo "Creating game image..."
pushd server
stack image container
popd
docker save -o .images/game-stormcards game-stormcards

echo "Provisioning server..."
docker-machine create --driver=digitalocean --digitalocean-access-token=$DIGITALOCEAN_ACCESS_TOKEN --digitalocean-region=lon1 --digitalocean-image=ubuntu-14-04-x64 $name
eval "$(docker-machine env $name)"

echo "Copying image to server..."
docker-machine scp .images/game-stormcards $name:game-stormcards
docker load -i .images/game-stormcards

echo "Building compose..."
docker-compose -f docker-compose.prod.yml build

echo "Booting compose..."
docker-compose -f docker-compose.prod.yml up -d

echo "SUCCESS!"
