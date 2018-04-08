#!/bin/bash

pushd redis
openssl aes-256-cbc -e -in redis.prod.env -out redis.prod.env.secret -k $HUBRIS_SECRET_KEY
popd

pushd client/ssl
openssl aes-256-cbc -e -in www.playhubris.com.ca-bundle -out www.playhubris.com.ca-bundle.secret -k $HUBRIS_SECRET_KEY
openssl aes-256-cbc -e -in www.playhubris.com.key -out www.playhubris.com.key.secret -k $HUBRIS_SECRET_KEY
popd
