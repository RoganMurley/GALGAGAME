#!/bin/bash

pushd redis
openssl aes-256-cbc -d -in redis.prod.env.secret -out redis.prod.env -k $HUBRIS_SECRET_KEY
popd

pushd client/ssl
openssl aes-256-cbc -d -in www.playhubris.com.ca-bundle.secret -out www.playhubris.com.ca-bundle -k $HUBRIS_SECRET_KEY
openssl aes-256-cbc -d -in www.playhubris.com.key.secret -out www.playhubris.com.key -k $HUBRIS_SECRET_KEY
popd
