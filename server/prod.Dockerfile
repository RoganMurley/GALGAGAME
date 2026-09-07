FROM haskell:9.8.4-bullseye AS build
# Bullseye is past EOL: deb.debian.org still serves an index listing package
# versions whose .debs have been pruned from the pool, so installs 404 (notably
# python3.9, pulled in by lsb-release). Use the immutable snapshot the base
# image was built against, whose index and pool always agree. These lines ship
# commented out in the image's own sources.list.
#
# Staying on bullseye is deliberate. The runtime stage below is ubuntu:22.04
# (glibc 2.35); building on bookworm (glibc 2.36) would produce a binary that
# builds fine and then fails to start in production.
RUN sed -i \
        -e 's|^deb http://deb.debian.org|# deb http://deb.debian.org|' \
        -e 's|^# deb http://snapshot.debian.org|deb http://snapshot.debian.org|' \
        /etc/apt/sources.list \
    && echo 'Acquire::Check-Valid-Until "false";' > /etc/apt/apt.conf.d/99no-check-valid-until
RUN apt-get update -y \
    && apt-get install -y wget gnupg lsb-release \
    && echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update \
    && apt-get install -y postgresql-client-15 libpq-dev
RUN mkdir /opt/build
COPY galgagame.cabal stack.yaml /opt/build/
RUN cd /opt/build && stack install --only-dependencies --system-ghc
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc

FROM ubuntu:22.04
RUN apt-get update -y && apt-get install postgresql-client -y && apt-get install ca-certificates -y
RUN mkdir -p /opt/galgagame
WORKDIR /opt/galgagame
COPY --from=build /root/.local/bin/galgagame /opt/galgagame/galgagame
CMD ["/opt/galgagame/galgagame"]
