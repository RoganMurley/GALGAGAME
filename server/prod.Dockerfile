FROM haskell:9.8.4-bullseye AS build
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

FROM ubuntu:20.04
RUN apt-get update -y && apt-get install postgresql-client -y && apt-get install ca-certificates -y
RUN mkdir -p /opt/galgagame
WORKDIR /opt/galgagame
COPY --from=build /root/.local/bin/galgagame /opt/galgagame/galgagame
CMD ["/opt/galgagame/galgagame"]
