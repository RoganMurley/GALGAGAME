FROM fpco/stack-build:lts-14.0 as build
RUN mkdir /opt/build
COPY galgagame.cabal stack.yaml /opt/build/
RUN cd /opt/build && stack install --only-dependencies --system-ghc
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc

FROM ubuntu:20.04
RUN apt-get update && apt-get install postgresql-client
RUN mkdir -p /opt/galgagame
WORKDIR /opt/galgagame
COPY --from=build /root/.local/bin/galgagame /opt/galgagame/galgagame
CMD ["/opt/galgagame/galgagame"]
