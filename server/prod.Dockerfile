FROM fpco/stack-build:lts-14.0 as build
RUN mkdir /opt/build
COPY galgagame.cabal stack.yaml /opt/build/
RUN cd /opt/build && stack install --only-dependencies --system-ghc
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc

FROM alpine:3.7
RUN apk update && apk add gmp && apk add postgresql-client && apk add libc6-compat && apk add libgcc
RUN mkdir -p /opt/galgagame
WORKDIR /opt/galgagame
COPY --from=build /root/.local/bin/galgagame /opt/galgagame/galgagame
CMD ["/opt/galgagame/galgagame"]
