FROM fpco/stack-build:lts-14.0 as build
RUN mkdir /opt/build
COPY ringofworlds.cabal stack.yaml /opt/build/
RUN cd /opt/build && stack install --only-dependencies --system-ghc
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc

FROM alpine:3.7
RUN apk update && apk add gmp && apk add postgresql-client && apk add libc6-compat
RUN mkdir -p /opt/ringofworlds
WORKDIR /opt/ringofworlds
COPY --from=build /root/.local/bin/ringofworlds /opt/ringofworlds/ringofworlds
CMD ["/opt/ringofworlds/ringofworlds"]
