# # Add just the .cabal file to capture dependencies
# COPY ./libs/engine/engine.cabal /src/libs/engine/engine.cabal
# COPY ./services/api/api.cabal /src/services/api/api.cabal

FROM fpco/stack-build:lts-13.25 as build
WORKDIR /src

# docker cacheables
COPY ./libs/engine/engine.cabal /src/libs/engine/engine.cabal
COPY ./services/api/api.cabal /src/services/api/api.cabal
COPY stack.yaml package-defaults.yaml ./
RUN stack build --haddock --dependencies-only

COPY . .
RUN stack install --system-ghc

FROM debian:9
RUN apt-get update && apt-get install -y build-essential openssl && rm -r /var/lib/apt/lists/*

COPY --from=build /root/.local/bin/api /usr/bin/api

ENTRYPOINT ["/usr/bin/api"]
