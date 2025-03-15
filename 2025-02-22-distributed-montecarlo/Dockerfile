# Dockerfile

FROM haskell:9.6.4

WORKDIR /app

COPY . .
RUN cabal update
RUN cabal install --only-dependencies --allow-newer
RUN cabal build

RUN cp $(cabal list-bin DistributedMonteCarlo) /usr/local/bin/DistributedMonteCarlo

CMD ["/usr/local/bin/DistributedMonteCarlo"]
