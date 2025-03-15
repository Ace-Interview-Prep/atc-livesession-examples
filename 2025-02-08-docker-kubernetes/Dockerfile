FROM nixos/nix:2.22.1 AS build
WORKDIR /app
COPY . .
RUN nix-build release.nix

FROM alpine:3.19
COPY --from=build /nix/store /nix/store
COPY --from=build /app/result/bin/my-ml-service /app/my-ml-service

EXPOSE 8080
ENTRYPOINT ["/app/my-ml-service"]
