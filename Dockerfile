FROM docker.io/rust:1.69 AS builder

WORKDIR /app

COPY . .

RUN cargo build --release

FROM ghcr.io/gleam-lang/gleam:v0.29.0-elixir

RUN mix local.hex --force
RUN mix local.rebar --force

COPY --from=builder /app/target/release/erland /release/erland

ENTRYPOINT [ "/release/erland" ]
