FROM docker.io/rust:1.69 AS builder

WORKDIR /app

COPY . .

RUN cargo build --release

FROM docker.io/elixir:1.14

RUN mix local.hex --force
RUN mix local.rebar --force

COPY --from=builder /app/target/release/erland /release/erland

EXPOSE 8080
CMD [ "/release/erland" ]