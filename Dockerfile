```dockerfile
FROM docker.io/elixir:1.14 AS builder

WORKDIR /app

COPY . .

RUN rebar3 get-deps
RUN rebar3 compile
RUN rebar3 as prod release

FROM docker.io/elixir:1.14

RUN mix local.hex --force
RUN mix local.rebar --force

COPY --from=builder /app/_build/prod/rel/erland /release/erland

ENTRYPOINT [ "/release/erland/bin/erland" ]
```