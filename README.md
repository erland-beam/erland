<div align="center">

![image](.github/assets/banner.webp)

# Erland

Self-hosted playground server for BEAM languages. Along with containerization, it offers a safe playground for you.

</div>

> ⚠️ You are currently viewing main branch ⚠️

For client implementation, take a look at [erland.ts](https://github.com/erland-beam/erland.ts). We already use it for our UI.

## Languages

We currently support Erlang (26), Elixir (1.14) and Gleam (0.29).

## Installation

> **STRONGLY** recommended to use it with Docker or any other containerisation system.

### Docker

Erland is already published on [GitHub Packages](https://github.com/erland-beam/erland/pkgs/container/erland). You can pull the latest stable release like this:

```bash
docker pull ghcr.io/erland-beam/erland:main
```

And use it:

```bash
docker run -p 8080:8080 ghcr.io/erland-beam/erland:main
```

### Cargo

Erland is already published on [crates.io](https://crates.io/crates/erland). You must have install

```bash
cargo install erland
```

And use it:

```bash
erland --help
```

## Contributing

You can always report bugs and request features via [GitHub Issues](/issues).

For pull requests, make sure your code is well-formatted and at least can explain itself.

## License

Erland server is licensed under the AGPL-3.0 license.
