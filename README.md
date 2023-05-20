# Erland

Self-hosted playground server for BEAM languages. Along with containerization, it offers a safe playground for you.

For client implementation, take a look at [erland.ts](https://github.com/erland-beam/erland.ts). We already use it for our UI.

## Installation

You can also build the project by yourself and use it on your machine. But we **STRONGY** recommend to use it with Docker.

Erland is already published on [Docker Hub](https://hub.docker.com/r/meppu/erland). You can pull the latest stable release like this:

```bash
# docker pull meppu/erland:<tag> (tag can be "latest" (stable latest [default]), "main" (unstable latest), or a version ("v1.0.0" etc...))
docker pull meppu/erland
```

And use it:

```bash
# docker run -p <port to expose>:8080 meppu/erland:<tag>
docker run -p 1337:8080 meppu/erland
```

Now you are able to connect WebSocket server.

## Contributing

You can always report bugs and request features via [GitHub Issues](/issues).

For pull requests, make sure your code is well-formatted and at least can explain itself.

## License

Erland server is licensed under the AGPL-3.0 license.
