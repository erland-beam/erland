# Erland Playground

Erland is a self-hosted playground server for Erlang/Elixir.

Erland allows you to add dependencies, host multiple playgrounds, receive real-time output and guarantees security together with containerization.

## Moving to Rust

We are moving to Rust for our server. Due to file operations and running shell commands, Rust is a better choice for performance. Of course, playground is still focuses on BEAM ecosystem.
