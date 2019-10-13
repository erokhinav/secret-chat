# secret-chat
A command-line p2p chat with encryption implemented in Haskell.

Before start chatting, a client firstly connects to a server, which only listens to users' connections and shows who is currently online. The chat supports group symmetric ecnryption, the key exchange algorithm is described [here](https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange#Operation_with_more_than_two_parties). Additionally, it supports saving chat info in DB for restoring messages after the client was disconnected.

## Commands

### 1. Server

To start the server run:

```sh
cabal new-run haschat-server -- <server-port>
```

Example:

```sh
cabal new-run haschat-server -- 64242
```

### 2. Client

To start a client run:

```sh
cabal new-run haschat-client -- <user-name> <own-address:own-port> <server-address:server-port>
```

Example:

```sh
cabal new-run haschat-client -- vika 127.0.0.1:5000 127.0.0.1:64242
```

### 3. Tests

To run tests:

```sh
cabal new-run haschat-tests
```
