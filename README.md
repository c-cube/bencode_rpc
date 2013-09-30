## Bencode-RPC

Remote Procedure Call in OCaml, with B-encode and Lwt.

This library allows to communicate through the network (TCP) using
a remote call abstraction. The data is serialized with Bencode.

Documentation is available [here](http://cedeela.fr/~simon/software/bencode_rpc/).

### Install

You need OCaml>=3.12, and the libraries `lwt` and `lwt.unix`.

    $ make
    $ make install

### License

Released under the BSD2 license. See `LICENSE`.
