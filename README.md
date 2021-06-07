# erl-cbor [![GitHub Actions CI][ci-img]][ci]

[ci]: https://github.com/miniclip/erl-cbor
[ci-img]: https://github.com/miniclip/erl-cbor/workflows/build/badge.svg

This repository contains an Erlang library implementing the CBOR data encoding
format defined in [RFC 7049](https://tools.ietf.org/html/rfc7049).

In the current state, the library supports:

- Encoding of various kinds of Erlang values, with special constructions for
  tagged values.
- Decoding of all CBOR value types, including indefinite length sequences.
- Interpretation of various tagged data to suitable Erlang values.
- A way to customize how tagged CBOR values are interpreted to Erlang values.
- A configurable depth limit to protect against extremely deep data structures.

## Documentation

A handbook is available [in the `doc`
directory](https://github.com/miniclip/erl-cbor/blob/master/doc/handbook.md).
