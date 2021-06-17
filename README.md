# erl_cbor [![GitHub Actions CI][ci-img]][ci]

[ci]: https://github.com/miniclip/erl_cbor
[ci-img]: https://github.com/miniclip/erl_cbor/workflows/build/badge.svg

This repository contains an Erlang library implementing the CBOR data encoding
format defined in [RFC 7049](https://tools.ietf.org/html/rfc7049).

In the current state, the library supports:

- Encoding of various kinds of Erlang values, with special constructions for
  tagged values.
- Decoding of all CBOR value types, including indefinite length sequences.
- Interpretation of various tagged data to suitable Erlang values.
- A way to customize how tagged CBOR values are interpreted to Erlang values.
- A configurable depth limit to protect against extremely deep data structures.

## Versioning, changelog, migration guide and README

This project adheres to [Semantic
Versioning](https://semver.org/spec/v2.0.0.html).

The format of `CHANGELOG.md` is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

Breaking changes are documented under `MIGRATION.md`.

The format of `README.md` is based on [Make a
README](https://www.makeareadme.com/).

## Documentation

A handbook is available [in the `doc`
directory](https://github.com/miniclip/erl_cbor/blob/master/doc/handbook.md).
