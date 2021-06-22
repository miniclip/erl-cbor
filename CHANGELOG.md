# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `rebar3_hank` [Paulo Oliveira]

### Changed

- CI container approach to `setup-beam` with cache [Paulo Oliveira]

### Fixed

- missing release info in CHANGELOG.md [Guilherme Andrade]

## [2.0.0] - 2021-06-22

### Added

- `setup-beam` -based GitHub Actions CI over Erlang 22-24 [Paulo Oliveira]
- linting via `rebar3_lint` [Paulo Oliveira]
- facilities for easier Hex.pm publishing [Paulo Oliveira]
- a type for decode errors [Guilherme Andrade]
- invocation of EUnit tests under `make test` [Guilherme Andrade]

### Changed

- library name from `cbor` to `erl_cbor` [Guilherme Andrade]

### Removed

- module `cbor` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_base64` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_base64_test` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_base64url` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_base64url_test` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_decoding` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_encoding` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_float` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_test` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_time` from API (see MIGRATION.md) [Guilherme Andrade]
- module `cbor_util` from API (see MIGRATION.md) [Guilherme Andrade]

### Fixed

- broken test case when under OTP 24 [Guilherme Andrade]

## Changes prior to 1.1.0 (including it) are not registered
