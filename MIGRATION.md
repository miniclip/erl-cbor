# Migration guide

Whenever there's an interface breaking change (a change in the project's major version),
required migration instructions will be detailed in this file.

The format is based on
[Make a Migration](https://confluence.cf.miniclip.com/display/TM/Make+a+MIGRATION),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## From [1.x] to [2.x]

### Update

- any call to or type export from module `cbor` to `erl_cbor` instead
- any call to or type export from module `cbor_base64` to `erl_cbor_base64` instead
- any call to or type export from module `cbor_base64_test` to `erl_cbor_base64_test` instead
- any call to or type export from module `cbor_base64url` to `erl_cbor_base64url` instead
- any call to or type export from module `cbor_base64url_test` to `erl_cbor_base64url_test` instead
- any call to or type export from module `cbor_decoding` to `erl_cbor_decoding` instead
- any call to or type export from module `cbor_encoding` to `erl_cbor_encoding` instead
- any call to or type export from module `cbor_float` to `erl_cbor_float` instead
- any call to or type export from module `cbor_test` to `erl_cbor_test` instead
- any call to or type export from module `cbor_time` to `erl_cbor_time` instead
- any call to or type export from module `cbor_util` to `erl_cbor_util` instead
