%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(erl_cbor).

-export([encode/1, encode_hex/1,
         decode/1, decode/2, decode_hex/1, decode_hex/2]).

-export_type([tag/0, value/0, simple_value/0, type/0]).

-type tag() :: non_neg_integer().

-type type() :: unsigned_integer
              | neg_integer
              | byte_string
              | utf8_string
              | array
              | map
              | simple
              | float
              | tag().

-type value() :: {type(), integer()
                        | binary()
                        | list()
                        | map()
                        | byte()
                        | boolean()
                        | null
                        | undefined
                        | float()}.

-type simple_value() :: {simple_value, byte()}
                      | false | true | null | undefined.

-spec encode(erl_cbor_encoding:encodable()) -> nonempty_binary().
encode(Data) ->
  erl_cbor_encoding:encode(Data).

-spec encode_hex(erl_cbor_encoding:encodable()) -> unicode:chardata().
encode_hex(Value) ->
  Data = iolist_to_binary(erl_cbor_encoding:encode(Value)),
  erl_cbor_util:binary_to_hex_string(Data).

-spec decode(binary()) -> erl_cbor_decoding:decoding_result(term()).
decode(Data) ->
  decode(Data, erl_cbor_decoding:default_options()).

-spec decode(binary(), erl_cbor_decoding:options()) ->
        erl_cbor_decoding:decoding_result(term()).
decode(Data, Opts) ->
  Decoder = erl_cbor_decoding:decoder(Opts),
  erl_cbor_decoding:decode(Decoder, Data).

-spec decode_hex(binary()) -> erl_cbor_decoding:decoding_result(term()).
decode_hex(Value) ->
  decode_hex(Value, erl_cbor_decoding:default_options()).

-spec decode_hex(binary(), erl_cbor_decoding:options()) ->
        erl_cbor_decoding:decoding_result(term()).
decode_hex(Str, Opts) ->
  Decoder = erl_cbor_decoding:decoder(Opts),
  Bin = erl_cbor_util:hex_string_to_binary(Str),
  case erl_cbor_decoding:decode(Decoder, Bin) of
    {ok, Value, Rest} ->
      {ok, Value, erl_cbor_util:binary_to_hex_string(Rest)};
    {error, Reason} ->
      {error, Reason}
  end.
