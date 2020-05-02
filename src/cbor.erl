%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

%% @doc The cbor module is an implementation of the CBOR data encoding format
%% as defined by RFC 7049.
%%
%% @reference See <a href="https://tools.ietf.org/html/rfc7049">RFC 7049</a>.
-module(cbor).

-export([encode/1, encode_hex/1,
         decode/1, decode/2, decode_hex/1, decode_hex/2]).

-export_type([decoding_options/0, decoding_option/0]).

-type decoding_options() :: [decoding_option()].
%% A set of options affecting CBOR decoding.

% TODO
-type decoding_option() :: term().
%% An option affecting CBOR decoding.
%% <dl>
%% </dl>

-type decoding_result(ValueType) :: {ok, ValueType, iodata()} | {error, term()}.
%% The type of values returned by decoding functions.

%% @doc Encode an Erlang value and return the binary representation of the
%% resulting CBOR data item.
%%
%% Integers, floats, boolean, binaries, lists and maps are encoded to the
%% associated CBOR type.
%%
%% Atoms are used for specific constants:
%% <dl>
%%   <dt>infinity</dt>
%%   <dt>positive_infinity</dt>
%%   <dd>IEEE.754 positive infinity.</dd>
%%
%%   <dt>negative_infinity</dt>
%%   <dd>IEEE.754 negative infinity.</dd>
%%
%%   <dt>nan</dt>
%%   <dd>IEEE.754 NaN.</dd>
%%
%%   <dt>null</dt>
%%   <dd>CBOR null value.</dd>
%%
%%   <dt>undefined</dt>
%%   <dd>CBOR undefined value.</dd>
%% </dl>
%%
%% Tuples are used for more complex CBOR values:
%% <dl>
%%   <dt>`{string, Value}'</dt>
%%   <dd>
%%     `Value' is encoded to a CBOR string; `Value' must be of type
%%     `unicode:chardata/0'.
%%   </dd>
%%   <dt>`{datetime, Value}'</dt>
%%   <dt>`{datetime, Value, UTCOffset}'</dt>
%%   <dd>
%%     `Value' is encoded to a CBOR text string tagged as a standard datetime
%%     string. `Value' must be of type `cbor_time:datetime/0'. If present,
%%     `UTCOffset' is used to transform the universal date represented by
%%     `Value' into a local date whose timezone is separated from Universal
%%     Coordinated Time (UTC) by `UTCOffset' seconds.
%%   </dd>
%%   <dt>`{timestamp, Value}'</dt>
%%   <dd>
%%     `Value' is encoded to a CBOR integer or floating point number tagged as
%%     an epoch-based datetime. `Value' must be of type
%%     `cbor_time:datetime/0'.  of type `erlang:timestamp()'.
%%   </dd>
%%   <dt>`{Tag, Value}'</dt>
%%   <dd>
%%     `Value' is encoded to a tagged CBOR value. `Tag' must be a positive
%%     integer.
%%   </dd>
%% </dl>
-spec encode(term()) -> iodata().
encode(Value) when is_integer(Value) ->
  encode_integer(Value);
encode(Value) when is_float(Value) ->
  encode_float(Value);
encode(infinity) ->
  <<16#f9, 16#7c, 16#00>>;
encode(positive_infinity) ->
  <<16#f9, 16#7c, 16#00>>;
encode(negative_infinity) ->
  <<16#f9, 16#fc, 16#00>>;
encode(nan) ->
  <<16#f9, 16#7e, 16#00>>;
encode(Value) when is_boolean(Value) ->
  encode_boolean(Value);
encode(Value) when is_binary(Value) ->
  encode_binary(Value);
encode(Value) when is_list(Value) ->
  encode_list(Value);
encode(Value) when is_map(Value) ->
  encode_map(Value);
encode(null) ->
  <<16#f6>>;
encode(undefined) ->
  <<16#f7>>;
encode({string, Value}) ->
  encode_string(Value);
encode({datetime, Value}) ->
  encode_datetime(Value);
encode({datetime, Value, Offset}) ->
  encode_datetime(Value, Offset);
encode({timestamp, Value}) ->
  encode_timestamp(Value);
encode({Tag, Value}) when is_integer(Tag) ->
  encode_tagged_value(Tag, Value);
encode(Value) ->
  error({unencodable_value, Value}).

%% @doc Encode an Erlang value and return the representation of the resulting
%% CBOR data item as an hex-encoded string.
%%
%% @see encode/1
-spec encode_hex(term()) -> unicode:chardata().
encode_hex(Value) ->
  Data = iolist_to_binary(encode(Value)),
  cbor_util:binary_to_hex_string(Data).

%% @doc Encode an integer to a signed or unsigned CBOR integer.
-spec encode_integer(integer()) -> iodata().
encode_integer(I) when I > 16#ffffffffffffffff ->
  [<<16#c2>>, encode_binary(cbor_util:unsigned_integer_bytes(I))];
encode_integer(I) when I > 16#ffffffff ->
  <<16#1b, I:64>>;
encode_integer(I) when I > 16#ffff ->
  <<16#1a, I:32>>;
encode_integer(I) when I > 16#ff ->
  <<16#19, I:16>>;
encode_integer(I) when I > 16#17 ->
  <<16#18, I:8>>;
encode_integer(I) when I >= 16#00 ->
  <<I:8>>;
encode_integer(I) when I >= -16#18 ->
  <<(16#20 - 1 - I):8>>;
encode_integer(I) when I >= -16#ff - 1 ->
  <<16#38, (-1 - I):8>>;
encode_integer(I) when I >= -16#ffff - 1 ->
  <<16#39, (-1 - I):16>>;
encode_integer(I) when I >= -16#ffffffff - 1 ->
  <<16#3a, (-1 - I):32>>;
encode_integer(I) when I >= -16#ffffffffffffffff - 1 ->
  <<16#3b, (-1 - I):64>>;
encode_integer(I) ->
  [<<16#c3>>, encode_binary(cbor_util:unsigned_integer_bytes(-1 - I))].

%% @doc Encode a float to a CBOR floating point number.
%%
%% We currently do not support encoding to 16 bit or 32 bit CBOR floating
%% point numbers.
-spec encode_float(float()) -> iodata().
encode_float(F) ->
  <<16#fb, F:64/float>>.

%% @doc Encode a boolean to a CBOR boolean.
-spec encode_boolean(boolean()) -> iodata().
encode_boolean(false) ->
  <<16#f4>>;
encode_boolean(true) ->
  <<16#f5>>.

%% @doc Encode binary data to a CBOR byte string.
-spec encode_binary(binary()) -> iodata().
encode_binary(Bin) ->
  [cbor_util:encode_sequence_header(2, byte_size(Bin)), Bin].

%% @doc Encode a binary string to a CBOR text string.
-spec encode_string(unicode:chardata()) -> iodata().
encode_string(CharData) ->
  Bin = unicode:characters_to_binary(CharData),
  [cbor_util:encode_sequence_header(3, byte_size(Bin)), Bin].

%% @doc Encode a list to a CBOR array.
-spec encode_list(list()) -> iodata().
encode_list(List) ->
  {Data, Len} = encode_list_data(List, <<>>, 0),
  [cbor_util:encode_sequence_header(4, Len), Data].

-spec encode_list_data(list(), iodata(), Len) -> {iodata(), Len} when
    Len :: non_neg_integer().
encode_list_data([], Data, Len) ->
  {Data, Len};
encode_list_data([Value | Rest], Data, Len) ->
  encode_list_data(Rest, [Data, encode(Value)], Len + 1).

%% @doc Encode a map to a CBOR map.
%%
%% Note that we have to flatten key data to sort pairs by lexical byte order,
%% in order to follow the rules for canonical encoding.
-spec encode_map(map()) -> iodata().
encode_map(Map) ->
  Len = maps:size(Map),
  Data = maps:fold(fun (K, V, Acc) ->
                       [[iolist_to_binary(encode(K)), encode(V)] | Acc]
                   end, [], Map),
  SortedData = lists:sort(fun ([K1, _], [K2, _]) ->
                              K1 =< K2
                          end, Data),
  [cbor_util:encode_sequence_header(5, Len), SortedData].

%% @doc Encode a datetime value to a CBOR tagged string.
-spec encode_datetime(Datetime) -> iodata() when
    Datetime :: calendar:datetime() | integer().
encode_datetime(Datetime) ->
  encode_datetime(Datetime, 0).

%% @doc Encode a datetime value to a CBOR tagged string with a specific
%% timezone offset.
-spec encode_datetime(cbor_time:datetime(), integer()) -> iodata().
encode_datetime(Datetime, Offset) ->
  {Seconds, _Nanoseconds} = cbor_time:datetime_to_seconds(Datetime),
  OffsetValue = case Offset of
                  0 -> "Z";
                  _ -> Offset
                end,
  String = calendar:system_time_to_rfc3339(Seconds, [{offset, OffsetValue}]),
  encode_tagged_value(0, {string, String}).

%% @doc Encode a datetime value to a CBOR integer or floating point number
%% representing an epoch-based date. An integer is used when the datetime
%% value does not have fractional seconds.
-spec encode_timestamp(cbor_time:datetime()) -> iodata().
encode_timestamp(Datetime) ->
  case cbor_time:datetime_to_seconds(Datetime) of
    {Seconds, 0} ->
      encode_tagged_value(1, Seconds);
    {Seconds, Nanoseconds} ->
      encode_tagged_value(1, erlang:float(Seconds) + Nanoseconds * 1.0e-9)
  end.

%% @doc Encode a value preceded by a semantic tag.
-spec encode_tagged_value(non_neg_integer(), term()) -> iodata().
encode_tagged_value(Tag, Value) when Tag =< 16#17 ->
  [<<6:3, Tag:5>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ff ->
  [<<6:3, 24:5, Tag:8>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ffff ->
  [<<6:3, 25:5, Tag:16>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ffffffff ->
  [<<6:3, 26:5, Tag:32>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ffffffffffffffff ->
  [<<6:3, 27:5, Tag:64>>, encode(Value)];
encode_tagged_value(Tag, _Value) ->
  error({unencodable_tag, Tag}).

%% @doc Decode a CBOR data item from binary data and return both the Erlang
%% value it represents and the rest of the binary data which were not decoded.
%%
%% @see decode/2
-spec decode(iodata()) -> decoding_result(term()).
decode(Data) ->
  decode(Data, []).

%% @doc Decode a CBOR data item from binary data and return both the Erlang
%% value it represents and the rest of the binary data which were not decoded.
-spec decode(iodata(), decoding_options()) ->
        {ok, term(), iodata()} | {error, term()}.
decode(<<Tag:8, Data/binary>>, Opts) ->
  decode1(Tag, Data, Opts).

-spec decode1(Tag, iodata(), decoding_options()) -> decoding_result(term()) when
    Tag :: byte().
decode1(Tag, Data, _Opts) when Tag =< 16#17 ->
  {ok, Tag, Data};
decode1(Tag, Data, _Opts) when Tag >= 16#18 andalso Tag =< 16#1b ->
  decode_unsigned_integer(Tag, Data);
decode1(Tag, Data, _Opts) when Tag >= 16#20 andalso Tag =< 16#37 ->
  {ok, -1 - (Tag - 16#20), Data};
decode1(Tag, Data, _Opts) when Tag >= 16#38 andalso Tag =< 16#3b ->
  decode_negative_integer(Tag, Data);
decode1(Tag, Data, _Opts) when Tag >= 16#40 andalso Tag =< 16#5b ->
  decode_byte_string(Tag, Data);
decode1(16#5f, _Data, _Opts) ->
  %% TODO undefinite length byte strings
  {error, unsupported_undefinite_length_byte_string};
decode1(Tag, Data, _Opts) when Tag >= 16#60 andalso Tag =< 16#7b ->
  decode_utf8_string(Tag, Data);
decode1(16#7f, _Data, _Opts) ->
  %% TODO undefinite length utf-8 strings
  {error, unsupported_undefinite_length_utf8_string};
decode1(Tag, Data, _Opts) when Tag >= 16#80 andalso Tag =< 16#9b ->
  decode_array(Tag, Data);
decode1(16#9f, _Data, _Opts) ->
  %% TODO undefinite length arrays
  {error, unsupported_undefinite_length_array};
decode1(Tag, Data, _Opts) when Tag >= 16#a0 andalso Tag =< 16#bb ->
  %% TODO undefinite length maps
  decode_map(Tag, Data);
decode1(16#bf, _Data, _Opts) ->
  {error, unsupported_undefinite_length_map};
% TODO c0-d4 tagged items
% TODO d5-d7 "expected conversion"
% TODO d8-db extended tagged items
% TODO e0-f3 simple values
decode1(16#f4, Data, _Opts) ->
  {ok, false, Data};
decode1(16#f5, Data, _Opts) ->
  {ok, true, Data};
decode1(16#f6, Data, _Opts) ->
  {ok, null, Data};
decode1(16#f7, Data, _Opts) ->
  {ok, undefined, Data};
% TODO f8 simple value
% TODO f9-fb floating point numbers
decode1(Tag, _Data, _Opts) ->
  {error, {invalid_tag, Tag}}.

%% @doc Decode a CBOR data item from an hex-encoded string and return both the
%% Erlang value it represents and the rest of the string which was not
%% decoded.
%%
%% @see decode/1
-spec decode_hex(string()) -> decoding_result(term()).
decode_hex(Value) ->
  decode_hex(Value, []).

%% @doc Decode a CBOR data item from an hex-encoded string and return both the
%% Erlang value it represents and the rest of the string which was not
%% decoded.
%%
%% @see decode/2
-spec decode_hex(string(), decoding_options()) -> decoding_result(term()).
decode_hex(Str, Opts) ->
  Bin = cbor_util:hex_string_to_binary(Str),
  case decode(Bin, Opts) of
    {ok, Value, Rest} ->
      {ok, Value, cbor_util:binary_to_hex_string(Rest)};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a CBOR unsigned integer.
-spec decode_unsigned_integer(Tag, iodata()) ->
        decoding_result(non_neg_integer()) when
    Tag :: 16#18..16#1b.
decode_unsigned_integer(16#18, <<I:8, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#19, <<I:16, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#1a, <<I:32, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#1b, <<I:64, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(_Tag, _Data) ->
  {error, truncated_unsigned_integer}.

%% @doc Decode a CBOR negative integer.
-spec decode_negative_integer(Tag, iodata()) ->
        decoding_result(neg_integer()) when
    Tag :: 16#38..16#3b.
decode_negative_integer(16#38, <<I:8, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#39, <<I:16, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#3a, <<I:32, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#3b, <<I:64, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(_Tag, _Data) ->
  {error, truncated_negative_integer}.

%% @doc Decode a CBOR binary string to an Erlang binary.
-spec decode_byte_string(Tag, iodata()) -> decoding_result(binary()) when
    Tag :: 16#40..16#5b.
decode_byte_string(Tag, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Tag, Data),
  case Data2 of
    <<Bin:Len/binary, Rest/binary>> ->
      {ok, iolist_to_binary(Bin), Rest};
    _ ->
      {error, truncated_byte_string}
  end.

%% @doc Decode a CBOR UTF-8 string to an Erlang binary. Invalid or incomplete
%% UTF-8 sequence cause a decoding error.
-spec decode_utf8_string(Tag, iodata()) -> decoding_result(binary()) when
    Tag :: 16#60..16#7b.
decode_utf8_string(Tag, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Tag, Data),
  case Data2 of
    <<Bin:Len/binary, Rest/binary>> ->
      Str = case unicode:characters_to_binary(Bin) of
              Bin2 when is_binary(Bin2) ->
                Bin2;
              {error, _, _} ->
                {error, {invalid_utf8_string, Bin}};
              {incomplete, _, _} ->
                {error, {incomplete_utf8_string, Bin}}
            end,
      {ok, Str, Rest};
    _ ->
      {error, truncated_utf8_string}
  end.

%% @doc Decode a CBOR array to an Erlang list.
-spec decode_array(Tag, iodata()) -> decoding_result(list()) when
    Tag :: 16#80..16#9b.
decode_array(Tag, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Tag, Data),
  case decode_sequence(Data2, Len, []) of
    {ok, Values, Rest} ->
      {ok, Values, Rest};
    {error, truncated_sequence} ->
      {error, truncated_array};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a CBOR map to an Erlang map.
-spec decode_map(Tag, iodata()) -> decoding_result(map()) when
    Tag :: 16#a0..16#bb.
decode_map(Tag, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Tag, Data),
  case decode_sequence(Data2, Len*2, []) of
    {ok, Values, Rest} ->
      {ok, cbor_util:list_to_map(Values), Rest};
    {error, truncated_sequence} ->
      {error, truncated_map};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a fixed number of consecutive CBOR data items and return them
%% as a list.
-spec decode_sequence(iodata(), non_neg_integer(), list()) -> decoding_result(list()).
decode_sequence(Data, 0, Acc) ->
  {ok, lists:reverse(Acc), Data};
decode_sequence(<<>>, _N, _Acc) ->
  {error, truncated_sequence};
decode_sequence(Data, N, Acc) ->
  case decode(Data) of
    {ok, Value, Rest} ->
      decode_sequence(Rest, N-1, [Value | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.
