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

-module(erl_cbor_encoding).

-export([encode/1]).

-if(?OTP_RELEASE < 24).
-type nonempty_binary() :: <<_:8, _:_*8>>.
-endif.

-type encodable() :: integer()
                   | float()
                   | positive_zero
                   | negative_zero
                   | positive_infinity
                   | negative_infinity
                   | nan
                   | boolean()
                   | binary()
                   | list()
                   | map()
                   | byte()
                   | null
                   | undefined
                   | {string, string() | binary()}
                   | {datetime, erl_cbor_time:datetime()}
                   | {datetime, erl_cbor_time:datetime(), integer()}
                   | {timestamp, erl_cbor_time:datetime()}
                   | {erl_cbor:tag(), encodable()}.

-export_type([encodable/0]).

-spec encode(encodable()) -> nonempty_binary() | nonempty_list(<<_:8>> | unicode:chardata()).
encode(Value) when is_integer(Value) ->
  encode_integer(Value);
encode(Value) when is_float(Value) ->
  encode_float(Value);
encode(positive_zero) ->
  <<16#f9, 0, 0, 0, 0, 0, 0, 0, 0>>;
encode(negative_zero) ->
  <<16#f9, 128, 0, 0, 0, 0, 0, 0, 0>>;
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

-spec encode_integer(integer()) -> nonempty_binary()
                                 | nonempty_list(<<_:8>> | nonempty_list(binary())).
encode_integer(I) when I > 16#ffffffffffffffff ->
  [<<16#c2>>, encode_binary(erl_cbor_util:unsigned_integer_bytes(I))];
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
  [<<16#c3>>, encode_binary(erl_cbor_util:unsigned_integer_bytes(-1 - I))].

-spec encode_float(float()) -> <<_:72>>.
encode_float(F) ->
  <<16#fb, F:64/float>>.

-spec encode_boolean(boolean()) -> <<_:8>>.
encode_boolean(false) ->
  <<16#f4>>;
encode_boolean(true) ->
  <<16#f5>>.

-spec encode_binary(binary()) -> unicode:chardata().
encode_binary(Bin) ->
  [erl_cbor_util:encode_sequence_header(2, byte_size(Bin)), Bin].

-spec encode_string(unicode:chardata()) -> nonempty_list(binary()).
encode_string(CharData) ->
  Bin = unicode:characters_to_binary(CharData),
  [erl_cbor_util:encode_sequence_header(3, byte_size(Bin)), Bin].

-spec encode_list([encodable()]) -> nonempty_list(binary()).
encode_list(List) ->
  {Data, Len} = encode_list_data(List, <<>>, 0),
  [erl_cbor_util:encode_sequence_header(4, Len), Data].

-spec encode_list_data([encodable()], <<>> | nonempty_list(binary()), Len) -> {binary(), Len} when
    Len :: non_neg_integer().
encode_list_data([], Data, Len) ->
  {Data, Len};
encode_list_data([Value | Rest], Data, Len) ->
  encode_list_data(Rest, [Data, encode(Value)], Len + 1).

-spec encode_map(map()) -> nonempty_list(binary()).
encode_map(Map) ->
  Len = maps:size(Map),
  Data = maps:fold(fun (K, V, Acc) ->
                       [[iolist_to_binary(encode(K)), encode(V)] | Acc]
                   end, [], Map),
  SortedData = lists:sort(fun ([K1, _], [K2, _]) ->
                              K1 =< K2
                          end, Data),
  [erl_cbor_util:encode_sequence_header(5, Len), SortedData].

-spec encode_datetime(Datetime) -> nonempty_list(nonempty_binary()) when
    Datetime :: calendar:datetime() | integer().
encode_datetime(Datetime) ->
  encode_datetime(Datetime, 0).

-spec encode_datetime(erl_cbor_time:datetime(), integer()) ->
    nonempty_list(nonempty_binary()).
encode_datetime(Datetime, Offset) ->
  {Seconds, _Nanoseconds} = erl_cbor_time:datetime_to_seconds(Datetime),
  OffsetValue = case Offset of
                  0 -> "Z";
                  _ -> Offset
                end,
  String = calendar:system_time_to_rfc3339(Seconds, [{offset, OffsetValue}]),
  encode_tagged_value(0, {string, String}).

-spec encode_timestamp(erl_cbor_time:datetime()) ->
    nonempty_list(nonempty_binary()).
encode_timestamp(Datetime) ->
  case erl_cbor_time:datetime_to_seconds(Datetime) of
    {Seconds, 0} ->
      encode_tagged_value(1, Seconds);
    {Seconds, Nanoseconds} ->
      encode_tagged_value(1, erlang:float(Seconds) + Nanoseconds * 1.0e-9)
  end.

-spec encode_tagged_value(erl_cbor:tag(), encodable()) ->
    nonempty_list(nonempty_binary()).
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
