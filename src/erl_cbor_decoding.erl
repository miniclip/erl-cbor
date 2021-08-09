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

-module(erl_cbor_decoding).

-export([default_options/0, default_value_interpreters/0,
         decoder/1, decode/2]).

-if(?OTP_RELEASE < 24).
-type nonempty_binary() :: <<_:8, _:_*8>>.
-endif.

-export_type([decoder/0, options/0, value_interpreter/0,
             decoding_result/1, decoding_error/0,
             invalid_input_error/0, truncated_input_error/0,
             interpretation_result/1, interpretation_error/0]).

-record(decoder, {options = #{} :: options(),
                  depth = 0 :: non_neg_integer()}).
-type decoder() :: #decoder{}.

-type options() :: #{max_depth => non_neg_integer(),
                     value_interpreters =>
                      #{erl_cbor:type() := value_interpreter()}}.

-type value_interpreter() :: fun((decoder(), erl_cbor:value()) -> interpretation_result(term()))
                           | fun((erl_cbor:value()) -> interpretation_result(term())).

-type decoding_result(Decodable) :: {ok, Decodable, binary()} | {error, decoding_error()}.

-type decoding_error() :: invalid_input_error() |
                          truncated_input_error() |
                          interpretation_error().

-type invalid_input_error() :: invalid_sequence_header |
                               no_input |
                               odd_number_of_map_values |
                               {invalid_base64_data, {invalid_data, binary()}
                                                   | {invalid_base64_digit, byte()}} |
                               {invalid_base64url_data, badarith} |
                               {invalid_cbor_data, term()} |
                               {invalid_tagged_value, tuple()} |
                               {invalid_trailing_data, nonempty_binary()} |
                               {invalid_type_tag, byte()}.

-type truncated_input_error() :: truncated_array |
                                 truncated_byte_string |
                                 truncated_float |
                                 truncated_map |
                                 truncated_negative_integer |
                                 truncated_sequence_header |
                                 truncated_simple_value |
                                 truncated_tagged_value |
                                 truncated_unsigned_integer |
                                 truncated_utf8_string.

-type interpretation_result(ValueType) :: {ok, ValueType} | {error, interpretation_error()}.

-type interpretation_error() :: term().

-spec default_options() -> options().
default_options() ->
  #{max_depth => 1024,
    value_interpreters => default_value_interpreters()}.

-spec default_value_interpreters() ->
        #{erl_cbor:tag() := value_interpreter()}.
default_value_interpreters() ->
  #{0 => fun interpret_utf8_string/1,
    1 => fun interpret_epoch_based_datetime/1,
    2 => fun interpret_positive_bignum/1,
    3 => fun interpret_negative_bignum/1,
    24 => fun interpret_cbor_value/2,
    32 => fun interpret_utf8_string/1,
    33 => fun interpret_base64url_data/1,
    34 => fun interpret_base64_data/1,
    35 => fun interpret_utf8_string/1,
    36 => fun interpret_utf8_string/1,
    55799 => fun interpret_self_described_cbor_value/1}.

-spec decoder(options()) -> decoder().
decoder(Opts) ->
  #decoder{options = Opts}.

-spec decode(decoder(), binary()) -> decoding_result(term()).
decode(#decoder{depth = Depth, options = #{max_depth := MaxDepth}}, _Data) when
    Depth > MaxDepth ->
  {error, max_depth_reached};
decode(Decoder, <<T:8, Data/binary>>) when T =< 16#17 ->
  maybe_interpret_value(Decoder, {unsigned_integer, {ok, T, Data}});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#18, T =< 16#1b ->
  DecodedValue = decode_unsigned_integer(T, Data),
  maybe_interpret_value(Decoder, {unsigned_integer, DecodedValue});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#20, T =< 16#37 ->
  maybe_interpret_value(Decoder, {neg_integer, {ok, -1 - (T - 16#20), Data}});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#38, T =< 16#3b ->
  DecodedValue = decode_negative_integer(T, Data),
  maybe_interpret_value(Decoder, {neg_integer, DecodedValue});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#40, T =< 16#5b ->
  DecodedValue = decode_byte_string(T, Data),
  maybe_interpret_value(Decoder, {byte_string, DecodedValue});
decode(Decoder, <<16#5f:8, Data/binary>>) ->
  DecodeValue = decode_indefinite_length_byte_string(Data),
  maybe_interpret_value(Decoder, {byte_string, DecodeValue});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#60, T =< 16#7b ->
  DecodedValue = decode_utf8_string(T, Data),
  maybe_interpret_value(Decoder, {utf8_string, DecodedValue});
decode(Decoder, <<16#7f:8, Data/binary>>) ->
  DecodedValue = decode_indefinite_length_utf8_string(Data),
  maybe_interpret_value(Decoder, {utf8_string, DecodedValue});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#80, T =< 16#9b ->
  DecodedValue = decode_array(Decoder, T, Data),
  maybe_interpret_value(Decoder, {array, DecodedValue});
decode(Decoder, <<16#9f:8, Data/binary>>) ->
  DecodedValue = decode_indefinite_length_array(Decoder, Data),
  maybe_interpret_value(Decoder, {array, DecodedValue});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#a0, T =< 16#bb ->
  DecodedValue = decode_map(Decoder, T, Data),
  maybe_interpret_value(Decoder, {map, DecodedValue});
decode(Decoder, <<16#bf:8, Data/binary>>) ->
  DecodedValue = decode_indefinite_length_map(Decoder, Data),
  maybe_interpret_value(Decoder, {map, DecodedValue});
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#c0, T =< 16#db ->
  decode_tagged_value(Decoder, T, Data);
decode(Decoder, <<T:8, Data/binary>>) when T >= 16#e0, T =< 16#f8 ->
  DecodedValue = decode_simple_value(T, Data),
  maybe_interpret_value(Decoder, {simple, DecodedValue});
decode(Decoder, <<T, Data/binary>>) when T >= 16#f9, T =< 16#fb ->
  DecodedValue = decode_float(T, Data),
  maybe_interpret_value(Decoder, {float, DecodedValue});
decode(_Decoder, <<T:8, _Data/binary>>) ->
  {error, {invalid_type_tag, T}};
decode(_Decoder, <<>>) ->
  {error, no_input}.

-spec decode_unsigned_integer(Type, nonempty_binary()) ->
        decoding_result(non_neg_integer()) when
    Type :: 16#18..16#1b.
decode_unsigned_integer(16#18, <<I:8, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#19, <<I:16, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#1a, <<I:32, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#1b, <<I:64, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(_Type, _Data) ->
  {error, truncated_unsigned_integer}.

-spec decode_negative_integer(Type, nonempty_binary()) ->
        decoding_result(neg_integer()) when
    Type :: 16#38..16#3b.
decode_negative_integer(16#38, <<I:8, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#39, <<I:16, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#3a, <<I:32, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#3b, <<I:64, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(_Type, _Data) ->
  {error, truncated_negative_integer}.

-spec decode_byte_string(Type, nonempty_binary()) ->
    {ok, nonempty_binary(), binary()}
  | {error, invalid_sequence_header | truncated_byte_string | truncated_sequence_header} when
    Type :: byte().
decode_byte_string(Type, Data) ->
  case erl_cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case Data2 of
        <<Bin:Len/binary, Rest/binary>> ->
          {ok, iolist_to_binary(Bin), Rest};
        _ ->
          {error, truncated_byte_string}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_byte_string(nonempty_binary()) ->
        {ok, nonempty_binary(), binary()} | {error, truncated_byte_string}.
decode_indefinite_length_byte_string(Data) ->
  case binary:match(Data, <<255:8>>) of
    {Off, _Len} ->
      <<Bin:Off/binary, 255:8, Rest/binary>> = Data,
      {ok, Bin, Rest};
    nomatch ->
      {error, truncated_byte_string}
  end.

-spec decode_utf8_string(Type, nonempty_binary()) -> Res when
    Type :: byte(),
    Res :: {ok, nonempty_binary(), binary()}
         | {error, invalid_sequence_header
                 | truncated_sequence_header
                 | truncated_utf8_string
                 | {incomplete_utf8_string, nonempty_binary()}
                 | {invalid_utf8_string, nonempty_binary()}}.
decode_utf8_string(Type, Data) ->
  case erl_cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case Data2 of
        <<Bin:Len/binary, Rest/binary>> ->
          case unicode:characters_to_binary(Bin) of
            Bin2 when is_binary(Bin2) ->
              {ok, Bin2, Rest};
            {error, _, _} ->
              {error, {invalid_utf8_string, Bin}};
            {incomplete, _, _} ->
              {error, {incomplete_utf8_string, Bin}}
          end;
        _ ->
          {error, truncated_utf8_string}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_utf8_string(nonempty_binary()) ->
        {ok, nonempty_binary(), binary()}
      | {error, truncated_utf8_string
              | {incomplete_utf8_string, nonempty_binary()}
              | {invalid_utf8_string, nonempty_binary()}}.
decode_indefinite_length_utf8_string(Data) ->
  case binary:match(Data, <<255:8>>) of
    {Off, _Len} ->
      <<Bin:Off/binary, 255:8, Rest/binary>> = Data,
      case unicode:characters_to_binary(Bin) of
        Bin2 when is_binary(Bin2) ->
          {ok, Bin2, Rest};
        {error, _, _} ->
          {error, {invalid_utf8_string, Bin}};
        {incomplete, _, _} ->
          {error, {incomplete_utf8_string, Bin}}
      end;
    nomatch ->
      {error, truncated_utf8_string}
  end.

-spec decode_array(decoder(), Type, binary()) -> decoding_result(list()) when
    Type :: byte().
decode_array(Decoder, Type, Data) ->
  case erl_cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case decode_values(Decoder, Data2, Len, []) of
        {ok, Values, Rest} ->
          {ok, Values, Rest};
        {error, truncated_sequence} ->
          {error, truncated_array};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_array(decoder(), binary()) ->
        decoding_result(list()).
decode_indefinite_length_array(Decoder, Data) ->
  case decode_indefinite_length_values(Decoder, Data, []) of
    {ok, Values, Rest} ->
      {ok, Values, Rest};
    {error, truncated_sequence} ->
      {error, truncated_array};
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_map(decoder(), Type, binary()) -> decoding_result(map()) when
    Type :: byte().
decode_map(Decoder, Type, Data) ->
  case erl_cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case decode_values(Decoder, Data2, Len*2, []) of
        {ok, Values, Rest} ->
          {ok, erl_cbor_util:list_to_map(Values), Rest};
        {error, truncated_sequence} ->
          {error, truncated_map};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_map(decoder(), binary()) ->
        decoding_result(map()).
decode_indefinite_length_map(Decoder, Data) ->
  case decode_indefinite_length_values(Decoder, Data, []) of
    {ok, Values, _Rest} when (length(Values) rem 2) /= 0 ->
      {error, odd_number_of_map_values};
    {ok, Values, Rest} ->
      {ok, erl_cbor_util:list_to_map(Values), Rest};
    {error, truncated_sequence} ->
      {error, truncated_map};
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_tagged_value(decoder(), Type, binary()) ->
        decoding_result(Result) when
    Type :: 16#c0..16#db,
    Result :: erl_cbor:value().
decode_tagged_value(Decoder, Type, Data) when
    Type >= 16#c0, Type =< 16#d7 ->
  decode_tagged_data(Decoder, Type - 16#c0, Data);
decode_tagged_value(Decoder, 16#d8, <<Tag:8, Data/binary>>) ->
  decode_tagged_data(Decoder, Tag, Data);
decode_tagged_value(Decoder, 16#d9, <<Tag:16, Data/binary>>) ->
  decode_tagged_data(Decoder, Tag, Data);
decode_tagged_value(Decoder, 16#da, <<Tag:32, Data/binary>>) ->
  decode_tagged_data(Decoder, Tag, Data);
decode_tagged_value(Decoder, 16#db, <<Tag:64, Data/binary>>) ->
  decode_tagged_data(Decoder, Tag, Data);
decode_tagged_value(_Decoder, _Type, _Data) ->
  {error, truncated_tagged_value}.

-spec decode_tagged_data(decoder(), erl_cbor:tag(), binary()) ->
        decoding_result(erl_cbor:value()).
decode_tagged_data(Decoder = #decoder{depth = Depth}, Tag, Data) ->
  Decoder2 = Decoder#decoder{depth = Depth+1},
  case decode(Decoder2, Data) of
    {ok, Value, Rest} ->
      case interpret_value(Decoder2, {Tag, Value}) of
        {ok, Value2} ->
          {ok, Value2, Rest};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec maybe_interpret_value(decoder(), {erl_cbor:type(), decoding_result(term())})
  -> decoding_result(term()).
maybe_interpret_value(_Decoder, {_Type, {error, _Reason} = Error}) ->
  Error;
maybe_interpret_value(Decoder, {Type, {ok, Value, Rest}}) ->
  case interpret_value(Decoder, {Type, Value}) of
    {ok, {Type, Value}} ->
      {ok, Value, Rest};
    {ok, Interpreted} ->
      {ok, Interpreted, Rest}
  end.

-spec interpret_value(decoder(), erl_cbor:value()) ->
        interpretation_result(term()).
interpret_value(Decoder = #decoder{options = #{value_interpreters := Interpreters}},
                       TaggedValue = {Tag, _Value}) ->
  case maps:find(Tag, Interpreters) of
    {ok, Interpreter} when is_function(Interpreter, 2) ->
      Interpreter(Decoder, TaggedValue);
    {ok, Interpreter} when is_function(Interpreter, 1) ->
      Interpreter(TaggedValue);
    error ->
      {ok, TaggedValue}
  end;
interpret_value(_Decoder, TaggedValue) ->
  {ok, TaggedValue}.

-spec interpret_utf8_string({erl_cbor:type(), binary()}) ->
        interpretation_result(unicode:chardata()).
interpret_utf8_string({_Tag, Value}) when is_binary(Value) ->
  {ok, Value};
interpret_utf8_string(TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_epoch_based_datetime({erl_cbor:type(), number()}) ->
        interpretation_result(integer()).
interpret_epoch_based_datetime({_Tag, Value}) when
    is_integer(Value) ->
  {ok, Value * 1000000000};
interpret_epoch_based_datetime({_Tag, Value}) when
    is_float(Value) ->
  {ok, round(Value * 1.0e9)};
interpret_epoch_based_datetime(TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_positive_bignum({erl_cbor:type(), binary()}) ->
        interpretation_result(integer()).
interpret_positive_bignum({_Tag, Value}) when is_binary(Value) ->
  Size = byte_size(Value) * 8,
  <<N:Size>> = Value,
  {ok, N};
interpret_positive_bignum(TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_negative_bignum({erl_cbor:type(), binary()}) ->
        interpretation_result(integer()).
interpret_negative_bignum({_Tag, Value}) when is_binary(Value) ->
  Size = byte_size(Value) * 8,
  <<N:Size>> = Value,
  {ok, -1 - N};
interpret_negative_bignum(TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_base64url_data({erl_cbor:type(), binary()}) ->
        interpretation_result(binary()).
interpret_base64url_data({_Tag, Value}) when is_binary(Value) ->
  case erl_cbor_base64url:decode(Value) of
    {ok, Bin} ->
      {ok, Bin};
    {error, Reason} ->
      {error, {invalid_base64url_data, Reason}}
  end;
interpret_base64url_data(TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_base64_data({erl_cbor:type(), binary()}) ->
        interpretation_result(binary()).
interpret_base64_data({_Tag, Value}) when is_binary(Value) ->
  case erl_cbor_base64:decode(Value) of
    {ok, Bin} ->
      {ok, Bin};
    {error, Reason} ->
      {error, {invalid_base64_data, Reason}}
  end;
interpret_base64_data(TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_cbor_value(decoder(), {erl_cbor:type(), binary()}) ->
        interpretation_result(term()).
interpret_cbor_value(Decoder, {_Type, Value}) when is_binary(Value) ->
  case decode(Decoder, Value) of
    {ok, Value2, <<>>} ->
      {ok, Value2};
    {ok, _Value2, Rest} ->
      {error, {invalid_trailing_data, Rest}};
    {error, Reason} ->
      {error, {invalid_cbor_data, Reason}}
  end;
interpret_cbor_value(_Decoder, TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_self_described_cbor_value(erl_cbor:value()) ->
        {ok, erl_cbor:value()}.
interpret_self_described_cbor_value({_Tag, Value}) ->
  {ok, Value}.

-spec decode_simple_value(Type, binary()) ->
        decoding_result(erl_cbor:simple_value()) when
    Type :: 16#e9..16#f8.
decode_simple_value(Type, <<Data/binary>>) when Type >= 16#e0, Type =< 16#f3 ->
  {ok, {simple_value, Type - 16#e0}, Data};
decode_simple_value(16#f4, <<Data/binary>>) ->
  {ok, false, Data};
decode_simple_value(16#f5, <<Data/binary>>) ->
  {ok, true, Data};
decode_simple_value(16#f6, <<Data/binary>>) ->
  {ok, null, Data};
decode_simple_value(16#f7, <<Data/binary>>) ->
  {ok, undefined, Data};
decode_simple_value(16#f8, <<Value:8, Data/binary>>) ->
  {ok, {simple_value, Value}, Data};
decode_simple_value(16#f8, <<>>) ->
  {error, truncated_simple_value}.

-spec decode_float(Type, binary()) -> decoding_result(erl_cbor_float:value()) when
    Type :: 16#f9..16#fb.
%% Half-precision
decode_float(16#f9, <<Data:2/binary, Rest/binary>>) ->
  {ok, erl_cbor_float:decode_f16(Data), Rest};
%% Single precision
decode_float(16#fa, <<Data:4/binary, Rest/binary>>) ->
  {ok, erl_cbor_float:decode_f32(Data), Rest};
%% Double precision
decode_float(16#fb, <<Data:8/binary, Rest/binary>>) ->
  {ok, erl_cbor_float:decode_f64(Data), Rest};
%% Truncated
decode_float(_Type, _Data) ->
  {error, truncated_float}.

-spec decode_values(decoder(), binary(), N, list()) ->
        decoding_result(list()) when
    N :: non_neg_integer().
decode_values(_Decoder, Data, 0, Acc) ->
  {ok, lists:reverse(Acc), Data};
decode_values(_Decoder, <<>>, _N, _Acc) ->
  {error, truncated_sequence};
decode_values(Decoder, Data, N, Acc) ->
  case decode(Decoder#decoder{depth = Decoder#decoder.depth + 1}, Data) of
    {ok, Value, Rest} ->
      decode_values(Decoder, Rest, N-1, [Value | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_values(decoder(), binary(), list()) ->
        decoding_result(list()).
decode_indefinite_length_values(_Decoder, <<>>, _Acc) ->
  {error, truncated_sequence};
decode_indefinite_length_values(_Decoder, <<16#ff:8, Data/binary>>, Acc) ->
  {ok, lists:reverse(Acc), Data};
decode_indefinite_length_values(Decoder, Data, Acc) ->
  case decode(Decoder#decoder{depth = Decoder#decoder.depth + 1}, Data) of
    {ok, Value, Rest} ->
      decode_indefinite_length_values(Decoder, Rest, [Value | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.
