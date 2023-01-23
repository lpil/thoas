-module(prop_thoas).

-export([
    json_term/0, input_term/0
]).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

prop_decode_never_fails() ->
    ?FORALL(
       {Input, DecodeOpts},
       {oneof([
               ?LET(JsonStruct, json_term(), thoas:encode(JsonStruct)),
               proper_unicode:utf8(),
               binary(),
               list(oneof([byte(), binary()]))
              ]),
        decode_options()},
       case thoas:decode(Input, DecodeOpts) of
           {ok, _} ->
               true;
           {error, _} ->
               true;
           Other ->
               throw({unexpected_return, Other})
       end).

prop_valid_input_always_encodable() ->
    ?FORALL(
       {Input, EncOpts},
       {json_term(), encode_options()},
       begin
           is_binary(thoas:encode(Input, EncOpts))
       end).

prop_encode_decode_matches() ->
    ?FORALL(
       {Input, EncOpts},
       {json_term(), encode_options()},
       begin
           Encoded = thoas:encode(Input, EncOpts),
           ?assertEqual({ok, Input}, thoas:decode(Encoded)),
           true
       end).

prop_input_term_encodable_decodable() ->
    ?FORALL(
       {Input, EncOpts},
       {input_term(), encode_options()},
       begin
           Encoded = thoas:encode(Input, EncOpts),
           ?assertMatch({ok, _}, thoas:decode(Encoded)),
           true
       end).

prop_truncated_no_crash() ->
    ?FORALL(
       {Json, CutAt},                         % at least 2 char JSON and position in it where to cut
       ?LET(Json,
            ?SUCHTHAT(Json,
                      ?LET({Term, EncOpts},
                           {json_term(), encode_options()},
                           thoas:encode(Term, EncOpts)),
                      byte_size(Json) > 1),
            {Json, integer(1, byte_size(Json) - 1)}),
       begin
           Part = binary:part(Json, 0, CutAt),
           case thoas:decode(Part) of
               {error, unexpected_end_of_input} ->
                   true;
               {error, {unexpected_byte, _, _}} ->
                   true;
               {ok, Num} when is_number(Num) ->
                   %% Integers can be truncated without anyone noticing
                   true;
               Other ->
                   error({unexpected_return, Other})
           end
       end).

prop_thoas_encode_jsx_decode_match() ->
    ?FORALL(
       Input,
       json_term(),
       begin
           Encoded = thoas:encode(Input),
           ?assertEqual(Input, jsx:decode(Encoded)),
           true
       end).

prop_jsx_encode_thoas_decode_match() ->
    ?FORALL(
       Input,
       json_term(),
       begin
           Encoded = jsx:encode(Input),
           ?assertEqual({ok, Input}, thoas:decode(Encoded)),
           true
       end).

%% Generator

encode_options() ->
    oneof(
      [
       #{},
       ?LET(Escape,
            oneof([json, unicode, html, javascript]),
            #{escape => Escape})
      ]).

decode_options() ->
    oneof(
      [
       #{},
       ?LET(Strings,
            oneof([reference, copy]),
            #{strings => Strings})
      ]).

-define(DIV_FACTOR, 4).

%% flexible representation (accepted by encoder, but can be altered by decoder)
input_term() ->
    ?SIZED(Size, input_term(Size)).

input_term(Size) ->
    ?LAZY(proper_types:frequency(
            [{40, json_term(Size)},
             {20, ?SUCHTHAT(Atom, atom(), not lists:member(Atom, [true, false, null]))},
             {20, j_array(Size, fun input_term/1)},
             {20, j_object(Size, fun input_term/1)}
            ])).

%% Canonical representation (returned by decoder)
json_term() ->
    ?SIZED(Size, json_term(Size)).


json_term(0) ->
    j_literal();
json_term(Size) ->
    ?LAZY(proper_types:frequency(
            [{50, j_literal()},
             {25, j_array(Size)},
             {25, j_object(Size)}])).

j_object(Size) ->
    j_object(Size, fun json_term/1).
j_object(0, _) ->
    #{};
j_object(Size, Of) ->
    ?LET(KV,
         proper_types:list({j_string(), Of(Size div ?DIV_FACTOR)}),
         maps:from_list(KV)).

j_array(Size) ->
    j_array(Size, fun json_term/1).
j_array(0, _) ->
    [];
j_array(Size, Of) ->
    proper_types:list(Of(Size div ?DIV_FACTOR)).

j_string() ->
    proper_types:oneof(
      [alphanum(),
       proper_unicode:utf8()]).


j_literal() ->
    proper_types:oneof(
      [j_string(),
       proper_types:integer(),
       proper_types:float(),
       proper_types:boolean(),
       null]).

alphanum() ->
    %% proper_unicode:utf8().
    ?LET(Str,
         proper_types:non_empty(alphanum_str()),
         unicode:characters_to_binary(Str)).

alphanum_str() ->
    proper_types:list(
      proper_types:oneof(lists:seq($a, $z) ++
                             lists:seq($A, $Z) ++
                             lists:seq($0, $9))).
