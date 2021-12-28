-module(thoas).

-export([
    decode/1, decode/2, encode/1, encode/2, encode_to_iodata/1,
    encode_to_iodata/2
]).

-export_type([
    decode_error/0, decode_options/0, encode_options/0, json_term/0,
    input_term/0
]).

-type decode_options() :: #{
    strings => reference | copy
}.

-type encode_options() :: #{
    escape => json | unicode | html | javascript
}.

-type json_term() :: 
    integer() |
    float() |
    binary() |
    list(json_term()) |
    #{ binary() => json_term() }.

-type input_term() :: 
    integer() |
    float() |
    binary() |
    atom() |
    list(input_term()) |
    list({binary() | atom(), input_term()}) |
    #{ binary() | atom() => input_term() }.

-type decode_error() ::
    unexpected_end_of_input |
    {unexpected_byte, binary(), integer()} |
    {unexpected_sequence, binary(), integer()}.

-spec decode(iodata()) -> {ok, json_term()} | {error, decode_error()}.
decode(Json) ->
    decode(Json, #{}).

-spec decode(iodata(), decode_options()) -> 
    {ok, json_term()} | {error, decode_error()}.
decode(Json, Options) when is_map(Options) ->
    Binary = iolist_to_binary(Json),
    thoas_decode:decode(Binary, Options).

%% Throws on invalid input
-spec encode(input_term()) -> binary().
encode(Term) ->
    encode(Term, #{}).

%% Throws on invalid input
-spec encode(input_term(), encode_options()) -> binary().
encode(Input, Options) when is_map(Options) ->
    iolist_to_binary(thoas_encode:encode(Input, Options)).

%% Throws on invalid input
-spec encode_to_iodata(input_term()) -> iodata().
encode_to_iodata(Term) ->
    encode_to_iodata(Term, #{}).

%% Throws on invalid input
-spec encode_to_iodata(input_term(), encode_options()) -> iodata().
encode_to_iodata(Input, Options) ->
    thoas_encode:encode(Input, Options).

