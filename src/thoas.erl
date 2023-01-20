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
    boolean() |
    'null' |
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

%% Decode JSON into Erlang terms.
%%
-spec decode(iodata()) -> {ok, json_term()} | {error, decode_error()}.
decode(Json) ->
    decode(Json, #{}).

%% Decode JSON into Erlang terms.
%%
%% # Options
%%
%% - `strings`
%%   - `reference` (default) - when possible thoas tries to create a
%%      sub-binary into the original
%%   - `copy` - always copies the strings. This option is especially 
%%     useful when parts of the decoded data will be stored for a long time (in
%%     ets or some process) to avoid keeping the reference to the original data.
%%
-spec decode(iodata(), decode_options()) -> 
    {ok, json_term()} | {error, decode_error()}.
decode(Json, Options) when is_map(Options) ->
    Binary = iolist_to_binary(Json),
    thoas_decode:decode(Binary, Options).

%% Encode Erlang terms into JSON.
%%
%% Throws on invalid input.
%%
-spec encode(input_term()) -> binary().
encode(Term) ->
    encode(Term, #{}).

%% Encode Erlang terms into JSON.
%%
%% Throws on invalid input.
%%
%% # Options
%%
%% - `escape`
%%   - `json` (default) - the regular JSON escaping as defined by RFC 7159.
%%   - `javascript` - additionally escapes the LINE SEPARATOR (U+2028) and
%%     PARAGRAPH SEPARATOR (U+2029) characters to make the produced JSON valid
%%     JavaScript.
%%   - `html` - similar to `javascript_safe`, but also escapes the / character
%%     to prevent XSS.
%%   - `unicode` - escapes all non-ascii characters.
%%
-spec encode(input_term(), encode_options()) -> binary().
encode(Input, Options) when is_map(Options) ->
    iolist_to_binary(thoas_encode:encode(Input, Options)).

%% Encode Erlang terms into JSON as iodata.
%%
%% This function should be preferred to encode/2, if the generated JSON will be
%% handed over to one of the IO functions or sent over the socket. The Erlang
%% runtime is able to leverage vectorised writes and avoid allocating a continuous
%% buffer for the whole resulting string, lowering memory use and increasing
%% performance.
%%
%% Throws on invalid input.
%%
-spec encode_to_iodata(input_term()) -> iodata().
encode_to_iodata(Term) ->
    encode_to_iodata(Term, #{}).

%% Encode Erlang terms into JSON as iodata.
%%
%% This function should be preferred to encode/2, if the generated JSON will be
%% handed over to one of the IO functions or sent over the socket. The Erlang
%% runtime is able to leverage vectorised writes and avoid allocating a continuous
%% buffer for the whole resulting string, lowering memory use and increasing
%% performance.
%%
%% Throws on invalid input.
%%
%% # Options
%%
%% - `escape`
%%   - `json` (default) - the regular JSON escaping as defined by RFC 7159.
%%   - `javascript` - additionally escapes the LINE SEPARATOR (U+2028) and
%%     PARAGRAPH SEPARATOR (U+2029) characters to make the produced JSON valid
%%     JavaScript.
%%   - `html` - similar to `javascript_safe`, but also escapes the / character
%%     to prevent XSS.
%%   - `unicode` - escapes all non-ascii characters.
%%
-spec encode_to_iodata(input_term(), encode_options()) -> iodata().
encode_to_iodata(Input, Options) ->
    thoas_encode:encode(Input, Options).

