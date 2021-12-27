-module(thoas).

-export_type([decode_opt/0]).

-type decode_opt() :: {strings, strings()}.

-export_type([strings/0]).

-type strings() :: reference | copy.

-export_type([encode_opt/0]).

-type encode_opt() :: {escape, escape()}.

-export_type([escape/0]).

-type escape() :: json | unicode | html | javascript.

-export([decode/1,
         decode/2,
         encode/1,
         encode/2,
         encode_to_iodata/1,
         encode_to_iodata/2]).

decode(_@1) ->
    decode(_@1, []).

decode(_input@1, _opts@1) ->
    _input@2 = iolist_to_binary(_input@1),
    thoas_decode:parse(_input@2, format_decode_opts(_opts@1)).

do_encode(_input@1, _opts@1) ->
    thoas_encode:encode(_input@1, _opts@1).

encode(_@1) ->
    encode(_@1, []).

encode(_input@1, _opts@1) ->
    iolist_to_binary(do_encode(_input@1, format_encode_opts(_opts@1))).

encode_to_iodata(_@1) ->
    encode_to_iodata(_@1, []).

encode_to_iodata(_input@1, _opts@1) ->
    do_encode(_input@1, format_encode_opts(_opts@1)).

format_decode_opts(_opts@1) ->
    'Elixir.Enum':into(_opts@1,
                       #{keys => strings, strings => reference}).

format_encode_opts(_opts@1) ->
    'Elixir.Enum':into(_opts@1, #{escape => json, maps => naive}).

