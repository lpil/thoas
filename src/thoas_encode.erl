-module(thoas_encode).

-compile([
    {no_auto_import, [float/1]},
    {inline, [{float, 1}, {integer, 1}, {error_invalid_byte_error, 2}]}
]).

%%% Normal reflection based API that turns Erlang terms into JSON.
-export([encode/2]).

%%% Non-recursive API that expects sub-objects to already be encoded. Likely
%%% useful for statically typed languages such as Gleam.
-export([
    true/0, false/0, null/0, boolean/1, integer/1, float/1, string/1,
    non_recursive_array/1, non_recursive_object/1
]).

%%% A boolean value as JSON.
-spec boolean(boolean()) -> iodata().
boolean('true') -> <<"true">>;
boolean('false') -> <<"false">>.

%%% The JSON value `true`.
-spec true() -> iodata().
true() -> <<"true">>.

%%% The JSON value `false`.
-spec false() -> iodata().
false() -> <<"false">>.

%%% The JSON value `null`.
-spec null() -> iodata().
null() -> <<"null">>.

%%% A float in JSON format.
-spec float(float()) -> iodata().
float(Float) ->
    io_lib_format:fwrite_g(Float).

%%% An integer in JSON format.
-spec integer(integer()) -> iodata().
integer(Int) ->
    integer_to_list(Int).

%%% A string in JSON format, using normal JSON escaping of the contents.
-spec string(binary()) -> iodata().
string(String) ->
    encode_string(String, fun escape_json/3).

%%% An array of JSON values.
%%%
%%% Important: The values supplied in the list are not processed and **must**
%%% already encoded into JSON using one of the other functions, such as
%%% `integer/1` or `string/1`.
%%%
-spec non_recursive_array(list(iodata())) -> iodata().
non_recursive_array([]) ->
    <<"[]">>;
non_recursive_array([First | Rest]) ->
    [$[, First | non_recursive_array_loop(Rest)].

non_recursive_array_loop([]) ->
    [$]];
non_recursive_array_loop([First | Rest]) ->
    [$,, First | non_recursive_array_loop(Rest)].


%%% An object of JSON values.
%%%
%%% Important: The values supplied in the list are not processed and **must**
%%% already encoded into JSON using one of the other functions, such as
%%% `integer/1` or `string/1`.
%%% Keys are processed as strings and get escaped using normal JSON escaping.
%%%
-spec non_recursive_object(list({binary(), iodata()})) -> iodata().
non_recursive_object([]) ->
    <<"{}">>;
non_recursive_object([{Key, Value} | Tail]) ->
    Escape = fun escape_json/3,
    [
        <<"{\"">>, key(Key, Escape), <<"\":">>, Value
        | non_recursive_object_loop(Tail, Escape)
    ].

non_recursive_object_loop([], _Escape) ->
    [$}];
non_recursive_object_loop([{Key, Value} | Tail], Escape) ->
    [
        <<",\"">>, key(Key, Escape), <<"\":">>, Value
        | non_recursive_object_loop(Tail, Escape)
    ].

%%%%
%%%% Recursive encoding functions
%%%%

encode(Value, Opts) ->
    value(Value, escape_function(Opts)).

encode_atom(null, _Escape) -> <<"null">>;
encode_atom(true, _Escape) -> <<"true">>;
encode_atom(false, _Escape) -> <<"false">>;
encode_atom(Atom, Escape) -> encode_string(atom_to_binary(Atom, utf8), Escape).

encode_string(String, Escape) ->
    [$", Escape(String, String, 0), $"].

escape(0) -> <<"\\u0000">>;
escape(1) -> <<"\\u0001">>;
escape(2) -> <<"\\u0002">>;
escape(3) -> <<"\\u0003">>;
escape(4) -> <<"\\u0004">>;
escape(5) -> <<"\\u0005">>;
escape(6) -> <<"\\u0006">>;
escape(7) -> <<"\\u0007">>;
escape(8) -> <<"\\b">>;
escape(9) -> <<"\\t">>;
escape(10) -> <<"\\n">>;
escape(11) -> <<"\\u000B">>;
escape(12) -> <<"\\f">>;
escape(13) -> <<"\\r">>;
escape(14) -> <<"\\u000E">>;
escape(15) -> <<"\\u000F">>;
escape(16) -> <<"\\u0010">>;
escape(17) -> <<"\\u0011">>;
escape(18) -> <<"\\u0012">>;
escape(19) -> <<"\\u0013">>;
escape(20) -> <<"\\u0014">>;
escape(21) -> <<"\\u0015">>;
escape(22) -> <<"\\u0016">>;
escape(23) -> <<"\\u0017">>;
escape(24) -> <<"\\u0018">>;
escape(25) -> <<"\\u0019">>;
escape(26) -> <<"\\u001A">>;
escape(27) -> <<"\\u001B">>;
escape(28) -> <<"\\u001C">>;
escape(29) -> <<"\\u001D">>;
escape(30) -> <<"\\u001E">>;
escape(31) -> <<"\\u001F">>;
escape(32) -> throw(error);
escape(33) -> throw(error);
escape(34) -> <<"\\\"">>;
escape(X) when X < 47 andalso X > 34 -> throw(error);
escape(47) -> <<"\\/">>;
escape(X) when X < 92 andalso X > 47 -> throw(error);
escape(92) -> <<"\\\\">>.

escape_function(Options) ->
    case maps:get(escape, Options, json) of
        json -> fun escape_json/3;
        html -> fun escape_html/3;
        unicode -> fun escape_unicode/3;
        javascript -> fun escape_js/3
    end.

escape_html(Data, Input, Skip) ->
    escape_html(Data, [], Input, Skip).

escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 33 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 33 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 34 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 47 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 47 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 92 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 1);
escape_html(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 2047 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 2);
escape_html(<<8232/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    Acc2 = [Acc | <<"\\u2028">>],
    escape_html(Rest, Acc2, Input, Skip + 3);
escape_html(<<8233/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    Acc2 = [Acc | <<"\\u2029">>],
    escape_html(Rest, Acc2, Input, Skip + 3);
escape_html(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip)
    when Char =< 65535 ->
    escape_html_chunk(Rest, Acc, Input, Skip, 3);
escape_html(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    escape_html_chunk(Rest, Acc, Input, Skip, 4);
escape_html(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_html(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip) ->
    error_invalid_byte_error(Byte, Input).

escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 32 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 34 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 34 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 47 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 47 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 92 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Input, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 128 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_html_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 2047 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 2);
escape_html_chunk(<<8232/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2028">>],
    escape_html(Rest, Acc2, Input, Skip + Len + 3);
escape_html_chunk(<<8233/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2029">>],
    escape_html(Rest, Acc2, Input, Skip + Len + 3);
escape_html_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 65535 ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 3);
escape_html_chunk(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_html_chunk(Rest, Acc, Input, Skip, Len + 4);
escape_html_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_html_chunk(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    error_invalid_byte_error(Byte, Input).

escape_js(Data, Input, Skip) ->
    escape_js(Data, [], Input, Skip).

escape_js(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 32 ->
    Acc2 = [Acc | escape(Byte)],
    escape_js(Rest, Acc2, Input, Skip + 1);
escape_js(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 34 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 1);
escape_js(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 34 ->
    Acc2 = [Acc | escape(Byte)],
    escape_js(Rest, Acc2, Input, Skip + 1);
escape_js(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 92 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 1);
escape_js(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_js(Rest, Acc2, Input, Skip + 1);
escape_js(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 128 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 1);
escape_js(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 2047 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 2);
escape_js(<<8232/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    Acc2 = [Acc | <<"\\u2028">>],
    escape_js(Rest, Acc2, Input, Skip + 3);
escape_js(<<8233/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    Acc2 = [Acc | <<"\\u2029">>],
    escape_js(Rest, Acc2, Input, Skip + 3);
escape_js(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 65535 ->
    escape_js_chunk(Rest, Acc, Input, Skip, 3);
escape_js(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    escape_js_chunk(Rest, Acc, Input, Skip, 4);
escape_js(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_js(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip) ->
    error_invalid_byte_error(Byte, Input).

escape_js_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 32 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_js(Rest, Acc2, Input, Skip + Len + 1);
escape_js_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 34 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_js_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 34 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_js(Rest, Acc2, Input, Skip + Len + 1);
escape_js_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 92 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_js_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_js(Rest, Acc2, Input, Skip + Len + 1);

escape_js_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 128 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_js_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 2047 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len + 2);
escape_js_chunk(<<8232/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2028">>],
    escape_js(Rest, Acc2, Input, Skip + Len + 3);
escape_js_chunk(<<8233/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2029">>],
    escape_js(Rest, Acc2, Input, Skip + Len + 3);
escape_js_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 65535 ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len + 3);
escape_js_chunk(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_js_chunk(Rest, Acc, Input, Skip, Len + 4);
escape_js_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_js_chunk(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    error_invalid_byte_error(Byte, Input).

escape_json(Data, Input, Skip) ->
    escape_json(Data, [], Input, Skip).

escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte < 32 ->
    Acc2 = [Acc1 | escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte < 34 ->
    escape_json_chunk(Rest, Acc1, Input, Skip, 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte =:= 34 ->
    Acc2 = [Acc1 | escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Input, Skip) when Byte < 92 ->
    escape_json_chunk(Rest, Acc1, Input, Skip, 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip) when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 1);
escape_json(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 2047 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 2);
escape_json(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip) when Char =< 65535 ->
    escape_json_chunk(Rest, Acc, Input, Skip, 3);
escape_json(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip) ->
    escape_json_chunk(Rest, Acc, Input, Skip, 4);
escape_json(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_json(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip) ->
    error_invalid_byte_error(Byte, Input).

escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 32 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 34 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 34 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 92 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_json(Rest, Acc2, Input, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Input, Skip, Len) when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 1);
escape_json_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 2047 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 2);
escape_json_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) when Char =< 65535 ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 3);
escape_json_chunk(<<_Char/utf8,Rest/bitstring>>, Acc, Input, Skip, Len) ->
    escape_json_chunk(Rest, Acc, Input, Skip, Len + 4);
escape_json_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_json_chunk(<<Byte/integer,_Rest/bitstring>>, _Acc, Input, _Skip, _Len) ->
    error_invalid_byte_error(Byte, Input).

escape_unicode(Data, Input, Skip) ->
    escape_unicode(Data, [], Input, Skip).

escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 0 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 1 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 2 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 3 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 4 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 5 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 6 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 7 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 8 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 9 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 10 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 11 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 12 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 13 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 14 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 15 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 16 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 17 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 18 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 19 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 20 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 21 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 22 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 23 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 24 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 25 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 26 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 27 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 28 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 29 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 30 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 31 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 32 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 33 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 34 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 35 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 36 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 37 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 38 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 39 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 40 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 41 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 42 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 43 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 44 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 45 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 46 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 47 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 48 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 49 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 50 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 51 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 52 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 53 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 54 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 55 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 56 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 57 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 58 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 59 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 60 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 61 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 62 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 63 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 64 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 65 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 66 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 67 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 68 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 69 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 70 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 71 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 72 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 73 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 74 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 75 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 76 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 77 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 78 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 79 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 80 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 81 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 82 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 83 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 84 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 85 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 86 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 87 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 88 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 89 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 90 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 91 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 93 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 94 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 95 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 96 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 97 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 98 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 99 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 100 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 101 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 102 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 103 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 104 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 105 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 106 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 107 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 108 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 109 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 110 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 111 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 112 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 113 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 114 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 115 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 116 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 117 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 118 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 119 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 120 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 121 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 122 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 123 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 124 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 125 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 126 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Input, Skip)
    when Byte =:= 127 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip, 1);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Input, Skip)
    when Char =< 255 ->
    Acc2 = [Acc, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 2);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Input, Skip)
    when Char =< 2047 ->
    Acc2 = [Acc, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 2);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Input, Skip)
    when Char =< 4095 ->
    Acc2 = [Acc, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 3);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Input, Skip)
    when Char =< 65535 ->
    Acc2 = [Acc, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 3);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Input, Skip) ->
    _char@2 = Char - 65536,
    Acc2 =
        [Acc,
         <<"\\uD">>,
         integer_to_list(2048 bor (_char@2 bsr 10), 16),
         <<"\\uD">> |
         integer_to_list(3072 bor _char@2 band 1023, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + 4);
escape_unicode(<<>>, Acc, _Input, _Skip) ->
    Acc;
escape_unicode(<<Byte/integer,_Rest/bitstring>>,
               _Acc, Input, _Skip) ->
    error_invalid_byte_error(Byte, Input).

escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 0 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 1 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 2 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 3 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 4 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 5 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 6 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 7 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 8 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 9 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 10 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 11 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 12 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 13 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 14 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 15 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 16 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 17 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 18 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 19 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 20 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 21 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 22 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 23 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 24 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 25 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 26 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 27 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 28 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 29 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 30 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 31 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 32 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 33 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 34 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 35 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 36 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 37 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 38 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 39 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 40 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 41 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 42 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 43 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 44 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 45 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 46 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 47 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 48 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 49 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 50 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 51 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 52 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 53 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 54 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 55 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 56 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 57 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 58 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 59 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 60 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 61 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 62 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 63 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 64 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 65 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 66 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 67 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 68 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 69 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 70 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 71 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 72 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 73 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 74 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 75 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 76 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 77 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 78 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 79 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 80 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 81 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 82 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 83 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 84 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 85 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 86 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 87 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 88 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 89 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 90 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 91 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 92 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 93 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 94 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 95 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 96 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 97 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 98 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 99 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 100 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 101 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 102 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 103 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 104 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 105 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 106 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 107 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 108 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 109 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 110 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 111 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 112 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 113 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 114 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 115 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 116 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 117 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 118 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 119 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 120 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 121 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 122 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 123 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 124 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 125 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 126 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Byte =:= 127 ->
    escape_unicode_chunk(Rest, Acc, Input, Skip,
                         Len + 1);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Char =< 255 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 2);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Char =< 2047 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 2);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Char =< 4095 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 3);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Input, Skip, Len)
    when Char =< 65535 ->
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 3);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Input, Skip, Len) ->
    _char@2 = Char - 65536,
    Part = binary_part(Input, Skip, Len),
    Acc2 = [Acc, Part, <<"\\uD">>,
        integer_to_list(2048 bor (_char@2 bsr 10), 16),
        <<"\\uD">>
        | integer_to_list(3072 bor _char@2 band 1023, 16)
    ],
    escape_unicode(Rest, Acc2, Input, Skip + Len + 4);
escape_unicode_chunk(<<>>, Acc, Input, Skip, Len) ->
    Part = binary_part(Input, Skip, Len),
    [Acc | Part];
escape_unicode_chunk(<<Byte/integer,_Rest/bitstring>>,
                     _Acc, Input, _Skip, _Len) ->
    error_invalid_byte_error(Byte, Input).

key(String, Escape) when is_binary(String) ->
    Escape(String, String, 0);
key(Atom, Escape) when is_atom(Atom) ->
    String = atom_to_binary(Atom, utf8),
    Escape(String, String, 0);
key(_charlist@1, Escape) when is_list(_charlist@1) ->
    String = list_to_binary(_charlist@1),
    Escape(String, String, 0);
key(Int, Escape) when is_integer(Int) ->
    String = integer_to_binary(Int),
    Escape(String, String, 0).

list([], _Escape) ->
    <<"[]">>;
list([First | Tail], Escape) ->
    [91, value(First, Escape) | list_loop(Tail, Escape)].

list_loop([], _Escape) ->
    [93];
list_loop([First | Tail], Escape) ->
    [44, value(First, Escape) | list_loop(Tail, Escape)].

map_naive([{Key, Value} | Tail], Escape) ->
    [<<"{\"">>,
     key(Key, Escape),
     <<"\":">>,
     value(Value, Escape) |
     map_naive_loop(Tail, Escape)].

map_naive_loop([], _Escape) ->
    [$}];
map_naive_loop([{Key, Value} | Tail], Escape) ->
    [<<",\"">>,
     key(Key, Escape),
     <<"\":">>,
     value(Value, Escape) |
     map_naive_loop(Tail, Escape)].

error_invalid_byte_error(Byte, Input) ->
    error({invalid_byte,
           <<"0x"/utf8,(integer_to_binary(Byte, 16))/binary>>,
           Input}).

value(Value, Escape) when is_atom(Value) ->
    encode_atom(Value, Escape);
value(Value, Escape) when is_binary(Value) ->
    encode_string(Value, Escape);
value(Value, _Escape) when is_integer(Value) ->
    integer(Value);
value(Value, _Escape) when is_float(Value) ->
    float(Value);
value([{_, _} | _] = Keyword, Escape) ->
    map_naive(Keyword, Escape);
value(Value, Escape) when is_list(Value) ->
    list(Value, Escape);
value(Value, Escape) when is_map(Value) ->
    case maps:to_list(Value) of
        [] ->
            <<"{}">>;
        Keyword ->
            map_naive(Keyword, Escape)
    end.

