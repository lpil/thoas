-module(jaserl_encode).

-compile([{inline, [{float_, 1}, {integer, 1}]},
          {inline, [{throw_invalid_byte_error, 2}]}]).

-spec integer(integer()) -> iodata().

-spec float_(float()) -> iodata().

-spec encode(any(), map()) -> iodata().

-export([atom/2,
         encode/2,
         float_/1,
         integer/1,
         key/2,
         keyword/2,
         map/2,
         string/2,
         value/2]).


atom(_atom@1, _escape@1) ->
    encode_atom(_atom@1, _escape@1).

encode(_value@1, _opts@1) ->
    _escape@1 = escape_function(_opts@1),
    value(_value@1, _escape@1).

encode_atom(nil, __escape@1) ->
    <<"null">>;
encode_atom(true, __escape@1) ->
    <<"true">>;
encode_atom(false, __escape@1) ->
    <<"false">>;
encode_atom(_atom@1, _escape@1) ->
    encode_string(atom_to_binary(_atom@1, utf8), _escape@1).

encode_string(_string@1, _escape@1) ->
    [34, _escape@1(_string@1, _string@1, 0), 34].

escape(0) ->
    <<"\\u0000">>;
escape(1) ->
    <<"\\u0001">>;
escape(2) ->
    <<"\\u0002">>;
escape(3) ->
    <<"\\u0003">>;
escape(4) ->
    <<"\\u0004">>;
escape(5) ->
    <<"\\u0005">>;
escape(6) ->
    <<"\\u0006">>;
escape(7) ->
    <<"\\u0007">>;
escape(8) ->
    <<"\\b">>;
escape(9) ->
    <<"\\t">>;
escape(10) ->
    <<"\\n">>;
escape(11) ->
    <<"\\u000B">>;
escape(12) ->
    <<"\\f">>;
escape(13) ->
    <<"\\r">>;
escape(14) ->
    <<"\\u000E">>;
escape(15) ->
    <<"\\u000F">>;
escape(16) ->
    <<"\\u0010">>;
escape(17) ->
    <<"\\u0011">>;
escape(18) ->
    <<"\\u0012">>;
escape(19) ->
    <<"\\u0013">>;
escape(20) ->
    <<"\\u0014">>;
escape(21) ->
    <<"\\u0015">>;
escape(22) ->
    <<"\\u0016">>;
escape(23) ->
    <<"\\u0017">>;
escape(24) ->
    <<"\\u0018">>;
escape(25) ->
    <<"\\u0019">>;
escape(26) ->
    <<"\\u001A">>;
escape(27) ->
    <<"\\u001B">>;
escape(28) ->
    <<"\\u001C">>;
escape(29) ->
    <<"\\u001D">>;
escape(30) ->
    <<"\\u001E">>;
escape(31) ->
    <<"\\u001F">>;
escape(32) ->
    throw(error);
escape(33) ->
    throw(error);
escape(34) ->
    <<"\\\"">>;
escape(X) when X < 47 andalso X > 34 ->
    throw(error);
escape(47) ->
    <<"\\/">>;
escape(X) when X < 92 andalso X > 47 ->
    throw(error);
escape(92) ->
    <<"\\\\">>.

escape_function(#{escape := _escape@1}) ->
    case _escape@1 of
        json ->
            fun escape_json/3;
        html ->
            fun escape_html/3;
        unicode ->
            fun escape_unicode/3;
        javascript ->
            fun escape_javascript/3
    end.

escape_html(_data@1, Original, Skip) ->
    escape_html(_data@1, [], Original, Skip).

escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip) when Byte < 33 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip) when Byte =:= 33 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip) when Byte =:= 34 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip) when Byte < 47 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);

escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 47 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 48 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 49 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 50 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 51 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 52 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 53 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 54 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 55 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 56 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 57 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 58 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 59 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 60 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 61 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 62 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 63 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 64 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 65 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 66 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 67 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 68 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 69 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 70 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 71 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 72 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 73 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 74 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 75 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 76 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 77 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 78 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 79 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 80 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 81 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 82 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 83 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 84 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 85 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 86 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 87 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 88 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 89 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 90 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 91 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 93 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 94 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 95 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 96 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 97 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 98 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 99 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 100 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 101 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 102 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 103 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 104 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 105 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 106 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 107 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 108 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 109 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 110 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 111 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 112 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 113 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 114 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 115 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 116 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 117 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 118 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 119 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 120 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 121 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 122 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 123 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 124 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 125 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 126 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Byte/integer,Rest/bitstring>>,
            Acc, Original, Skip)
    when Byte =:= 127 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 1);
escape_html(<<Char/utf8,Rest/bitstring>>,
            Acc, Original, Skip)
    when Char =< 2047 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 2);
escape_html(<<8232/utf8,Rest/bitstring>>,
            Acc, Original, Skip) ->
    Acc2 = [Acc | <<"\\u2028">>],
    escape_html(Rest, Acc2, Original, Skip + 3);
escape_html(<<8233/utf8,Rest/bitstring>>,
            Acc, Original, Skip) ->
    Acc2 = [Acc | <<"\\u2029">>],
    escape_html(Rest, Acc2, Original, Skip + 3);
escape_html(<<Char/utf8,Rest/bitstring>>,
            Acc, Original, Skip)
    when Char =< 65535 ->
    escape_html_chunk(Rest, Acc, Original, Skip, 3);
escape_html(<<_Char/utf8,Rest/bitstring>>,
            Acc, Original, Skip) ->
    escape_html_chunk(Rest, Acc, Original, Skip, 4);
escape_html(<<>>, Acc, _Original, _Skip) ->
    Acc;
escape_html(<<Byte/integer,_Rest/bitstring>>,
            _Acc, Original, _Skip) ->
    throw_invalid_byte_error(Byte, Original).

escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 0 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 1 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 2 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 3 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 4 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 5 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 6 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 7 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 8 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 9 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 10 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 11 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 12 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 13 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 14 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 15 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 16 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 17 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 18 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 19 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 20 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 21 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 22 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 23 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 24 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 25 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 26 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 27 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 28 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 29 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 30 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 31 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 32 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 33 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 34 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 35 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 36 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 37 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 38 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 39 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 40 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 41 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 42 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 43 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 44 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 45 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 46 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 47 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 48 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 49 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 50 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 51 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 52 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 53 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 54 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 55 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 56 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 57 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 58 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 59 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 60 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 61 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 62 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 63 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 64 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 65 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 66 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 67 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 68 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 69 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 70 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 71 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 72 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 73 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 74 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 75 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 76 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 77 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 78 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 79 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 80 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 81 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 82 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 83 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 84 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 85 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 86 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 87 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 88 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 89 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 90 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 91 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 92 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_html(Rest, Acc2, Original, Skip + Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 93 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 94 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 95 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 96 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 97 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 98 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 99 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 100 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 101 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 102 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 103 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 104 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 105 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 106 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 107 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 108 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 109 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 110 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 111 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 112 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 113 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 114 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 115 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 116 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 117 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 118 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 119 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 120 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 121 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 122 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 123 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 124 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 125 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 126 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Byte =:= 127 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_html_chunk(<<Char/utf8,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Char =< 2047 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 2);
escape_html_chunk(<<8232/utf8,Rest/bitstring>>,
                  Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2028">>],
    escape_html(Rest, Acc2, Original, Skip + Len + 3);
escape_html_chunk(<<8233/utf8,Rest/bitstring>>,
                  Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2029">>],
    escape_html(Rest, Acc2, Original, Skip + Len + 3);
escape_html_chunk(<<Char/utf8,Rest/bitstring>>,
                  Acc, Original, Skip, Len)
    when Char =< 65535 ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 3);
escape_html_chunk(<<_Char/utf8,Rest/bitstring>>,
                  Acc, Original, Skip, Len) ->
    escape_html_chunk(Rest, Acc, Original, Skip, Len + 4);
escape_html_chunk(<<>>, Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    [Acc | Part];
escape_html_chunk(<<Byte/integer,_Rest/bitstring>>,
                  _Acc, Original, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Original).

escape_javascript(_data@1, Original, Skip) ->
    escape_javascript(_data@1, [], Original, Skip).

escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 0 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 1 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 2 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 3 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 4 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 5 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 6 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 7 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 8 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 9 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 10 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 11 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 12 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 13 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 14 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 15 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 16 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 17 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 18 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 19 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 20 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 21 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 22 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 23 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 24 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 25 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 26 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 27 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 28 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 29 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 30 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 31 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 32 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 33 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 34 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 35 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 36 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 37 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 38 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 39 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 40 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 41 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 42 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 43 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 44 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 45 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 46 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 47 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 48 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 49 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 50 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 51 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 52 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 53 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 54 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 55 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 56 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 57 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 58 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 59 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 60 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 61 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 62 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 63 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 64 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 65 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 66 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 67 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 68 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 69 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 70 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 71 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 72 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 73 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 74 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 75 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 76 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 77 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 78 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 79 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 80 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 81 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 82 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 83 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 84 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 85 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 86 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 87 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 88 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 89 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 90 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 91 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_javascript(Rest, Acc2, Original, Skip + 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 93 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 94 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 95 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 96 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 97 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 98 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 99 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 100 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 101 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 102 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 103 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 104 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 105 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 106 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 107 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 108 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 109 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 110 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 111 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 112 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 113 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 114 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 115 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 116 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 117 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 118 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 119 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 120 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 121 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 122 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 123 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 124 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 125 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 126 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Byte/integer,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Byte =:= 127 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 1);
escape_javascript(<<Char/utf8,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Char =< 2047 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 2);
escape_javascript(<<8232/utf8,Rest/bitstring>>,
                  Acc, Original, Skip) ->
    Acc2 = [Acc | <<"\\u2028">>],
    escape_javascript(Rest, Acc2, Original, Skip + 3);
escape_javascript(<<8233/utf8,Rest/bitstring>>,
                  Acc, Original, Skip) ->
    Acc2 = [Acc | <<"\\u2029">>],
    escape_javascript(Rest, Acc2, Original, Skip + 3);
escape_javascript(<<Char/utf8,Rest/bitstring>>,
                  Acc, Original, Skip)
    when Char =< 65535 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 3);
escape_javascript(<<_Char/utf8,Rest/bitstring>>,
                  Acc, Original, Skip) ->
    escape_javascript_chunk(Rest, Acc, Original, Skip, 4);
escape_javascript(<<>>, Acc, _Original, _Skip) ->
    Acc;
escape_javascript(<<Byte/integer,_Rest/bitstring>>,
                  _Acc, Original, _Skip) ->
    throw_invalid_byte_error(Byte, Original).

escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 0 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 1 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 2 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 3 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 4 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 5 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 6 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 7 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 8 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 9 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 10 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 11 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 12 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 13 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 14 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 15 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 16 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 17 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 18 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 19 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 20 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 21 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 22 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 23 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 24 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 25 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 26 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 27 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 28 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 29 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 30 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 31 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 32 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 33 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 34 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 35 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 36 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 37 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 38 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 39 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 40 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 41 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 42 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 43 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 44 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 45 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 46 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 47 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 48 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 49 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 50 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 51 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 52 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 53 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 54 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 55 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 56 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 57 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 58 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 59 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 60 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 61 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 62 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 63 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 64 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 65 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 66 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 67 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 68 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 69 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 70 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 71 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 72 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 73 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 74 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 75 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 76 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 77 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 78 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 79 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 80 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 81 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 82 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 83 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 84 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 85 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 86 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 87 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 88 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 89 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 90 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 91 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 92 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 93 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 94 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 95 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 96 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 97 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 98 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 99 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 100 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 101 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 102 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 103 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 104 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 105 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 106 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 107 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 108 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 109 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 110 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 111 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 112 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 113 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 114 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 115 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 116 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 117 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 118 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 119 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 120 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 121 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 122 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 123 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 124 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 125 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 126 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Byte/integer,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Byte =:= 127 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 1);
escape_javascript_chunk(<<Char/utf8,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Char =< 2047 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 2);
escape_javascript_chunk(<<8232/utf8,Rest/bitstring>>,
                        Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2028">>],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 3);
escape_javascript_chunk(<<8233/utf8,Rest/bitstring>>,
                        Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | <<"\\u2029">>],
    escape_javascript(Rest, Acc2, Original,
                      Skip + Len + 3);
escape_javascript_chunk(<<Char/utf8,Rest/bitstring>>,
                        Acc, Original, Skip, Len)
    when Char =< 65535 ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 3);
escape_javascript_chunk(<<_Char/utf8,Rest/bitstring>>,
                        Acc, Original, Skip, Len) ->
    escape_javascript_chunk(Rest, Acc, Original, Skip,
                            Len + 4);
escape_javascript_chunk(<<>>, Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    [Acc | Part];
escape_javascript_chunk(<<Byte/integer,_Rest/bitstring>>,
                        _Acc, Original, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Original).

escape_json(Data, Original, Skip) ->
    escape_json(Data, [], Original, Skip).

escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Original, Skip) when Byte < 32 ->
    Acc2 = [Acc1 | escape(Byte)],
    escape_json(Rest, Acc2, Original, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Original, Skip) when Byte < 34 ->
    escape_json_chunk(Rest, Acc1, Original, Skip, 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Original, Skip) when Byte =:= 34 ->
    Acc2 = [Acc1 | escape(Byte)],
    escape_json(Rest, Acc2, Original, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc1, Original, Skip) when Byte < 92 ->
    escape_json_chunk(Rest, Acc1, Original, Skip, 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip) when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_json(Rest, Acc2, Original, Skip + 1);
escape_json(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip) when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Original, Skip, 1);
escape_json(<<Char/utf8,Rest/bitstring>>, Acc, Original, Skip) when Char =< 2047 ->
    escape_json_chunk(Rest, Acc, Original, Skip, 2);
escape_json(<<Char/utf8,Rest/bitstring>>, Acc, Original, Skip) when Char =< 65535 ->
    escape_json_chunk(Rest, Acc, Original, Skip, 3);
escape_json(<<_Char/utf8,Rest/bitstring>>, Acc, Original, Skip) ->
    escape_json_chunk(Rest, Acc, Original, Skip, 4);
escape_json(<<>>, Acc, _Original, _Skip) ->
    Acc;
escape_json(<<Byte/integer,_Rest/bitstring>>, _Acc, Original, _Skip) ->
    throw_invalid_byte_error(Byte, Original).

escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip, Len) when Byte < 32 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_json(Rest, Acc2, Original, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip, Len) when Byte < 34 ->
    escape_json_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip, Len) when Byte =:= 34 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_json(Rest, Acc2, Original, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip, Len) when Byte < 92 ->
    escape_json_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip, Len) when Byte =:= 92 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_json(Rest, Acc2, Original, Skip + Len + 1);
escape_json_chunk(<<Byte/integer,Rest/bitstring>>, Acc, Original, Skip, Len) when Byte < 128 ->
    escape_json_chunk(Rest, Acc, Original, Skip, Len + 1);
escape_json_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Original, Skip, Len) when Char =< 2047 ->
    escape_json_chunk(Rest, Acc, Original, Skip, Len + 2);
escape_json_chunk(<<Char/utf8,Rest/bitstring>>, Acc, Original, Skip, Len) when Char =< 65535 ->
    escape_json_chunk(Rest, Acc, Original, Skip, Len + 3);
escape_json_chunk(<<_Char/utf8,Rest/bitstring>>, Acc, Original, Skip, Len) ->
    escape_json_chunk(Rest, Acc, Original, Skip, Len + 4);
escape_json_chunk(<<>>, Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    [Acc | Part];
escape_json_chunk(<<Byte/integer,_Rest/bitstring>>, _Acc, Original, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Original).

escape_unicode(_data@1, Original, Skip) ->
    escape_unicode(_data@1, [], Original, Skip).

escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 0 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 1 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 2 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 3 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 4 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 5 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 6 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 7 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 8 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 9 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 10 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 11 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 12 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 13 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 14 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 15 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 16 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 17 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 18 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 19 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 20 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 21 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 22 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 23 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 24 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 25 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 26 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 27 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 28 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 29 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 30 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 31 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 32 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 33 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 34 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 35 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 36 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 37 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 38 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 39 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 40 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 41 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 42 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 43 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 44 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 45 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 46 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 47 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 48 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 49 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 50 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 51 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 52 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 53 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 54 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 55 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 56 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 57 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 58 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 59 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 60 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 61 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 62 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 63 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 64 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 65 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 66 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 67 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 68 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 69 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 70 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 71 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 72 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 73 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 74 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 75 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 76 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 77 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 78 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 79 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 80 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 81 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 82 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 83 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 84 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 85 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 86 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 87 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 88 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 89 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 90 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 91 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 92 ->
    Acc2 = [Acc | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 93 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 94 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 95 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 96 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 97 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 98 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 99 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 100 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 101 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 102 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 103 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 104 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 105 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 106 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 107 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 108 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 109 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 110 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 111 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 112 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 113 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 114 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 115 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 116 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 117 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 118 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 119 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 120 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 121 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 122 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 123 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 124 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 125 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 126 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Byte/integer,Rest/bitstring>>,
               Acc, Original, Skip)
    when Byte =:= 127 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip, 1);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Original, Skip)
    when Char =< 255 ->
    Acc2 = [Acc, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + 2);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Original, Skip)
    when Char =< 2047 ->
    Acc2 = [Acc, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + 2);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Original, Skip)
    when Char =< 4095 ->
    Acc2 = [Acc, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + 3);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Original, Skip)
    when Char =< 65535 ->
    Acc2 = [Acc, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + 3);
escape_unicode(<<Char/utf8,Rest/bitstring>>,
               Acc, Original, Skip) ->
    _char@2 = Char - 65536,
    Acc2 =
        [Acc,
         <<"\\uD">>,
         integer_to_list(2048 bor (_char@2 bsr 10), 16),
         <<"\\uD">> |
         integer_to_list(3072 bor _char@2 band 1023, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + 4);
escape_unicode(<<>>, Acc, _Original, _Skip) ->
    Acc;
escape_unicode(<<Byte/integer,_Rest/bitstring>>,
               _Acc, Original, _Skip) ->
    throw_invalid_byte_error(Byte, Original).

escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 0 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 1 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 2 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 3 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 4 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 5 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 6 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 7 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 8 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 9 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 10 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 11 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 12 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 13 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 14 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 15 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 16 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 17 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 18 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 19 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 20 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 21 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 22 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 23 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 24 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 25 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 26 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 27 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 28 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 29 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 30 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 31 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 32 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 33 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 34 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 35 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 36 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 37 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 38 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 39 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 40 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 41 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 42 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 43 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 44 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 45 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 46 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 47 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 48 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 49 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 50 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 51 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 52 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 53 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 54 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 55 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 56 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 57 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 58 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 59 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 60 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 61 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 62 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 63 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 64 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 65 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 66 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 67 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 68 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 69 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 70 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 71 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 72 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 73 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 74 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 75 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 76 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 77 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 78 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 79 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 80 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 81 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 82 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 83 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 84 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 85 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 86 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 87 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 88 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 89 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 90 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 91 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 92 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part | escape(Byte)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 93 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 94 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 95 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 96 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 97 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 98 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 99 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 100 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 101 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 102 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 103 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 104 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 105 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 106 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 107 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 108 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 109 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 110 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 111 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 112 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 113 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 114 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 115 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 116 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 117 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 118 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 119 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 120 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 121 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 122 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 123 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 124 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 125 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 126 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Byte/integer,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Byte =:= 127 ->
    escape_unicode_chunk(Rest, Acc, Original, Skip,
                         Len + 1);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Char =< 255 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u00">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 2);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Char =< 2047 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 2);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Char =< 4095 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 =
        [Acc, Part, <<"\\u0">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 3);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Original, Skip, Len)
    when Char =< 65535 ->
    Part = binary_part(Original, Skip, Len),
    Acc2 = [Acc, Part, <<"\\u">> | integer_to_list(Char, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 3);
escape_unicode_chunk(<<Char/utf8,Rest/bitstring>>,
                     Acc, Original, Skip, Len) ->
    _char@2 = Char - 65536,
    Part = binary_part(Original, Skip, Len),
    Acc2 =
        [Acc, Part,
         <<"\\uD">>,
         integer_to_list(2048 bor (_char@2 bsr 10), 16),
         <<"\\uD">> |
         integer_to_list(3072 bor _char@2 band 1023, 16)],
    escape_unicode(Rest, Acc2, Original, Skip + Len + 4);
escape_unicode_chunk(<<>>, Acc, Original, Skip, Len) ->
    Part = binary_part(Original, Skip, Len),
    [Acc | Part];
escape_unicode_chunk(<<Byte/integer,_Rest/bitstring>>,
                     _Acc, Original, _Skip, _Len) ->
    throw_invalid_byte_error(Byte, Original).

float_(_float@1) ->
    io_lib_format:fwrite_g(_float@1).

integer(_integer@1) ->
    integer_to_list(_integer@1).

key(_string@1, _escape@1) when is_binary(_string@1) ->
    _escape@1(_string@1, _string@1, 0);
key(_atom@1, _escape@1) when is_atom(_atom@1) ->
    _string@1 = atom_to_binary(_atom@1, utf8),
    _escape@1(_string@1, _string@1, 0);
key(_charlist@1, _escape@1) when is_list(_charlist@1) ->
    _string@1 = list_to_binary(_charlist@1),
    _escape@1(_string@1, _string@1, 0).

keyword(_list@1, _) when _list@1 == [] ->
    <<"{}">>;
keyword(_list@1, _escape@1) when is_list(_list@1) ->
    map_naive(_list@1, _escape@1).

list([], __escape@1) ->
    <<"[]">>;
list([_head@1 | _tail@1], _escape@1) ->
    [91, value(_head@1, _escape@1) | list_loop(_tail@1, _escape@1)].

list_loop([], __escape@1) ->
    [93];
list_loop([_head@1 | _tail@1], _escape@1) ->
    [44, value(_head@1, _escape@1) | list_loop(_tail@1, _escape@1)].

map(_value@1, _escape@1) ->
    case maps:to_list(_value@1) of
        [] ->
            <<"{}">>;
        _keyword@1 ->
            map_naive(_keyword@1, _escape@1)
    end.

map_naive([{_key@1, _value@1} | _tail@1], _escape@1) ->
    [<<"{\"">>,
     key(_key@1, _escape@1),
     <<"\":">>,
     value(_value@1, _escape@1) |
     map_naive_loop(_tail@1, _escape@1)].

map_naive_loop([], __escape@1) ->
    [125];
map_naive_loop([{_key@1, _value@1} | _tail@1], _escape@1) ->
    [<<",\"">>,
     key(_key@1, _escape@1),
     <<"\":">>,
     value(_value@1, _escape@1) |
     map_naive_loop(_tail@1, _escape@1)].

string(_string@1, _escape@1) ->
    encode_string(_string@1, _escape@1).

throw_invalid_byte_error(Byte, Original) ->
    throw({invalid_byte,
           <<"0x"/utf8,(integer_to_binary(Byte, 16))/binary>>,
           Original}).

value(_value@1, _escape@1) when is_atom(_value@1) ->
    encode_atom(_value@1, _escape@1);
value(_value@1, _escape@1) when is_binary(_value@1) ->
    encode_string(_value@1, _escape@1);
value(_value@1, __escape@1) when is_integer(_value@1) ->
    integer(_value@1);
value(_value@1, __escape@1) when is_float(_value@1) ->
    float_(_value@1);
value(_value@1, _escape@1) when is_list(_value@1) ->
    list(_value@1, _escape@1);
value(_value@1, _escape@1) when is_map(_value@1) ->
    case maps:to_list(_value@1) of
        [] ->
            <<"{}">>;
        _keyword@1 ->
            map_naive(_keyword@1, _escape@1)
    end.

