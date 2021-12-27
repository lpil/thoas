-module(thoas_decode).

-compile([{inline, [
    {array, 6}, {object, 6}, {token_error, 3}, {token_error, 2}, 
    {throw_error, 2}, {continue, 6}
]}]).

-export([parse/2]).

% We use integers instead of atoms to take advantage of the jump table optimization
-define(terminate, 0).
-define(array, 1).
-define(key, 2).
-define(object, 3).

array(Rest, Input, Skip, Stack, StringDecode) ->
    value(Rest, Input, Skip, [?array, [] | Stack], StringDecode).

array(Data, Input, Skip, Stack, StringDecode, Value) ->
    case Data of
        <<$\t/integer,Rest/bitstring>> ->
            array(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$\n/integer,Rest/bitstring>> ->
            array(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$\r/integer,Rest/bitstring>> ->
            array(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$\s/integer,Rest/bitstring>> ->
            array(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$,/integer,Rest/bitstring>> ->
            [Acc | Stack2] = Stack,
            value(Rest, Input, Skip + 1, [?array, [Value | Acc] | Stack2], StringDecode);
        <<$]/integer,Rest/bitstring>> ->
            [Acc | Stack2] = Stack,
            Value2 = lists:reverse(Acc, [Value]),
            continue(Rest, Input, Skip + 1, Stack2, StringDecode, Value2);
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip);
        <<_/bitstring>> ->
            empty_error(Input, Skip)
    end.

continue(Rest, Input, Skip, Stack, StringDecode, Value) ->
    case Stack of
        [?terminate | Stack2] ->
            terminate(Rest, Input, Skip, Stack2, StringDecode, Value);
        [?array | Stack2] ->
            array(Rest, Input, Skip, Stack2, StringDecode, Value);
        [?key | Stack2] ->
            key(Rest, Input, Skip, Stack2, StringDecode, Value);
        [?object | Stack2] ->
            object(Rest, Input, Skip, Stack2, StringDecode, Value)
    end.

empty_array(<<Rest/bitstring>>, Input, Skip, Stack, StringDecode) ->
    case Stack of
        [?array, [] | _stack@2] ->
            continue(Rest, Input, Skip, _stack@2, StringDecode, []);
        _ ->
            throw_error(Input, Skip - 1)
    end.

empty_error(_Input, Skip) ->
    throw({position, Skip}).

escape(Data, Input, Skip, Stack, StringDecode, Acc) ->
    case Data of
        <<$\"/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $\"], 0);
        <<$//integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $/], 0);
        <<$\\/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $\\], 0);
        <<$b/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $\b], 0);
        <<$f/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $\f], 0);
        <<$n/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $\n], 0);
        <<$r/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $\r], 0);
        <<$t/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 2, Stack, StringDecode, [Acc, $\t], 0);
        <<$u/integer,Rest/bitstring>> ->
            escapeu(Rest, Input, Skip, Stack, StringDecode, Acc);
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip + 1);
        <<_/bitstring>> ->
            empty_error(Input, Skip)
    end.

escape_surrogate(<<92/integer, 117/integer, Int1:16/integer, Int2:16/integer, Rest/bitstring>>,
                 Input, Skip, Stack, StringDecode, Acc, Hi) ->
    Last = escapeu_last(Int2, Input, Skip + 6),
    case Int1 of
        17475 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@1 = Acc,
                       _@2 = 220,
                       _@3 = Last,
                       _@4 = Hi,
                       begin
                           _@5 = _@2 band 3 bsl 8 + _@3,
                           [_@1 | <<(_@4 + _@5)/utf8>>]
                       end
                   end,
                   0);
        17476 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@6 = Acc,
                       _@7 = 221,
                       _@8 = Last,
                       _@9 = Hi,
                       begin
                           _@10 = _@7 band 3 bsl 8 + _@8,
                           [_@6 | <<(_@9 + _@10)/utf8>>]
                       end
                   end,
                   0);
        17477 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@11 = Acc,
                       _@12 = 222,
                       _@13 = Last,
                       _@14 = Hi,
                       begin
                           _@15 = _@12 band 3 bsl 8 + _@13,
                           [_@11 | <<(_@14 + _@15)/utf8>>]
                       end
                   end,
                   0);
        17478 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@16 = Acc,
                       _@17 = 223,
                       _@18 = Last,
                       _@19 = Hi,
                       begin
                           _@20 = _@17 band 3 bsl 8 + _@18,
                           [_@16 | <<(_@19 + _@20)/utf8>>]
                       end
                   end,
                   0);
        17507 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@21 = Acc,
                       _@22 = 220,
                       _@23 = Last,
                       _@24 = Hi,
                       begin
                           _@25 = _@22 band 3 bsl 8 + _@23,
                           [_@21 | <<(_@24 + _@25)/utf8>>]
                       end
                   end,
                   0);
        17508 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@26 = Acc,
                       _@27 = 221,
                       _@28 = Last,
                       _@29 = Hi,
                       begin
                           _@30 = _@27 band 3 bsl 8 + _@28,
                           [_@26 | <<(_@29 + _@30)/utf8>>]
                       end
                   end,
                   0);
        17509 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@31 = Acc,
                       _@32 = 222,
                       _@33 = Last,
                       _@34 = Hi,
                       begin
                           _@35 = _@32 band 3 bsl 8 + _@33,
                           [_@31 | <<(_@34 + _@35)/utf8>>]
                       end
                   end,
                   0);
        17510 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@36 = Acc,
                       _@37 = 223,
                       _@38 = Last,
                       _@39 = Hi,
                       begin
                           _@40 = _@37 band 3 bsl 8 + _@38,
                           [_@36 | <<(_@39 + _@40)/utf8>>]
                       end
                   end,
                   0);
        25667 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@41 = Acc,
                       _@42 = 220,
                       _@43 = Last,
                       _@44 = Hi,
                       begin
                           _@45 = _@42 band 3 bsl 8 + _@43,
                           [_@41 | <<(_@44 + _@45)/utf8>>]
                       end
                   end,
                   0);
        25668 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@46 = Acc,
                       _@47 = 221,
                       _@48 = Last,
                       _@49 = Hi,
                       begin
                           _@50 = _@47 band 3 bsl 8 + _@48,
                           [_@46 | <<(_@49 + _@50)/utf8>>]
                       end
                   end,
                   0);
        25669 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@51 = Acc,
                       _@52 = 222,
                       _@53 = Last,
                       _@54 = Hi,
                       begin
                           _@55 = _@52 band 3 bsl 8 + _@53,
                           [_@51 | <<(_@54 + _@55)/utf8>>]
                       end
                   end,
                   0);
        25670 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@56 = Acc,
                       _@57 = 223,
                       _@58 = Last,
                       _@59 = Hi,
                       begin
                           _@60 = _@57 band 3 bsl 8 + _@58,
                           [_@56 | <<(_@59 + _@60)/utf8>>]
                       end
                   end,
                   0);
        25699 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@61 = Acc,
                       _@62 = 220,
                       _@63 = Last,
                       _@64 = Hi,
                       begin
                           _@65 = _@62 band 3 bsl 8 + _@63,
                           [_@61 | <<(_@64 + _@65)/utf8>>]
                       end
                   end,
                   0);
        25700 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@66 = Acc,
                       _@67 = 221,
                       _@68 = Last,
                       _@69 = Hi,
                       begin
                           _@70 = _@67 band 3 bsl 8 + _@68,
                           [_@66 | <<(_@69 + _@70)/utf8>>]
                       end
                   end,
                   0);
        25701 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@71 = Acc,
                       _@72 = 222,
                       _@73 = Last,
                       _@74 = Hi,
                       begin
                           _@75 = _@72 band 3 bsl 8 + _@73,
                           [_@71 | <<(_@74 + _@75)/utf8>>]
                       end
                   end,
                   0);
        25702 ->
            string(Rest, Input,
                   Skip + 12,
                   Stack, StringDecode,
                   begin
                       _@76 = Acc,
                       _@77 = 223,
                       _@78 = Last,
                       _@79 = Hi,
                       begin
                           _@80 = _@77 band 3 bsl 8 + _@78,
                           [_@76 | <<(_@79 + _@80)/utf8>>]
                       end
                   end,
                   0);
        _ ->
            token_error(Input, Skip, 12)
    end;
escape_surrogate(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, _Acc, _Hi) ->
    throw_error(Input, Skip + 6).

escapeu(<<Int1:16/integer,Int2:16/integer,Rest/bitstring>>,
        Input, Skip, Stack, StringDecode, Acc) ->
    Last = escapeu_last(Int2, Input, Skip),
    case Int1 of
        12336 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1 = Acc,
                       _@2 = 0,
                       _@3 = Last,
                       case _@3 =< 127 of
                           false ->
                               _@4 = 6 bsl 5 + (_@2 bsl 2) + (_@3 bsr 6),
                               _@5 = 2 bsl 6 + _@3 band 63,
                               [_@1, _@4, _@5];
                           true ->
                               [_@1, _@3]
                       end
                   end,
                   0);
        12337 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@6 = Acc,
                       _@7 = 1,
                       _@8 = Last,
                       begin
                           _@9 = 6 bsl 5 + (_@7 bsl 2) + (_@8 bsr 6),
                           _@10 = 2 bsl 6 + _@8 band 63,
                           [_@6, _@9, _@10]
                       end
                   end,
                   0);
        12338 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@11 = Acc,
                       _@12 = 2,
                       _@13 = Last,
                       begin
                           _@14 = 6 bsl 5 + (_@12 bsl 2) + (_@13 bsr 6),
                           _@15 = 2 bsl 6 + _@13 band 63,
                           [_@11, _@14, _@15]
                       end
                   end,
                   0);
        12339 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@16 = Acc,
                       _@17 = 3,
                       _@18 = Last,
                       begin
                           _@19 = 6 bsl 5 + (_@17 bsl 2) + (_@18 bsr 6),
                           _@20 = 2 bsl 6 + _@18 band 63,
                           [_@16, _@19, _@20]
                       end
                   end,
                   0);
        12340 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@21 = Acc,
                       _@22 = 4,
                       _@23 = Last,
                       begin
                           _@24 = 6 bsl 5 + (_@22 bsl 2) + (_@23 bsr 6),
                           _@25 = 2 bsl 6 + _@23 band 63,
                           [_@21, _@24, _@25]
                       end
                   end,
                   0);
        12341 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@26 = Acc,
                       _@27 = 5,
                       _@28 = Last,
                       begin
                           _@29 = 6 bsl 5 + (_@27 bsl 2) + (_@28 bsr 6),
                           _@30 = 2 bsl 6 + _@28 band 63,
                           [_@26, _@29, _@30]
                       end
                   end,
                   0);
        12342 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@31 = Acc,
                       _@32 = 6,
                       _@33 = Last,
                       begin
                           _@34 = 6 bsl 5 + (_@32 bsl 2) + (_@33 bsr 6),
                           _@35 = 2 bsl 6 + _@33 band 63,
                           [_@31, _@34, _@35]
                       end
                   end,
                   0);
        12343 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@36 = Acc,
                       _@37 = 7,
                       _@38 = Last,
                       begin
                           _@39 = 6 bsl 5 + (_@37 bsl 2) + (_@38 bsr 6),
                           _@40 = 2 bsl 6 + _@38 band 63,
                           [_@36, _@39, _@40]
                       end
                   end,
                   0);
        12344 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@41 = Acc,
                       _@42 = 8,
                       _@43 = Last,
                       begin
                           _@44 = 14 bsl 4 + (_@42 bsr 4),
                           _@45 =
                               2 bsl 6 + (_@42 band 15 bsl 2)
                               +
                               (_@43 bsr 6),
                           _@46 = 2 bsl 6 + _@43 band 63,
                           [_@41, _@44, _@45, _@46]
                       end
                   end,
                   0);
        12345 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@47 = Acc,
                       _@48 = 9,
                       _@49 = Last,
                       begin
                           _@50 = 14 bsl 4 + (_@48 bsr 4),
                           _@51 =
                               2 bsl 6 + (_@48 band 15 bsl 2)
                               +
                               (_@49 bsr 6),
                           _@52 = 2 bsl 6 + _@49 band 63,
                           [_@47, _@50, _@51, _@52]
                       end
                   end,
                   0);
        12353 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@53 = Acc,
                       _@54 = 10,
                       _@55 = Last,
                       begin
                           _@56 = 14 bsl 4 + (_@54 bsr 4),
                           _@57 =
                               2 bsl 6 + (_@54 band 15 bsl 2)
                               +
                               (_@55 bsr 6),
                           _@58 = 2 bsl 6 + _@55 band 63,
                           [_@53, _@56, _@57, _@58]
                       end
                   end,
                   0);
        12354 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@59 = Acc,
                       _@60 = 11,
                       _@61 = Last,
                       begin
                           _@62 = 14 bsl 4 + (_@60 bsr 4),
                           _@63 =
                               2 bsl 6 + (_@60 band 15 bsl 2)
                               +
                               (_@61 bsr 6),
                           _@64 = 2 bsl 6 + _@61 band 63,
                           [_@59, _@62, _@63, _@64]
                       end
                   end,
                   0);
        12355 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@65 = Acc,
                       _@66 = 12,
                       _@67 = Last,
                       begin
                           _@68 = 14 bsl 4 + (_@66 bsr 4),
                           _@69 =
                               2 bsl 6 + (_@66 band 15 bsl 2)
                               +
                               (_@67 bsr 6),
                           _@70 = 2 bsl 6 + _@67 band 63,
                           [_@65, _@68, _@69, _@70]
                       end
                   end,
                   0);
        12356 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@71 = Acc,
                       _@72 = 13,
                       _@73 = Last,
                       begin
                           _@74 = 14 bsl 4 + (_@72 bsr 4),
                           _@75 =
                               2 bsl 6 + (_@72 band 15 bsl 2)
                               +
                               (_@73 bsr 6),
                           _@76 = 2 bsl 6 + _@73 band 63,
                           [_@71, _@74, _@75, _@76]
                       end
                   end,
                   0);
        12357 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@77 = Acc,
                       _@78 = 14,
                       _@79 = Last,
                       begin
                           _@80 = 14 bsl 4 + (_@78 bsr 4),
                           _@81 =
                               2 bsl 6 + (_@78 band 15 bsl 2)
                               +
                               (_@79 bsr 6),
                           _@82 = 2 bsl 6 + _@79 band 63,
                           [_@77, _@80, _@81, _@82]
                       end
                   end,
                   0);
        12358 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@83 = Acc,
                       _@84 = 15,
                       _@85 = Last,
                       begin
                           _@86 = 14 bsl 4 + (_@84 bsr 4),
                           _@87 =
                               2 bsl 6 + (_@84 band 15 bsl 2)
                               +
                               (_@85 bsr 6),
                           _@88 = 2 bsl 6 + _@85 band 63,
                           [_@83, _@86, _@87, _@88]
                       end
                   end,
                   0);
        12385 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@89 = Acc,
                       _@90 = 10,
                       _@91 = Last,
                       begin
                           _@92 = 14 bsl 4 + (_@90 bsr 4),
                           _@93 =
                               2 bsl 6 + (_@90 band 15 bsl 2)
                               +
                               (_@91 bsr 6),
                           _@94 = 2 bsl 6 + _@91 band 63,
                           [_@89, _@92, _@93, _@94]
                       end
                   end,
                   0);
        12386 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@95 = Acc,
                       _@96 = 11,
                       _@97 = Last,
                       begin
                           _@98 = 14 bsl 4 + (_@96 bsr 4),
                           _@99 =
                               2 bsl 6 + (_@96 band 15 bsl 2)
                               +
                               (_@97 bsr 6),
                           _@100 = 2 bsl 6 + _@97 band 63,
                           [_@95, _@98, _@99, _@100]
                       end
                   end,
                   0);
        12387 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@101 = Acc,
                       _@102 = 12,
                       _@103 = Last,
                       begin
                           _@104 = 14 bsl 4 + (_@102 bsr 4),
                           _@105 =
                               2 bsl 6 + (_@102 band 15 bsl 2)
                               +
                               (_@103 bsr 6),
                           _@106 = 2 bsl 6 + _@103 band 63,
                           [_@101, _@104, _@105, _@106]
                       end
                   end,
                   0);
        12388 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@107 = Acc,
                       _@108 = 13,
                       _@109 = Last,
                       begin
                           _@110 = 14 bsl 4 + (_@108 bsr 4),
                           _@111 =
                               2 bsl 6 + (_@108 band 15 bsl 2)
                               +
                               (_@109 bsr 6),
                           _@112 = 2 bsl 6 + _@109 band 63,
                           [_@107, _@110, _@111, _@112]
                       end
                   end,
                   0);
        12389 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@113 = Acc,
                       _@114 = 14,
                       _@115 = Last,
                       begin
                           _@116 = 14 bsl 4 + (_@114 bsr 4),
                           _@117 =
                               2 bsl 6 + (_@114 band 15 bsl 2)
                               +
                               (_@115 bsr 6),
                           _@118 = 2 bsl 6 + _@115 band 63,
                           [_@113, _@116, _@117, _@118]
                       end
                   end,
                   0);
        12390 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@119 = Acc,
                       _@120 = 15,
                       _@121 = Last,
                       begin
                           _@122 = 14 bsl 4 + (_@120 bsr 4),
                           _@123 =
                               2 bsl 6 + (_@120 band 15 bsl 2)
                               +
                               (_@121 bsr 6),
                           _@124 = 2 bsl 6 + _@121 band 63,
                           [_@119, _@122, _@123, _@124]
                       end
                   end,
                   0);
        12592 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@125 = Acc,
                       _@126 = 16,
                       _@127 = Last,
                       begin
                           _@128 = 14 bsl 4 + (_@126 bsr 4),
                           _@129 =
                               2 bsl 6 + (_@126 band 15 bsl 2)
                               +
                               (_@127 bsr 6),
                           _@130 = 2 bsl 6 + _@127 band 63,
                           [_@125, _@128, _@129, _@130]
                       end
                   end,
                   0);
        12593 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@131 = Acc,
                       _@132 = 17,
                       _@133 = Last,
                       begin
                           _@134 = 14 bsl 4 + (_@132 bsr 4),
                           _@135 =
                               2 bsl 6 + (_@132 band 15 bsl 2)
                               +
                               (_@133 bsr 6),
                           _@136 = 2 bsl 6 + _@133 band 63,
                           [_@131, _@134, _@135, _@136]
                       end
                   end,
                   0);
        12594 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@137 = Acc,
                       _@138 = 18,
                       _@139 = Last,
                       begin
                           _@140 = 14 bsl 4 + (_@138 bsr 4),
                           _@141 =
                               2 bsl 6 + (_@138 band 15 bsl 2)
                               +
                               (_@139 bsr 6),
                           _@142 = 2 bsl 6 + _@139 band 63,
                           [_@137, _@140, _@141, _@142]
                       end
                   end,
                   0);
        12595 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@143 = Acc,
                       _@144 = 19,
                       _@145 = Last,
                       begin
                           _@146 = 14 bsl 4 + (_@144 bsr 4),
                           _@147 =
                               2 bsl 6 + (_@144 band 15 bsl 2)
                               +
                               (_@145 bsr 6),
                           _@148 = 2 bsl 6 + _@145 band 63,
                           [_@143, _@146, _@147, _@148]
                       end
                   end,
                   0);
        12596 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@149 = Acc,
                       _@150 = 20,
                       _@151 = Last,
                       begin
                           _@152 = 14 bsl 4 + (_@150 bsr 4),
                           _@153 =
                               2 bsl 6 + (_@150 band 15 bsl 2)
                               +
                               (_@151 bsr 6),
                           _@154 = 2 bsl 6 + _@151 band 63,
                           [_@149, _@152, _@153, _@154]
                       end
                   end,
                   0);
        12597 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@155 = Acc,
                       _@156 = 21,
                       _@157 = Last,
                       begin
                           _@158 = 14 bsl 4 + (_@156 bsr 4),
                           _@159 =
                               2 bsl 6 + (_@156 band 15 bsl 2)
                               +
                               (_@157 bsr 6),
                           _@160 = 2 bsl 6 + _@157 band 63,
                           [_@155, _@158, _@159, _@160]
                       end
                   end,
                   0);
        12598 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@161 = Acc,
                       _@162 = 22,
                       _@163 = Last,
                       begin
                           _@164 = 14 bsl 4 + (_@162 bsr 4),
                           _@165 =
                               2 bsl 6 + (_@162 band 15 bsl 2)
                               +
                               (_@163 bsr 6),
                           _@166 = 2 bsl 6 + _@163 band 63,
                           [_@161, _@164, _@165, _@166]
                       end
                   end,
                   0);
        12599 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@167 = Acc,
                       _@168 = 23,
                       _@169 = Last,
                       begin
                           _@170 = 14 bsl 4 + (_@168 bsr 4),
                           _@171 =
                               2 bsl 6 + (_@168 band 15 bsl 2)
                               +
                               (_@169 bsr 6),
                           _@172 = 2 bsl 6 + _@169 band 63,
                           [_@167, _@170, _@171, _@172]
                       end
                   end,
                   0);
        12600 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@173 = Acc,
                       _@174 = 24,
                       _@175 = Last,
                       begin
                           _@176 = 14 bsl 4 + (_@174 bsr 4),
                           _@177 =
                               2 bsl 6 + (_@174 band 15 bsl 2)
                               +
                               (_@175 bsr 6),
                           _@178 = 2 bsl 6 + _@175 band 63,
                           [_@173, _@176, _@177, _@178]
                       end
                   end,
                   0);
        12601 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@179 = Acc,
                       _@180 = 25,
                       _@181 = Last,
                       begin
                           _@182 = 14 bsl 4 + (_@180 bsr 4),
                           _@183 =
                               2 bsl 6 + (_@180 band 15 bsl 2)
                               +
                               (_@181 bsr 6),
                           _@184 = 2 bsl 6 + _@181 band 63,
                           [_@179, _@182, _@183, _@184]
                       end
                   end,
                   0);
        12609 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@185 = Acc,
                       _@186 = 26,
                       _@187 = Last,
                       begin
                           _@188 = 14 bsl 4 + (_@186 bsr 4),
                           _@189 =
                               2 bsl 6 + (_@186 band 15 bsl 2)
                               +
                               (_@187 bsr 6),
                           _@190 = 2 bsl 6 + _@187 band 63,
                           [_@185, _@188, _@189, _@190]
                       end
                   end,
                   0);
        12610 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@191 = Acc,
                       _@192 = 27,
                       _@193 = Last,
                       begin
                           _@194 = 14 bsl 4 + (_@192 bsr 4),
                           _@195 =
                               2 bsl 6 + (_@192 band 15 bsl 2)
                               +
                               (_@193 bsr 6),
                           _@196 = 2 bsl 6 + _@193 band 63,
                           [_@191, _@194, _@195, _@196]
                       end
                   end,
                   0);
        12611 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@197 = Acc,
                       _@198 = 28,
                       _@199 = Last,
                       begin
                           _@200 = 14 bsl 4 + (_@198 bsr 4),
                           _@201 =
                               2 bsl 6 + (_@198 band 15 bsl 2)
                               +
                               (_@199 bsr 6),
                           _@202 = 2 bsl 6 + _@199 band 63,
                           [_@197, _@200, _@201, _@202]
                       end
                   end,
                   0);
        12612 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@203 = Acc,
                       _@204 = 29,
                       _@205 = Last,
                       begin
                           _@206 = 14 bsl 4 + (_@204 bsr 4),
                           _@207 =
                               2 bsl 6 + (_@204 band 15 bsl 2)
                               +
                               (_@205 bsr 6),
                           _@208 = 2 bsl 6 + _@205 band 63,
                           [_@203, _@206, _@207, _@208]
                       end
                   end,
                   0);
        12613 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@209 = Acc,
                       _@210 = 30,
                       _@211 = Last,
                       begin
                           _@212 = 14 bsl 4 + (_@210 bsr 4),
                           _@213 =
                               2 bsl 6 + (_@210 band 15 bsl 2)
                               +
                               (_@211 bsr 6),
                           _@214 = 2 bsl 6 + _@211 band 63,
                           [_@209, _@212, _@213, _@214]
                       end
                   end,
                   0);
        12614 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@215 = Acc,
                       _@216 = 31,
                       _@217 = Last,
                       begin
                           _@218 = 14 bsl 4 + (_@216 bsr 4),
                           _@219 =
                               2 bsl 6 + (_@216 band 15 bsl 2)
                               +
                               (_@217 bsr 6),
                           _@220 = 2 bsl 6 + _@217 band 63,
                           [_@215, _@218, _@219, _@220]
                       end
                   end,
                   0);
        12641 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@221 = Acc,
                       _@222 = 26,
                       _@223 = Last,
                       begin
                           _@224 = 14 bsl 4 + (_@222 bsr 4),
                           _@225 =
                               2 bsl 6 + (_@222 band 15 bsl 2)
                               +
                               (_@223 bsr 6),
                           _@226 = 2 bsl 6 + _@223 band 63,
                           [_@221, _@224, _@225, _@226]
                       end
                   end,
                   0);
        12642 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@227 = Acc,
                       _@228 = 27,
                       _@229 = Last,
                       begin
                           _@230 = 14 bsl 4 + (_@228 bsr 4),
                           _@231 =
                               2 bsl 6 + (_@228 band 15 bsl 2)
                               +
                               (_@229 bsr 6),
                           _@232 = 2 bsl 6 + _@229 band 63,
                           [_@227, _@230, _@231, _@232]
                       end
                   end,
                   0);
        12643 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@233 = Acc,
                       _@234 = 28,
                       _@235 = Last,
                       begin
                           _@236 = 14 bsl 4 + (_@234 bsr 4),
                           _@237 =
                               2 bsl 6 + (_@234 band 15 bsl 2)
                               +
                               (_@235 bsr 6),
                           _@238 = 2 bsl 6 + _@235 band 63,
                           [_@233, _@236, _@237, _@238]
                       end
                   end,
                   0);
        12644 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@239 = Acc,
                       _@240 = 29,
                       _@241 = Last,
                       begin
                           _@242 = 14 bsl 4 + (_@240 bsr 4),
                           _@243 =
                               2 bsl 6 + (_@240 band 15 bsl 2)
                               +
                               (_@241 bsr 6),
                           _@244 = 2 bsl 6 + _@241 band 63,
                           [_@239, _@242, _@243, _@244]
                       end
                   end,
                   0);
        12645 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@245 = Acc,
                       _@246 = 30,
                       _@247 = Last,
                       begin
                           _@248 = 14 bsl 4 + (_@246 bsr 4),
                           _@249 =
                               2 bsl 6 + (_@246 band 15 bsl 2)
                               +
                               (_@247 bsr 6),
                           _@250 = 2 bsl 6 + _@247 band 63,
                           [_@245, _@248, _@249, _@250]
                       end
                   end,
                   0);
        12646 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@251 = Acc,
                       _@252 = 31,
                       _@253 = Last,
                       begin
                           _@254 = 14 bsl 4 + (_@252 bsr 4),
                           _@255 =
                               2 bsl 6 + (_@252 band 15 bsl 2)
                               +
                               (_@253 bsr 6),
                           _@256 = 2 bsl 6 + _@253 band 63,
                           [_@251, _@254, _@255, _@256]
                       end
                   end,
                   0);
        12848 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@257 = Acc,
                       _@258 = 32,
                       _@259 = Last,
                       begin
                           _@260 = 14 bsl 4 + (_@258 bsr 4),
                           _@261 =
                               2 bsl 6 + (_@258 band 15 bsl 2)
                               +
                               (_@259 bsr 6),
                           _@262 = 2 bsl 6 + _@259 band 63,
                           [_@257, _@260, _@261, _@262]
                       end
                   end,
                   0);
        12849 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@263 = Acc,
                       _@264 = 33,
                       _@265 = Last,
                       begin
                           _@266 = 14 bsl 4 + (_@264 bsr 4),
                           _@267 =
                               2 bsl 6 + (_@264 band 15 bsl 2)
                               +
                               (_@265 bsr 6),
                           _@268 = 2 bsl 6 + _@265 band 63,
                           [_@263, _@266, _@267, _@268]
                       end
                   end,
                   0);
        12850 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@269 = Acc,
                       _@270 = 34,
                       _@271 = Last,
                       begin
                           _@272 = 14 bsl 4 + (_@270 bsr 4),
                           _@273 =
                               2 bsl 6 + (_@270 band 15 bsl 2)
                               +
                               (_@271 bsr 6),
                           _@274 = 2 bsl 6 + _@271 band 63,
                           [_@269, _@272, _@273, _@274]
                       end
                   end,
                   0);
        12851 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@275 = Acc,
                       _@276 = 35,
                       _@277 = Last,
                       begin
                           _@278 = 14 bsl 4 + (_@276 bsr 4),
                           _@279 =
                               2 bsl 6 + (_@276 band 15 bsl 2)
                               +
                               (_@277 bsr 6),
                           _@280 = 2 bsl 6 + _@277 band 63,
                           [_@275, _@278, _@279, _@280]
                       end
                   end,
                   0);
        12852 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@281 = Acc,
                       _@282 = 36,
                       _@283 = Last,
                       begin
                           _@284 = 14 bsl 4 + (_@282 bsr 4),
                           _@285 =
                               2 bsl 6 + (_@282 band 15 bsl 2)
                               +
                               (_@283 bsr 6),
                           _@286 = 2 bsl 6 + _@283 band 63,
                           [_@281, _@284, _@285, _@286]
                       end
                   end,
                   0);
        12853 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@287 = Acc,
                       _@288 = 37,
                       _@289 = Last,
                       begin
                           _@290 = 14 bsl 4 + (_@288 bsr 4),
                           _@291 =
                               2 bsl 6 + (_@288 band 15 bsl 2)
                               +
                               (_@289 bsr 6),
                           _@292 = 2 bsl 6 + _@289 band 63,
                           [_@287, _@290, _@291, _@292]
                       end
                   end,
                   0);
        12854 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@293 = Acc,
                       _@294 = 38,
                       _@295 = Last,
                       begin
                           _@296 = 14 bsl 4 + (_@294 bsr 4),
                           _@297 =
                               2 bsl 6 + (_@294 band 15 bsl 2)
                               +
                               (_@295 bsr 6),
                           _@298 = 2 bsl 6 + _@295 band 63,
                           [_@293, _@296, _@297, _@298]
                       end
                   end,
                   0);
        12855 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@299 = Acc,
                       _@300 = 39,
                       _@301 = Last,
                       begin
                           _@302 = 14 bsl 4 + (_@300 bsr 4),
                           _@303 =
                               2 bsl 6 + (_@300 band 15 bsl 2)
                               +
                               (_@301 bsr 6),
                           _@304 = 2 bsl 6 + _@301 band 63,
                           [_@299, _@302, _@303, _@304]
                       end
                   end,
                   0);
        12856 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@305 = Acc,
                       _@306 = 40,
                       _@307 = Last,
                       begin
                           _@308 = 14 bsl 4 + (_@306 bsr 4),
                           _@309 =
                               2 bsl 6 + (_@306 band 15 bsl 2)
                               +
                               (_@307 bsr 6),
                           _@310 = 2 bsl 6 + _@307 band 63,
                           [_@305, _@308, _@309, _@310]
                       end
                   end,
                   0);
        12857 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@311 = Acc,
                       _@312 = 41,
                       _@313 = Last,
                       begin
                           _@314 = 14 bsl 4 + (_@312 bsr 4),
                           _@315 =
                               2 bsl 6 + (_@312 band 15 bsl 2)
                               +
                               (_@313 bsr 6),
                           _@316 = 2 bsl 6 + _@313 band 63,
                           [_@311, _@314, _@315, _@316]
                       end
                   end,
                   0);
        12865 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@317 = Acc,
                       _@318 = 42,
                       _@319 = Last,
                       begin
                           _@320 = 14 bsl 4 + (_@318 bsr 4),
                           _@321 =
                               2 bsl 6 + (_@318 band 15 bsl 2)
                               +
                               (_@319 bsr 6),
                           _@322 = 2 bsl 6 + _@319 band 63,
                           [_@317, _@320, _@321, _@322]
                       end
                   end,
                   0);
        12866 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@323 = Acc,
                       _@324 = 43,
                       _@325 = Last,
                       begin
                           _@326 = 14 bsl 4 + (_@324 bsr 4),
                           _@327 =
                               2 bsl 6 + (_@324 band 15 bsl 2)
                               +
                               (_@325 bsr 6),
                           _@328 = 2 bsl 6 + _@325 band 63,
                           [_@323, _@326, _@327, _@328]
                       end
                   end,
                   0);
        12867 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@329 = Acc,
                       _@330 = 44,
                       _@331 = Last,
                       begin
                           _@332 = 14 bsl 4 + (_@330 bsr 4),
                           _@333 =
                               2 bsl 6 + (_@330 band 15 bsl 2)
                               +
                               (_@331 bsr 6),
                           _@334 = 2 bsl 6 + _@331 band 63,
                           [_@329, _@332, _@333, _@334]
                       end
                   end,
                   0);
        12868 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@335 = Acc,
                       _@336 = 45,
                       _@337 = Last,
                       begin
                           _@338 = 14 bsl 4 + (_@336 bsr 4),
                           _@339 =
                               2 bsl 6 + (_@336 band 15 bsl 2)
                               +
                               (_@337 bsr 6),
                           _@340 = 2 bsl 6 + _@337 band 63,
                           [_@335, _@338, _@339, _@340]
                       end
                   end,
                   0);
        12869 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@341 = Acc,
                       _@342 = 46,
                       _@343 = Last,
                       begin
                           _@344 = 14 bsl 4 + (_@342 bsr 4),
                           _@345 =
                               2 bsl 6 + (_@342 band 15 bsl 2)
                               +
                               (_@343 bsr 6),
                           _@346 = 2 bsl 6 + _@343 band 63,
                           [_@341, _@344, _@345, _@346]
                       end
                   end,
                   0);
        12870 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@347 = Acc,
                       _@348 = 47,
                       _@349 = Last,
                       begin
                           _@350 = 14 bsl 4 + (_@348 bsr 4),
                           _@351 =
                               2 bsl 6 + (_@348 band 15 bsl 2)
                               +
                               (_@349 bsr 6),
                           _@352 = 2 bsl 6 + _@349 band 63,
                           [_@347, _@350, _@351, _@352]
                       end
                   end,
                   0);
        12897 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@353 = Acc,
                       _@354 = 42,
                       _@355 = Last,
                       begin
                           _@356 = 14 bsl 4 + (_@354 bsr 4),
                           _@357 =
                               2 bsl 6 + (_@354 band 15 bsl 2)
                               +
                               (_@355 bsr 6),
                           _@358 = 2 bsl 6 + _@355 band 63,
                           [_@353, _@356, _@357, _@358]
                       end
                   end,
                   0);
        12898 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@359 = Acc,
                       _@360 = 43,
                       _@361 = Last,
                       begin
                           _@362 = 14 bsl 4 + (_@360 bsr 4),
                           _@363 =
                               2 bsl 6 + (_@360 band 15 bsl 2)
                               +
                               (_@361 bsr 6),
                           _@364 = 2 bsl 6 + _@361 band 63,
                           [_@359, _@362, _@363, _@364]
                       end
                   end,
                   0);
        12899 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@365 = Acc,
                       _@366 = 44,
                       _@367 = Last,
                       begin
                           _@368 = 14 bsl 4 + (_@366 bsr 4),
                           _@369 =
                               2 bsl 6 + (_@366 band 15 bsl 2)
                               +
                               (_@367 bsr 6),
                           _@370 = 2 bsl 6 + _@367 band 63,
                           [_@365, _@368, _@369, _@370]
                       end
                   end,
                   0);
        12900 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@371 = Acc,
                       _@372 = 45,
                       _@373 = Last,
                       begin
                           _@374 = 14 bsl 4 + (_@372 bsr 4),
                           _@375 =
                               2 bsl 6 + (_@372 band 15 bsl 2)
                               +
                               (_@373 bsr 6),
                           _@376 = 2 bsl 6 + _@373 band 63,
                           [_@371, _@374, _@375, _@376]
                       end
                   end,
                   0);
        12901 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@377 = Acc,
                       _@378 = 46,
                       _@379 = Last,
                       begin
                           _@380 = 14 bsl 4 + (_@378 bsr 4),
                           _@381 =
                               2 bsl 6 + (_@378 band 15 bsl 2)
                               +
                               (_@379 bsr 6),
                           _@382 = 2 bsl 6 + _@379 band 63,
                           [_@377, _@380, _@381, _@382]
                       end
                   end,
                   0);
        12902 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@383 = Acc,
                       _@384 = 47,
                       _@385 = Last,
                       begin
                           _@386 = 14 bsl 4 + (_@384 bsr 4),
                           _@387 =
                               2 bsl 6 + (_@384 band 15 bsl 2)
                               +
                               (_@385 bsr 6),
                           _@388 = 2 bsl 6 + _@385 band 63,
                           [_@383, _@386, _@387, _@388]
                       end
                   end,
                   0);
        13104 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@389 = Acc,
                       _@390 = 48,
                       _@391 = Last,
                       begin
                           _@392 = 14 bsl 4 + (_@390 bsr 4),
                           _@393 =
                               2 bsl 6 + (_@390 band 15 bsl 2)
                               +
                               (_@391 bsr 6),
                           _@394 = 2 bsl 6 + _@391 band 63,
                           [_@389, _@392, _@393, _@394]
                       end
                   end,
                   0);
        13105 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@395 = Acc,
                       _@396 = 49,
                       _@397 = Last,
                       begin
                           _@398 = 14 bsl 4 + (_@396 bsr 4),
                           _@399 =
                               2 bsl 6 + (_@396 band 15 bsl 2)
                               +
                               (_@397 bsr 6),
                           _@400 = 2 bsl 6 + _@397 band 63,
                           [_@395, _@398, _@399, _@400]
                       end
                   end,
                   0);
        13106 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@401 = Acc,
                       _@402 = 50,
                       _@403 = Last,
                       begin
                           _@404 = 14 bsl 4 + (_@402 bsr 4),
                           _@405 =
                               2 bsl 6 + (_@402 band 15 bsl 2)
                               +
                               (_@403 bsr 6),
                           _@406 = 2 bsl 6 + _@403 band 63,
                           [_@401, _@404, _@405, _@406]
                       end
                   end,
                   0);
        13107 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@407 = Acc,
                       _@408 = 51,
                       _@409 = Last,
                       begin
                           _@410 = 14 bsl 4 + (_@408 bsr 4),
                           _@411 =
                               2 bsl 6 + (_@408 band 15 bsl 2)
                               +
                               (_@409 bsr 6),
                           _@412 = 2 bsl 6 + _@409 band 63,
                           [_@407, _@410, _@411, _@412]
                       end
                   end,
                   0);
        13108 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@413 = Acc,
                       _@414 = 52,
                       _@415 = Last,
                       begin
                           _@416 = 14 bsl 4 + (_@414 bsr 4),
                           _@417 =
                               2 bsl 6 + (_@414 band 15 bsl 2)
                               +
                               (_@415 bsr 6),
                           _@418 = 2 bsl 6 + _@415 band 63,
                           [_@413, _@416, _@417, _@418]
                       end
                   end,
                   0);
        13109 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@419 = Acc,
                       _@420 = 53,
                       _@421 = Last,
                       begin
                           _@422 = 14 bsl 4 + (_@420 bsr 4),
                           _@423 =
                               2 bsl 6 + (_@420 band 15 bsl 2)
                               +
                               (_@421 bsr 6),
                           _@424 = 2 bsl 6 + _@421 band 63,
                           [_@419, _@422, _@423, _@424]
                       end
                   end,
                   0);
        13110 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@425 = Acc,
                       _@426 = 54,
                       _@427 = Last,
                       begin
                           _@428 = 14 bsl 4 + (_@426 bsr 4),
                           _@429 =
                               2 bsl 6 + (_@426 band 15 bsl 2)
                               +
                               (_@427 bsr 6),
                           _@430 = 2 bsl 6 + _@427 band 63,
                           [_@425, _@428, _@429, _@430]
                       end
                   end,
                   0);
        13111 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@431 = Acc,
                       _@432 = 55,
                       _@433 = Last,
                       begin
                           _@434 = 14 bsl 4 + (_@432 bsr 4),
                           _@435 =
                               2 bsl 6 + (_@432 band 15 bsl 2)
                               +
                               (_@433 bsr 6),
                           _@436 = 2 bsl 6 + _@433 band 63,
                           [_@431, _@434, _@435, _@436]
                       end
                   end,
                   0);
        13112 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@437 = Acc,
                       _@438 = 56,
                       _@439 = Last,
                       begin
                           _@440 = 14 bsl 4 + (_@438 bsr 4),
                           _@441 =
                               2 bsl 6 + (_@438 band 15 bsl 2)
                               +
                               (_@439 bsr 6),
                           _@442 = 2 bsl 6 + _@439 band 63,
                           [_@437, _@440, _@441, _@442]
                       end
                   end,
                   0);
        13113 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@443 = Acc,
                       _@444 = 57,
                       _@445 = Last,
                       begin
                           _@446 = 14 bsl 4 + (_@444 bsr 4),
                           _@447 =
                               2 bsl 6 + (_@444 band 15 bsl 2)
                               +
                               (_@445 bsr 6),
                           _@448 = 2 bsl 6 + _@445 band 63,
                           [_@443, _@446, _@447, _@448]
                       end
                   end,
                   0);
        13121 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@449 = Acc,
                       _@450 = 58,
                       _@451 = Last,
                       begin
                           _@452 = 14 bsl 4 + (_@450 bsr 4),
                           _@453 =
                               2 bsl 6 + (_@450 band 15 bsl 2)
                               +
                               (_@451 bsr 6),
                           _@454 = 2 bsl 6 + _@451 band 63,
                           [_@449, _@452, _@453, _@454]
                       end
                   end,
                   0);
        13122 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@455 = Acc,
                       _@456 = 59,
                       _@457 = Last,
                       begin
                           _@458 = 14 bsl 4 + (_@456 bsr 4),
                           _@459 =
                               2 bsl 6 + (_@456 band 15 bsl 2)
                               +
                               (_@457 bsr 6),
                           _@460 = 2 bsl 6 + _@457 band 63,
                           [_@455, _@458, _@459, _@460]
                       end
                   end,
                   0);
        13123 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@461 = Acc,
                       _@462 = 60,
                       _@463 = Last,
                       begin
                           _@464 = 14 bsl 4 + (_@462 bsr 4),
                           _@465 =
                               2 bsl 6 + (_@462 band 15 bsl 2)
                               +
                               (_@463 bsr 6),
                           _@466 = 2 bsl 6 + _@463 band 63,
                           [_@461, _@464, _@465, _@466]
                       end
                   end,
                   0);
        13124 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@467 = Acc,
                       _@468 = 61,
                       _@469 = Last,
                       begin
                           _@470 = 14 bsl 4 + (_@468 bsr 4),
                           _@471 =
                               2 bsl 6 + (_@468 band 15 bsl 2)
                               +
                               (_@469 bsr 6),
                           _@472 = 2 bsl 6 + _@469 band 63,
                           [_@467, _@470, _@471, _@472]
                       end
                   end,
                   0);
        13125 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@473 = Acc,
                       _@474 = 62,
                       _@475 = Last,
                       begin
                           _@476 = 14 bsl 4 + (_@474 bsr 4),
                           _@477 =
                               2 bsl 6 + (_@474 band 15 bsl 2)
                               +
                               (_@475 bsr 6),
                           _@478 = 2 bsl 6 + _@475 band 63,
                           [_@473, _@476, _@477, _@478]
                       end
                   end,
                   0);
        13126 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@479 = Acc,
                       _@480 = 63,
                       _@481 = Last,
                       begin
                           _@482 = 14 bsl 4 + (_@480 bsr 4),
                           _@483 =
                               2 bsl 6 + (_@480 band 15 bsl 2)
                               +
                               (_@481 bsr 6),
                           _@484 = 2 bsl 6 + _@481 band 63,
                           [_@479, _@482, _@483, _@484]
                       end
                   end,
                   0);
        13153 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@485 = Acc,
                       _@486 = 58,
                       _@487 = Last,
                       begin
                           _@488 = 14 bsl 4 + (_@486 bsr 4),
                           _@489 =
                               2 bsl 6 + (_@486 band 15 bsl 2)
                               +
                               (_@487 bsr 6),
                           _@490 = 2 bsl 6 + _@487 band 63,
                           [_@485, _@488, _@489, _@490]
                       end
                   end,
                   0);
        13154 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@491 = Acc,
                       _@492 = 59,
                       _@493 = Last,
                       begin
                           _@494 = 14 bsl 4 + (_@492 bsr 4),
                           _@495 =
                               2 bsl 6 + (_@492 band 15 bsl 2)
                               +
                               (_@493 bsr 6),
                           _@496 = 2 bsl 6 + _@493 band 63,
                           [_@491, _@494, _@495, _@496]
                       end
                   end,
                   0);
        13155 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@497 = Acc,
                       _@498 = 60,
                       _@499 = Last,
                       begin
                           _@500 = 14 bsl 4 + (_@498 bsr 4),
                           _@501 =
                               2 bsl 6 + (_@498 band 15 bsl 2)
                               +
                               (_@499 bsr 6),
                           _@502 = 2 bsl 6 + _@499 band 63,
                           [_@497, _@500, _@501, _@502]
                       end
                   end,
                   0);
        13156 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@503 = Acc,
                       _@504 = 61,
                       _@505 = Last,
                       begin
                           _@506 = 14 bsl 4 + (_@504 bsr 4),
                           _@507 =
                               2 bsl 6 + (_@504 band 15 bsl 2)
                               +
                               (_@505 bsr 6),
                           _@508 = 2 bsl 6 + _@505 band 63,
                           [_@503, _@506, _@507, _@508]
                       end
                   end,
                   0);
        13157 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@509 = Acc,
                       _@510 = 62,
                       _@511 = Last,
                       begin
                           _@512 = 14 bsl 4 + (_@510 bsr 4),
                           _@513 =
                               2 bsl 6 + (_@510 band 15 bsl 2)
                               +
                               (_@511 bsr 6),
                           _@514 = 2 bsl 6 + _@511 band 63,
                           [_@509, _@512, _@513, _@514]
                       end
                   end,
                   0);
        13158 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@515 = Acc,
                       _@516 = 63,
                       _@517 = Last,
                       begin
                           _@518 = 14 bsl 4 + (_@516 bsr 4),
                           _@519 =
                               2 bsl 6 + (_@516 band 15 bsl 2)
                               +
                               (_@517 bsr 6),
                           _@520 = 2 bsl 6 + _@517 band 63,
                           [_@515, _@518, _@519, _@520]
                       end
                   end,
                   0);
        13360 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@521 = Acc,
                       _@522 = 64,
                       _@523 = Last,
                       begin
                           _@524 = 14 bsl 4 + (_@522 bsr 4),
                           _@525 =
                               2 bsl 6 + (_@522 band 15 bsl 2)
                               +
                               (_@523 bsr 6),
                           _@526 = 2 bsl 6 + _@523 band 63,
                           [_@521, _@524, _@525, _@526]
                       end
                   end,
                   0);
        13361 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@527 = Acc,
                       _@528 = 65,
                       _@529 = Last,
                       begin
                           _@530 = 14 bsl 4 + (_@528 bsr 4),
                           _@531 =
                               2 bsl 6 + (_@528 band 15 bsl 2)
                               +
                               (_@529 bsr 6),
                           _@532 = 2 bsl 6 + _@529 band 63,
                           [_@527, _@530, _@531, _@532]
                       end
                   end,
                   0);
        13362 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@533 = Acc,
                       _@534 = 66,
                       _@535 = Last,
                       begin
                           _@536 = 14 bsl 4 + (_@534 bsr 4),
                           _@537 =
                               2 bsl 6 + (_@534 band 15 bsl 2)
                               +
                               (_@535 bsr 6),
                           _@538 = 2 bsl 6 + _@535 band 63,
                           [_@533, _@536, _@537, _@538]
                       end
                   end,
                   0);
        13363 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@539 = Acc,
                       _@540 = 67,
                       _@541 = Last,
                       begin
                           _@542 = 14 bsl 4 + (_@540 bsr 4),
                           _@543 =
                               2 bsl 6 + (_@540 band 15 bsl 2)
                               +
                               (_@541 bsr 6),
                           _@544 = 2 bsl 6 + _@541 band 63,
                           [_@539, _@542, _@543, _@544]
                       end
                   end,
                   0);
        13364 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@545 = Acc,
                       _@546 = 68,
                       _@547 = Last,
                       begin
                           _@548 = 14 bsl 4 + (_@546 bsr 4),
                           _@549 =
                               2 bsl 6 + (_@546 band 15 bsl 2)
                               +
                               (_@547 bsr 6),
                           _@550 = 2 bsl 6 + _@547 band 63,
                           [_@545, _@548, _@549, _@550]
                       end
                   end,
                   0);
        13365 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@551 = Acc,
                       _@552 = 69,
                       _@553 = Last,
                       begin
                           _@554 = 14 bsl 4 + (_@552 bsr 4),
                           _@555 =
                               2 bsl 6 + (_@552 band 15 bsl 2)
                               +
                               (_@553 bsr 6),
                           _@556 = 2 bsl 6 + _@553 band 63,
                           [_@551, _@554, _@555, _@556]
                       end
                   end,
                   0);
        13366 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@557 = Acc,
                       _@558 = 70,
                       _@559 = Last,
                       begin
                           _@560 = 14 bsl 4 + (_@558 bsr 4),
                           _@561 =
                               2 bsl 6 + (_@558 band 15 bsl 2)
                               +
                               (_@559 bsr 6),
                           _@562 = 2 bsl 6 + _@559 band 63,
                           [_@557, _@560, _@561, _@562]
                       end
                   end,
                   0);
        13367 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@563 = Acc,
                       _@564 = 71,
                       _@565 = Last,
                       begin
                           _@566 = 14 bsl 4 + (_@564 bsr 4),
                           _@567 =
                               2 bsl 6 + (_@564 band 15 bsl 2)
                               +
                               (_@565 bsr 6),
                           _@568 = 2 bsl 6 + _@565 band 63,
                           [_@563, _@566, _@567, _@568]
                       end
                   end,
                   0);
        13368 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@569 = Acc,
                       _@570 = 72,
                       _@571 = Last,
                       begin
                           _@572 = 14 bsl 4 + (_@570 bsr 4),
                           _@573 =
                               2 bsl 6 + (_@570 band 15 bsl 2)
                               +
                               (_@571 bsr 6),
                           _@574 = 2 bsl 6 + _@571 band 63,
                           [_@569, _@572, _@573, _@574]
                       end
                   end,
                   0);
        13369 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@575 = Acc,
                       _@576 = 73,
                       _@577 = Last,
                       begin
                           _@578 = 14 bsl 4 + (_@576 bsr 4),
                           _@579 =
                               2 bsl 6 + (_@576 band 15 bsl 2)
                               +
                               (_@577 bsr 6),
                           _@580 = 2 bsl 6 + _@577 band 63,
                           [_@575, _@578, _@579, _@580]
                       end
                   end,
                   0);
        13377 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@581 = Acc,
                       _@582 = 74,
                       _@583 = Last,
                       begin
                           _@584 = 14 bsl 4 + (_@582 bsr 4),
                           _@585 =
                               2 bsl 6 + (_@582 band 15 bsl 2)
                               +
                               (_@583 bsr 6),
                           _@586 = 2 bsl 6 + _@583 band 63,
                           [_@581, _@584, _@585, _@586]
                       end
                   end,
                   0);
        13378 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@587 = Acc,
                       _@588 = 75,
                       _@589 = Last,
                       begin
                           _@590 = 14 bsl 4 + (_@588 bsr 4),
                           _@591 =
                               2 bsl 6 + (_@588 band 15 bsl 2)
                               +
                               (_@589 bsr 6),
                           _@592 = 2 bsl 6 + _@589 band 63,
                           [_@587, _@590, _@591, _@592]
                       end
                   end,
                   0);
        13379 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@593 = Acc,
                       _@594 = 76,
                       _@595 = Last,
                       begin
                           _@596 = 14 bsl 4 + (_@594 bsr 4),
                           _@597 =
                               2 bsl 6 + (_@594 band 15 bsl 2)
                               +
                               (_@595 bsr 6),
                           _@598 = 2 bsl 6 + _@595 band 63,
                           [_@593, _@596, _@597, _@598]
                       end
                   end,
                   0);
        13380 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@599 = Acc,
                       _@600 = 77,
                       _@601 = Last,
                       begin
                           _@602 = 14 bsl 4 + (_@600 bsr 4),
                           _@603 =
                               2 bsl 6 + (_@600 band 15 bsl 2)
                               +
                               (_@601 bsr 6),
                           _@604 = 2 bsl 6 + _@601 band 63,
                           [_@599, _@602, _@603, _@604]
                       end
                   end,
                   0);
        13381 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@605 = Acc,
                       _@606 = 78,
                       _@607 = Last,
                       begin
                           _@608 = 14 bsl 4 + (_@606 bsr 4),
                           _@609 =
                               2 bsl 6 + (_@606 band 15 bsl 2)
                               +
                               (_@607 bsr 6),
                           _@610 = 2 bsl 6 + _@607 band 63,
                           [_@605, _@608, _@609, _@610]
                       end
                   end,
                   0);
        13382 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@611 = Acc,
                       _@612 = 79,
                       _@613 = Last,
                       begin
                           _@614 = 14 bsl 4 + (_@612 bsr 4),
                           _@615 =
                               2 bsl 6 + (_@612 band 15 bsl 2)
                               +
                               (_@613 bsr 6),
                           _@616 = 2 bsl 6 + _@613 band 63,
                           [_@611, _@614, _@615, _@616]
                       end
                   end,
                   0);
        13409 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@617 = Acc,
                       _@618 = 74,
                       _@619 = Last,
                       begin
                           _@620 = 14 bsl 4 + (_@618 bsr 4),
                           _@621 =
                               2 bsl 6 + (_@618 band 15 bsl 2)
                               +
                               (_@619 bsr 6),
                           _@622 = 2 bsl 6 + _@619 band 63,
                           [_@617, _@620, _@621, _@622]
                       end
                   end,
                   0);
        13410 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@623 = Acc,
                       _@624 = 75,
                       _@625 = Last,
                       begin
                           _@626 = 14 bsl 4 + (_@624 bsr 4),
                           _@627 =
                               2 bsl 6 + (_@624 band 15 bsl 2)
                               +
                               (_@625 bsr 6),
                           _@628 = 2 bsl 6 + _@625 band 63,
                           [_@623, _@626, _@627, _@628]
                       end
                   end,
                   0);
        13411 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@629 = Acc,
                       _@630 = 76,
                       _@631 = Last,
                       begin
                           _@632 = 14 bsl 4 + (_@630 bsr 4),
                           _@633 =
                               2 bsl 6 + (_@630 band 15 bsl 2)
                               +
                               (_@631 bsr 6),
                           _@634 = 2 bsl 6 + _@631 band 63,
                           [_@629, _@632, _@633, _@634]
                       end
                   end,
                   0);
        13412 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@635 = Acc,
                       _@636 = 77,
                       _@637 = Last,
                       begin
                           _@638 = 14 bsl 4 + (_@636 bsr 4),
                           _@639 =
                               2 bsl 6 + (_@636 band 15 bsl 2)
                               +
                               (_@637 bsr 6),
                           _@640 = 2 bsl 6 + _@637 band 63,
                           [_@635, _@638, _@639, _@640]
                       end
                   end,
                   0);
        13413 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@641 = Acc,
                       _@642 = 78,
                       _@643 = Last,
                       begin
                           _@644 = 14 bsl 4 + (_@642 bsr 4),
                           _@645 =
                               2 bsl 6 + (_@642 band 15 bsl 2)
                               +
                               (_@643 bsr 6),
                           _@646 = 2 bsl 6 + _@643 band 63,
                           [_@641, _@644, _@645, _@646]
                       end
                   end,
                   0);
        13414 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@647 = Acc,
                       _@648 = 79,
                       _@649 = Last,
                       begin
                           _@650 = 14 bsl 4 + (_@648 bsr 4),
                           _@651 =
                               2 bsl 6 + (_@648 band 15 bsl 2)
                               +
                               (_@649 bsr 6),
                           _@652 = 2 bsl 6 + _@649 band 63,
                           [_@647, _@650, _@651, _@652]
                       end
                   end,
                   0);
        13616 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@653 = Acc,
                       _@654 = 80,
                       _@655 = Last,
                       begin
                           _@656 = 14 bsl 4 + (_@654 bsr 4),
                           _@657 =
                               2 bsl 6 + (_@654 band 15 bsl 2)
                               +
                               (_@655 bsr 6),
                           _@658 = 2 bsl 6 + _@655 band 63,
                           [_@653, _@656, _@657, _@658]
                       end
                   end,
                   0);
        13617 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@659 = Acc,
                       _@660 = 81,
                       _@661 = Last,
                       begin
                           _@662 = 14 bsl 4 + (_@660 bsr 4),
                           _@663 =
                               2 bsl 6 + (_@660 band 15 bsl 2)
                               +
                               (_@661 bsr 6),
                           _@664 = 2 bsl 6 + _@661 band 63,
                           [_@659, _@662, _@663, _@664]
                       end
                   end,
                   0);
        13618 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@665 = Acc,
                       _@666 = 82,
                       _@667 = Last,
                       begin
                           _@668 = 14 bsl 4 + (_@666 bsr 4),
                           _@669 =
                               2 bsl 6 + (_@666 band 15 bsl 2)
                               +
                               (_@667 bsr 6),
                           _@670 = 2 bsl 6 + _@667 band 63,
                           [_@665, _@668, _@669, _@670]
                       end
                   end,
                   0);
        13619 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@671 = Acc,
                       _@672 = 83,
                       _@673 = Last,
                       begin
                           _@674 = 14 bsl 4 + (_@672 bsr 4),
                           _@675 =
                               2 bsl 6 + (_@672 band 15 bsl 2)
                               +
                               (_@673 bsr 6),
                           _@676 = 2 bsl 6 + _@673 band 63,
                           [_@671, _@674, _@675, _@676]
                       end
                   end,
                   0);
        13620 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@677 = Acc,
                       _@678 = 84,
                       _@679 = Last,
                       begin
                           _@680 = 14 bsl 4 + (_@678 bsr 4),
                           _@681 =
                               2 bsl 6 + (_@678 band 15 bsl 2)
                               +
                               (_@679 bsr 6),
                           _@682 = 2 bsl 6 + _@679 band 63,
                           [_@677, _@680, _@681, _@682]
                       end
                   end,
                   0);
        13621 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@683 = Acc,
                       _@684 = 85,
                       _@685 = Last,
                       begin
                           _@686 = 14 bsl 4 + (_@684 bsr 4),
                           _@687 =
                               2 bsl 6 + (_@684 band 15 bsl 2)
                               +
                               (_@685 bsr 6),
                           _@688 = 2 bsl 6 + _@685 band 63,
                           [_@683, _@686, _@687, _@688]
                       end
                   end,
                   0);
        13622 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@689 = Acc,
                       _@690 = 86,
                       _@691 = Last,
                       begin
                           _@692 = 14 bsl 4 + (_@690 bsr 4),
                           _@693 =
                               2 bsl 6 + (_@690 band 15 bsl 2)
                               +
                               (_@691 bsr 6),
                           _@694 = 2 bsl 6 + _@691 band 63,
                           [_@689, _@692, _@693, _@694]
                       end
                   end,
                   0);
        13623 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@695 = Acc,
                       _@696 = 87,
                       _@697 = Last,
                       begin
                           _@698 = 14 bsl 4 + (_@696 bsr 4),
                           _@699 =
                               2 bsl 6 + (_@696 band 15 bsl 2)
                               +
                               (_@697 bsr 6),
                           _@700 = 2 bsl 6 + _@697 band 63,
                           [_@695, _@698, _@699, _@700]
                       end
                   end,
                   0);
        13624 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@701 = Acc,
                       _@702 = 88,
                       _@703 = Last,
                       begin
                           _@704 = 14 bsl 4 + (_@702 bsr 4),
                           _@705 =
                               2 bsl 6 + (_@702 band 15 bsl 2)
                               +
                               (_@703 bsr 6),
                           _@706 = 2 bsl 6 + _@703 band 63,
                           [_@701, _@704, _@705, _@706]
                       end
                   end,
                   0);
        13625 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@707 = Acc,
                       _@708 = 89,
                       _@709 = Last,
                       begin
                           _@710 = 14 bsl 4 + (_@708 bsr 4),
                           _@711 =
                               2 bsl 6 + (_@708 band 15 bsl 2)
                               +
                               (_@709 bsr 6),
                           _@712 = 2 bsl 6 + _@709 band 63,
                           [_@707, _@710, _@711, _@712]
                       end
                   end,
                   0);
        13633 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@713 = Acc,
                       _@714 = 90,
                       _@715 = Last,
                       begin
                           _@716 = 14 bsl 4 + (_@714 bsr 4),
                           _@717 =
                               2 bsl 6 + (_@714 band 15 bsl 2)
                               +
                               (_@715 bsr 6),
                           _@718 = 2 bsl 6 + _@715 band 63,
                           [_@713, _@716, _@717, _@718]
                       end
                   end,
                   0);
        13634 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@719 = Acc,
                       _@720 = 91,
                       _@721 = Last,
                       begin
                           _@722 = 14 bsl 4 + (_@720 bsr 4),
                           _@723 =
                               2 bsl 6 + (_@720 band 15 bsl 2)
                               +
                               (_@721 bsr 6),
                           _@724 = 2 bsl 6 + _@721 band 63,
                           [_@719, _@722, _@723, _@724]
                       end
                   end,
                   0);
        13635 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@725 = Acc,
                       _@726 = 92,
                       _@727 = Last,
                       begin
                           _@728 = 14 bsl 4 + (_@726 bsr 4),
                           _@729 =
                               2 bsl 6 + (_@726 band 15 bsl 2)
                               +
                               (_@727 bsr 6),
                           _@730 = 2 bsl 6 + _@727 band 63,
                           [_@725, _@728, _@729, _@730]
                       end
                   end,
                   0);
        13636 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@731 = Acc,
                       _@732 = 93,
                       _@733 = Last,
                       begin
                           _@734 = 14 bsl 4 + (_@732 bsr 4),
                           _@735 =
                               2 bsl 6 + (_@732 band 15 bsl 2)
                               +
                               (_@733 bsr 6),
                           _@736 = 2 bsl 6 + _@733 band 63,
                           [_@731, _@734, _@735, _@736]
                       end
                   end,
                   0);
        13637 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@737 = Acc,
                       _@738 = 94,
                       _@739 = Last,
                       begin
                           _@740 = 14 bsl 4 + (_@738 bsr 4),
                           _@741 =
                               2 bsl 6 + (_@738 band 15 bsl 2)
                               +
                               (_@739 bsr 6),
                           _@742 = 2 bsl 6 + _@739 band 63,
                           [_@737, _@740, _@741, _@742]
                       end
                   end,
                   0);
        13638 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@743 = Acc,
                       _@744 = 95,
                       _@745 = Last,
                       begin
                           _@746 = 14 bsl 4 + (_@744 bsr 4),
                           _@747 =
                               2 bsl 6 + (_@744 band 15 bsl 2)
                               +
                               (_@745 bsr 6),
                           _@748 = 2 bsl 6 + _@745 band 63,
                           [_@743, _@746, _@747, _@748]
                       end
                   end,
                   0);
        13665 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@749 = Acc,
                       _@750 = 90,
                       _@751 = Last,
                       begin
                           _@752 = 14 bsl 4 + (_@750 bsr 4),
                           _@753 =
                               2 bsl 6 + (_@750 band 15 bsl 2)
                               +
                               (_@751 bsr 6),
                           _@754 = 2 bsl 6 + _@751 band 63,
                           [_@749, _@752, _@753, _@754]
                       end
                   end,
                   0);
        13666 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@755 = Acc,
                       _@756 = 91,
                       _@757 = Last,
                       begin
                           _@758 = 14 bsl 4 + (_@756 bsr 4),
                           _@759 =
                               2 bsl 6 + (_@756 band 15 bsl 2)
                               +
                               (_@757 bsr 6),
                           _@760 = 2 bsl 6 + _@757 band 63,
                           [_@755, _@758, _@759, _@760]
                       end
                   end,
                   0);
        13667 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@761 = Acc,
                       _@762 = 92,
                       _@763 = Last,
                       begin
                           _@764 = 14 bsl 4 + (_@762 bsr 4),
                           _@765 =
                               2 bsl 6 + (_@762 band 15 bsl 2)
                               +
                               (_@763 bsr 6),
                           _@766 = 2 bsl 6 + _@763 band 63,
                           [_@761, _@764, _@765, _@766]
                       end
                   end,
                   0);
        13668 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@767 = Acc,
                       _@768 = 93,
                       _@769 = Last,
                       begin
                           _@770 = 14 bsl 4 + (_@768 bsr 4),
                           _@771 =
                               2 bsl 6 + (_@768 band 15 bsl 2)
                               +
                               (_@769 bsr 6),
                           _@772 = 2 bsl 6 + _@769 band 63,
                           [_@767, _@770, _@771, _@772]
                       end
                   end,
                   0);
        13669 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@773 = Acc,
                       _@774 = 94,
                       _@775 = Last,
                       begin
                           _@776 = 14 bsl 4 + (_@774 bsr 4),
                           _@777 =
                               2 bsl 6 + (_@774 band 15 bsl 2)
                               +
                               (_@775 bsr 6),
                           _@778 = 2 bsl 6 + _@775 band 63,
                           [_@773, _@776, _@777, _@778]
                       end
                   end,
                   0);
        13670 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@779 = Acc,
                       _@780 = 95,
                       _@781 = Last,
                       begin
                           _@782 = 14 bsl 4 + (_@780 bsr 4),
                           _@783 =
                               2 bsl 6 + (_@780 band 15 bsl 2)
                               +
                               (_@781 bsr 6),
                           _@784 = 2 bsl 6 + _@781 band 63,
                           [_@779, _@782, _@783, _@784]
                       end
                   end,
                   0);
        13872 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@785 = Acc,
                       _@786 = 96,
                       _@787 = Last,
                       begin
                           _@788 = 14 bsl 4 + (_@786 bsr 4),
                           _@789 =
                               2 bsl 6 + (_@786 band 15 bsl 2)
                               +
                               (_@787 bsr 6),
                           _@790 = 2 bsl 6 + _@787 band 63,
                           [_@785, _@788, _@789, _@790]
                       end
                   end,
                   0);
        13873 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@791 = Acc,
                       _@792 = 97,
                       _@793 = Last,
                       begin
                           _@794 = 14 bsl 4 + (_@792 bsr 4),
                           _@795 =
                               2 bsl 6 + (_@792 band 15 bsl 2)
                               +
                               (_@793 bsr 6),
                           _@796 = 2 bsl 6 + _@793 band 63,
                           [_@791, _@794, _@795, _@796]
                       end
                   end,
                   0);
        13874 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@797 = Acc,
                       _@798 = 98,
                       _@799 = Last,
                       begin
                           _@800 = 14 bsl 4 + (_@798 bsr 4),
                           _@801 =
                               2 bsl 6 + (_@798 band 15 bsl 2)
                               +
                               (_@799 bsr 6),
                           _@802 = 2 bsl 6 + _@799 band 63,
                           [_@797, _@800, _@801, _@802]
                       end
                   end,
                   0);
        13875 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@803 = Acc,
                       _@804 = 99,
                       _@805 = Last,
                       begin
                           _@806 = 14 bsl 4 + (_@804 bsr 4),
                           _@807 =
                               2 bsl 6 + (_@804 band 15 bsl 2)
                               +
                               (_@805 bsr 6),
                           _@808 = 2 bsl 6 + _@805 band 63,
                           [_@803, _@806, _@807, _@808]
                       end
                   end,
                   0);
        13876 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@809 = Acc,
                       _@810 = 100,
                       _@811 = Last,
                       begin
                           _@812 = 14 bsl 4 + (_@810 bsr 4),
                           _@813 =
                               2 bsl 6 + (_@810 band 15 bsl 2)
                               +
                               (_@811 bsr 6),
                           _@814 = 2 bsl 6 + _@811 band 63,
                           [_@809, _@812, _@813, _@814]
                       end
                   end,
                   0);
        13877 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@815 = Acc,
                       _@816 = 101,
                       _@817 = Last,
                       begin
                           _@818 = 14 bsl 4 + (_@816 bsr 4),
                           _@819 =
                               2 bsl 6 + (_@816 band 15 bsl 2)
                               +
                               (_@817 bsr 6),
                           _@820 = 2 bsl 6 + _@817 band 63,
                           [_@815, _@818, _@819, _@820]
                       end
                   end,
                   0);
        13878 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@821 = Acc,
                       _@822 = 102,
                       _@823 = Last,
                       begin
                           _@824 = 14 bsl 4 + (_@822 bsr 4),
                           _@825 =
                               2 bsl 6 + (_@822 band 15 bsl 2)
                               +
                               (_@823 bsr 6),
                           _@826 = 2 bsl 6 + _@823 band 63,
                           [_@821, _@824, _@825, _@826]
                       end
                   end,
                   0);
        13879 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@827 = Acc,
                       _@828 = 103,
                       _@829 = Last,
                       begin
                           _@830 = 14 bsl 4 + (_@828 bsr 4),
                           _@831 =
                               2 bsl 6 + (_@828 band 15 bsl 2)
                               +
                               (_@829 bsr 6),
                           _@832 = 2 bsl 6 + _@829 band 63,
                           [_@827, _@830, _@831, _@832]
                       end
                   end,
                   0);
        13880 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@833 = Acc,
                       _@834 = 104,
                       _@835 = Last,
                       begin
                           _@836 = 14 bsl 4 + (_@834 bsr 4),
                           _@837 =
                               2 bsl 6 + (_@834 band 15 bsl 2)
                               +
                               (_@835 bsr 6),
                           _@838 = 2 bsl 6 + _@835 band 63,
                           [_@833, _@836, _@837, _@838]
                       end
                   end,
                   0);
        13881 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@839 = Acc,
                       _@840 = 105,
                       _@841 = Last,
                       begin
                           _@842 = 14 bsl 4 + (_@840 bsr 4),
                           _@843 =
                               2 bsl 6 + (_@840 band 15 bsl 2)
                               +
                               (_@841 bsr 6),
                           _@844 = 2 bsl 6 + _@841 band 63,
                           [_@839, _@842, _@843, _@844]
                       end
                   end,
                   0);
        13889 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@845 = Acc,
                       _@846 = 106,
                       _@847 = Last,
                       begin
                           _@848 = 14 bsl 4 + (_@846 bsr 4),
                           _@849 =
                               2 bsl 6 + (_@846 band 15 bsl 2)
                               +
                               (_@847 bsr 6),
                           _@850 = 2 bsl 6 + _@847 band 63,
                           [_@845, _@848, _@849, _@850]
                       end
                   end,
                   0);
        13890 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@851 = Acc,
                       _@852 = 107,
                       _@853 = Last,
                       begin
                           _@854 = 14 bsl 4 + (_@852 bsr 4),
                           _@855 =
                               2 bsl 6 + (_@852 band 15 bsl 2)
                               +
                               (_@853 bsr 6),
                           _@856 = 2 bsl 6 + _@853 band 63,
                           [_@851, _@854, _@855, _@856]
                       end
                   end,
                   0);
        13891 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@857 = Acc,
                       _@858 = 108,
                       _@859 = Last,
                       begin
                           _@860 = 14 bsl 4 + (_@858 bsr 4),
                           _@861 =
                               2 bsl 6 + (_@858 band 15 bsl 2)
                               +
                               (_@859 bsr 6),
                           _@862 = 2 bsl 6 + _@859 band 63,
                           [_@857, _@860, _@861, _@862]
                       end
                   end,
                   0);
        13892 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@863 = Acc,
                       _@864 = 109,
                       _@865 = Last,
                       begin
                           _@866 = 14 bsl 4 + (_@864 bsr 4),
                           _@867 =
                               2 bsl 6 + (_@864 band 15 bsl 2)
                               +
                               (_@865 bsr 6),
                           _@868 = 2 bsl 6 + _@865 band 63,
                           [_@863, _@866, _@867, _@868]
                       end
                   end,
                   0);
        13893 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@869 = Acc,
                       _@870 = 110,
                       _@871 = Last,
                       begin
                           _@872 = 14 bsl 4 + (_@870 bsr 4),
                           _@873 =
                               2 bsl 6 + (_@870 band 15 bsl 2)
                               +
                               (_@871 bsr 6),
                           _@874 = 2 bsl 6 + _@871 band 63,
                           [_@869, _@872, _@873, _@874]
                       end
                   end,
                   0);
        13894 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@875 = Acc,
                       _@876 = 111,
                       _@877 = Last,
                       begin
                           _@878 = 14 bsl 4 + (_@876 bsr 4),
                           _@879 =
                               2 bsl 6 + (_@876 band 15 bsl 2)
                               +
                               (_@877 bsr 6),
                           _@880 = 2 bsl 6 + _@877 band 63,
                           [_@875, _@878, _@879, _@880]
                       end
                   end,
                   0);
        13921 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@881 = Acc,
                       _@882 = 106,
                       _@883 = Last,
                       begin
                           _@884 = 14 bsl 4 + (_@882 bsr 4),
                           _@885 =
                               2 bsl 6 + (_@882 band 15 bsl 2)
                               +
                               (_@883 bsr 6),
                           _@886 = 2 bsl 6 + _@883 band 63,
                           [_@881, _@884, _@885, _@886]
                       end
                   end,
                   0);
        13922 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@887 = Acc,
                       _@888 = 107,
                       _@889 = Last,
                       begin
                           _@890 = 14 bsl 4 + (_@888 bsr 4),
                           _@891 =
                               2 bsl 6 + (_@888 band 15 bsl 2)
                               +
                               (_@889 bsr 6),
                           _@892 = 2 bsl 6 + _@889 band 63,
                           [_@887, _@890, _@891, _@892]
                       end
                   end,
                   0);
        13923 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@893 = Acc,
                       _@894 = 108,
                       _@895 = Last,
                       begin
                           _@896 = 14 bsl 4 + (_@894 bsr 4),
                           _@897 =
                               2 bsl 6 + (_@894 band 15 bsl 2)
                               +
                               (_@895 bsr 6),
                           _@898 = 2 bsl 6 + _@895 band 63,
                           [_@893, _@896, _@897, _@898]
                       end
                   end,
                   0);
        13924 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@899 = Acc,
                       _@900 = 109,
                       _@901 = Last,
                       begin
                           _@902 = 14 bsl 4 + (_@900 bsr 4),
                           _@903 =
                               2 bsl 6 + (_@900 band 15 bsl 2)
                               +
                               (_@901 bsr 6),
                           _@904 = 2 bsl 6 + _@901 band 63,
                           [_@899, _@902, _@903, _@904]
                       end
                   end,
                   0);
        13925 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@905 = Acc,
                       _@906 = 110,
                       _@907 = Last,
                       begin
                           _@908 = 14 bsl 4 + (_@906 bsr 4),
                           _@909 =
                               2 bsl 6 + (_@906 band 15 bsl 2)
                               +
                               (_@907 bsr 6),
                           _@910 = 2 bsl 6 + _@907 band 63,
                           [_@905, _@908, _@909, _@910]
                       end
                   end,
                   0);
        13926 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@911 = Acc,
                       _@912 = 111,
                       _@913 = Last,
                       begin
                           _@914 = 14 bsl 4 + (_@912 bsr 4),
                           _@915 =
                               2 bsl 6 + (_@912 band 15 bsl 2)
                               +
                               (_@913 bsr 6),
                           _@916 = 2 bsl 6 + _@913 band 63,
                           [_@911, _@914, _@915, _@916]
                       end
                   end,
                   0);
        14128 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@917 = Acc,
                       _@918 = 112,
                       _@919 = Last,
                       begin
                           _@920 = 14 bsl 4 + (_@918 bsr 4),
                           _@921 =
                               2 bsl 6 + (_@918 band 15 bsl 2)
                               +
                               (_@919 bsr 6),
                           _@922 = 2 bsl 6 + _@919 band 63,
                           [_@917, _@920, _@921, _@922]
                       end
                   end,
                   0);
        14129 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@923 = Acc,
                       _@924 = 113,
                       _@925 = Last,
                       begin
                           _@926 = 14 bsl 4 + (_@924 bsr 4),
                           _@927 =
                               2 bsl 6 + (_@924 band 15 bsl 2)
                               +
                               (_@925 bsr 6),
                           _@928 = 2 bsl 6 + _@925 band 63,
                           [_@923, _@926, _@927, _@928]
                       end
                   end,
                   0);
        14130 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@929 = Acc,
                       _@930 = 114,
                       _@931 = Last,
                       begin
                           _@932 = 14 bsl 4 + (_@930 bsr 4),
                           _@933 =
                               2 bsl 6 + (_@930 band 15 bsl 2)
                               +
                               (_@931 bsr 6),
                           _@934 = 2 bsl 6 + _@931 band 63,
                           [_@929, _@932, _@933, _@934]
                       end
                   end,
                   0);
        14131 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@935 = Acc,
                       _@936 = 115,
                       _@937 = Last,
                       begin
                           _@938 = 14 bsl 4 + (_@936 bsr 4),
                           _@939 =
                               2 bsl 6 + (_@936 band 15 bsl 2)
                               +
                               (_@937 bsr 6),
                           _@940 = 2 bsl 6 + _@937 band 63,
                           [_@935, _@938, _@939, _@940]
                       end
                   end,
                   0);
        14132 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@941 = Acc,
                       _@942 = 116,
                       _@943 = Last,
                       begin
                           _@944 = 14 bsl 4 + (_@942 bsr 4),
                           _@945 =
                               2 bsl 6 + (_@942 band 15 bsl 2)
                               +
                               (_@943 bsr 6),
                           _@946 = 2 bsl 6 + _@943 band 63,
                           [_@941, _@944, _@945, _@946]
                       end
                   end,
                   0);
        14133 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@947 = Acc,
                       _@948 = 117,
                       _@949 = Last,
                       begin
                           _@950 = 14 bsl 4 + (_@948 bsr 4),
                           _@951 =
                               2 bsl 6 + (_@948 band 15 bsl 2)
                               +
                               (_@949 bsr 6),
                           _@952 = 2 bsl 6 + _@949 band 63,
                           [_@947, _@950, _@951, _@952]
                       end
                   end,
                   0);
        14134 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@953 = Acc,
                       _@954 = 118,
                       _@955 = Last,
                       begin
                           _@956 = 14 bsl 4 + (_@954 bsr 4),
                           _@957 =
                               2 bsl 6 + (_@954 band 15 bsl 2)
                               +
                               (_@955 bsr 6),
                           _@958 = 2 bsl 6 + _@955 band 63,
                           [_@953, _@956, _@957, _@958]
                       end
                   end,
                   0);
        14135 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@959 = Acc,
                       _@960 = 119,
                       _@961 = Last,
                       begin
                           _@962 = 14 bsl 4 + (_@960 bsr 4),
                           _@963 =
                               2 bsl 6 + (_@960 band 15 bsl 2)
                               +
                               (_@961 bsr 6),
                           _@964 = 2 bsl 6 + _@961 band 63,
                           [_@959, _@962, _@963, _@964]
                       end
                   end,
                   0);
        14136 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@965 = Acc,
                       _@966 = 120,
                       _@967 = Last,
                       begin
                           _@968 = 14 bsl 4 + (_@966 bsr 4),
                           _@969 =
                               2 bsl 6 + (_@966 band 15 bsl 2)
                               +
                               (_@967 bsr 6),
                           _@970 = 2 bsl 6 + _@967 band 63,
                           [_@965, _@968, _@969, _@970]
                       end
                   end,
                   0);
        14137 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@971 = Acc,
                       _@972 = 121,
                       _@973 = Last,
                       begin
                           _@974 = 14 bsl 4 + (_@972 bsr 4),
                           _@975 =
                               2 bsl 6 + (_@972 band 15 bsl 2)
                               +
                               (_@973 bsr 6),
                           _@976 = 2 bsl 6 + _@973 band 63,
                           [_@971, _@974, _@975, _@976]
                       end
                   end,
                   0);
        14145 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@977 = Acc,
                       _@978 = 122,
                       _@979 = Last,
                       begin
                           _@980 = 14 bsl 4 + (_@978 bsr 4),
                           _@981 =
                               2 bsl 6 + (_@978 band 15 bsl 2)
                               +
                               (_@979 bsr 6),
                           _@982 = 2 bsl 6 + _@979 band 63,
                           [_@977, _@980, _@981, _@982]
                       end
                   end,
                   0);
        14146 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@983 = Acc,
                       _@984 = 123,
                       _@985 = Last,
                       begin
                           _@986 = 14 bsl 4 + (_@984 bsr 4),
                           _@987 =
                               2 bsl 6 + (_@984 band 15 bsl 2)
                               +
                               (_@985 bsr 6),
                           _@988 = 2 bsl 6 + _@985 band 63,
                           [_@983, _@986, _@987, _@988]
                       end
                   end,
                   0);
        14147 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@989 = Acc,
                       _@990 = 124,
                       _@991 = Last,
                       begin
                           _@992 = 14 bsl 4 + (_@990 bsr 4),
                           _@993 =
                               2 bsl 6 + (_@990 band 15 bsl 2)
                               +
                               (_@991 bsr 6),
                           _@994 = 2 bsl 6 + _@991 band 63,
                           [_@989, _@992, _@993, _@994]
                       end
                   end,
                   0);
        14148 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@995 = Acc,
                       _@996 = 125,
                       _@997 = Last,
                       begin
                           _@998 = 14 bsl 4 + (_@996 bsr 4),
                           _@999 =
                               2 bsl 6 + (_@996 band 15 bsl 2)
                               +
                               (_@997 bsr 6),
                           _@1000 = 2 bsl 6 + _@997 band 63,
                           [_@995, _@998, _@999, _@1000]
                       end
                   end,
                   0);
        14149 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1001 = Acc,
                       _@1002 = 126,
                       _@1003 = Last,
                       begin
                           _@1004 = 14 bsl 4 + (_@1002 bsr 4),
                           _@1005 =
                               2 bsl 6 + (_@1002 band 15 bsl 2)
                               +
                               (_@1003 bsr 6),
                           _@1006 = 2 bsl 6 + _@1003 band 63,
                           [_@1001, _@1004, _@1005, _@1006]
                       end
                   end,
                   0);
        14150 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1007 = Acc,
                       _@1008 = 127,
                       _@1009 = Last,
                       begin
                           _@1010 = 14 bsl 4 + (_@1008 bsr 4),
                           _@1011 =
                               2 bsl 6 + (_@1008 band 15 bsl 2)
                               +
                               (_@1009 bsr 6),
                           _@1012 = 2 bsl 6 + _@1009 band 63,
                           [_@1007, _@1010, _@1011, _@1012]
                       end
                   end,
                   0);
        14177 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1013 = Acc,
                       _@1014 = 122,
                       _@1015 = Last,
                       begin
                           _@1016 = 14 bsl 4 + (_@1014 bsr 4),
                           _@1017 =
                               2 bsl 6 + (_@1014 band 15 bsl 2)
                               +
                               (_@1015 bsr 6),
                           _@1018 = 2 bsl 6 + _@1015 band 63,
                           [_@1013, _@1016, _@1017, _@1018]
                       end
                   end,
                   0);
        14178 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1019 = Acc,
                       _@1020 = 123,
                       _@1021 = Last,
                       begin
                           _@1022 = 14 bsl 4 + (_@1020 bsr 4),
                           _@1023 =
                               2 bsl 6 + (_@1020 band 15 bsl 2)
                               +
                               (_@1021 bsr 6),
                           _@1024 = 2 bsl 6 + _@1021 band 63,
                           [_@1019, _@1022, _@1023, _@1024]
                       end
                   end,
                   0);
        14179 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1025 = Acc,
                       _@1026 = 124,
                       _@1027 = Last,
                       begin
                           _@1028 = 14 bsl 4 + (_@1026 bsr 4),
                           _@1029 =
                               2 bsl 6 + (_@1026 band 15 bsl 2)
                               +
                               (_@1027 bsr 6),
                           _@1030 = 2 bsl 6 + _@1027 band 63,
                           [_@1025, _@1028, _@1029, _@1030]
                       end
                   end,
                   0);
        14180 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1031 = Acc,
                       _@1032 = 125,
                       _@1033 = Last,
                       begin
                           _@1034 = 14 bsl 4 + (_@1032 bsr 4),
                           _@1035 =
                               2 bsl 6 + (_@1032 band 15 bsl 2)
                               +
                               (_@1033 bsr 6),
                           _@1036 = 2 bsl 6 + _@1033 band 63,
                           [_@1031, _@1034, _@1035, _@1036]
                       end
                   end,
                   0);
        14181 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1037 = Acc,
                       _@1038 = 126,
                       _@1039 = Last,
                       begin
                           _@1040 = 14 bsl 4 + (_@1038 bsr 4),
                           _@1041 =
                               2 bsl 6 + (_@1038 band 15 bsl 2)
                               +
                               (_@1039 bsr 6),
                           _@1042 = 2 bsl 6 + _@1039 band 63,
                           [_@1037, _@1040, _@1041, _@1042]
                       end
                   end,
                   0);
        14182 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1043 = Acc,
                       _@1044 = 127,
                       _@1045 = Last,
                       begin
                           _@1046 = 14 bsl 4 + (_@1044 bsr 4),
                           _@1047 =
                               2 bsl 6 + (_@1044 band 15 bsl 2)
                               +
                               (_@1045 bsr 6),
                           _@1048 = 2 bsl 6 + _@1045 band 63,
                           [_@1043, _@1046, _@1047, _@1048]
                       end
                   end,
                   0);
        14384 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1049 = Acc,
                       _@1050 = 128,
                       _@1051 = Last,
                       begin
                           _@1052 = 14 bsl 4 + (_@1050 bsr 4),
                           _@1053 =
                               2 bsl 6 + (_@1050 band 15 bsl 2)
                               +
                               (_@1051 bsr 6),
                           _@1054 = 2 bsl 6 + _@1051 band 63,
                           [_@1049, _@1052, _@1053, _@1054]
                       end
                   end,
                   0);
        14385 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1055 = Acc,
                       _@1056 = 129,
                       _@1057 = Last,
                       begin
                           _@1058 = 14 bsl 4 + (_@1056 bsr 4),
                           _@1059 =
                               2 bsl 6 + (_@1056 band 15 bsl 2)
                               +
                               (_@1057 bsr 6),
                           _@1060 = 2 bsl 6 + _@1057 band 63,
                           [_@1055, _@1058, _@1059, _@1060]
                       end
                   end,
                   0);
        14386 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1061 = Acc,
                       _@1062 = 130,
                       _@1063 = Last,
                       begin
                           _@1064 = 14 bsl 4 + (_@1062 bsr 4),
                           _@1065 =
                               2 bsl 6 + (_@1062 band 15 bsl 2)
                               +
                               (_@1063 bsr 6),
                           _@1066 = 2 bsl 6 + _@1063 band 63,
                           [_@1061, _@1064, _@1065, _@1066]
                       end
                   end,
                   0);
        14387 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1067 = Acc,
                       _@1068 = 131,
                       _@1069 = Last,
                       begin
                           _@1070 = 14 bsl 4 + (_@1068 bsr 4),
                           _@1071 =
                               2 bsl 6 + (_@1068 band 15 bsl 2)
                               +
                               (_@1069 bsr 6),
                           _@1072 = 2 bsl 6 + _@1069 band 63,
                           [_@1067, _@1070, _@1071, _@1072]
                       end
                   end,
                   0);
        14388 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1073 = Acc,
                       _@1074 = 132,
                       _@1075 = Last,
                       begin
                           _@1076 = 14 bsl 4 + (_@1074 bsr 4),
                           _@1077 =
                               2 bsl 6 + (_@1074 band 15 bsl 2)
                               +
                               (_@1075 bsr 6),
                           _@1078 = 2 bsl 6 + _@1075 band 63,
                           [_@1073, _@1076, _@1077, _@1078]
                       end
                   end,
                   0);
        14389 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1079 = Acc,
                       _@1080 = 133,
                       _@1081 = Last,
                       begin
                           _@1082 = 14 bsl 4 + (_@1080 bsr 4),
                           _@1083 =
                               2 bsl 6 + (_@1080 band 15 bsl 2)
                               +
                               (_@1081 bsr 6),
                           _@1084 = 2 bsl 6 + _@1081 band 63,
                           [_@1079, _@1082, _@1083, _@1084]
                       end
                   end,
                   0);
        14390 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1085 = Acc,
                       _@1086 = 134,
                       _@1087 = Last,
                       begin
                           _@1088 = 14 bsl 4 + (_@1086 bsr 4),
                           _@1089 =
                               2 bsl 6 + (_@1086 band 15 bsl 2)
                               +
                               (_@1087 bsr 6),
                           _@1090 = 2 bsl 6 + _@1087 band 63,
                           [_@1085, _@1088, _@1089, _@1090]
                       end
                   end,
                   0);
        14391 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1091 = Acc,
                       _@1092 = 135,
                       _@1093 = Last,
                       begin
                           _@1094 = 14 bsl 4 + (_@1092 bsr 4),
                           _@1095 =
                               2 bsl 6 + (_@1092 band 15 bsl 2)
                               +
                               (_@1093 bsr 6),
                           _@1096 = 2 bsl 6 + _@1093 band 63,
                           [_@1091, _@1094, _@1095, _@1096]
                       end
                   end,
                   0);
        14392 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1097 = Acc,
                       _@1098 = 136,
                       _@1099 = Last,
                       begin
                           _@1100 = 14 bsl 4 + (_@1098 bsr 4),
                           _@1101 =
                               2 bsl 6 + (_@1098 band 15 bsl 2)
                               +
                               (_@1099 bsr 6),
                           _@1102 = 2 bsl 6 + _@1099 band 63,
                           [_@1097, _@1100, _@1101, _@1102]
                       end
                   end,
                   0);
        14393 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1103 = Acc,
                       _@1104 = 137,
                       _@1105 = Last,
                       begin
                           _@1106 = 14 bsl 4 + (_@1104 bsr 4),
                           _@1107 =
                               2 bsl 6 + (_@1104 band 15 bsl 2)
                               +
                               (_@1105 bsr 6),
                           _@1108 = 2 bsl 6 + _@1105 band 63,
                           [_@1103, _@1106, _@1107, _@1108]
                       end
                   end,
                   0);
        14401 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1109 = Acc,
                       _@1110 = 138,
                       _@1111 = Last,
                       begin
                           _@1112 = 14 bsl 4 + (_@1110 bsr 4),
                           _@1113 =
                               2 bsl 6 + (_@1110 band 15 bsl 2)
                               +
                               (_@1111 bsr 6),
                           _@1114 = 2 bsl 6 + _@1111 band 63,
                           [_@1109, _@1112, _@1113, _@1114]
                       end
                   end,
                   0);
        14402 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1115 = Acc,
                       _@1116 = 139,
                       _@1117 = Last,
                       begin
                           _@1118 = 14 bsl 4 + (_@1116 bsr 4),
                           _@1119 =
                               2 bsl 6 + (_@1116 band 15 bsl 2)
                               +
                               (_@1117 bsr 6),
                           _@1120 = 2 bsl 6 + _@1117 band 63,
                           [_@1115, _@1118, _@1119, _@1120]
                       end
                   end,
                   0);
        14403 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1121 = Acc,
                       _@1122 = 140,
                       _@1123 = Last,
                       begin
                           _@1124 = 14 bsl 4 + (_@1122 bsr 4),
                           _@1125 =
                               2 bsl 6 + (_@1122 band 15 bsl 2)
                               +
                               (_@1123 bsr 6),
                           _@1126 = 2 bsl 6 + _@1123 band 63,
                           [_@1121, _@1124, _@1125, _@1126]
                       end
                   end,
                   0);
        14404 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1127 = Acc,
                       _@1128 = 141,
                       _@1129 = Last,
                       begin
                           _@1130 = 14 bsl 4 + (_@1128 bsr 4),
                           _@1131 =
                               2 bsl 6 + (_@1128 band 15 bsl 2)
                               +
                               (_@1129 bsr 6),
                           _@1132 = 2 bsl 6 + _@1129 band 63,
                           [_@1127, _@1130, _@1131, _@1132]
                       end
                   end,
                   0);
        14405 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1133 = Acc,
                       _@1134 = 142,
                       _@1135 = Last,
                       begin
                           _@1136 = 14 bsl 4 + (_@1134 bsr 4),
                           _@1137 =
                               2 bsl 6 + (_@1134 band 15 bsl 2)
                               +
                               (_@1135 bsr 6),
                           _@1138 = 2 bsl 6 + _@1135 band 63,
                           [_@1133, _@1136, _@1137, _@1138]
                       end
                   end,
                   0);
        14406 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1139 = Acc,
                       _@1140 = 143,
                       _@1141 = Last,
                       begin
                           _@1142 = 14 bsl 4 + (_@1140 bsr 4),
                           _@1143 =
                               2 bsl 6 + (_@1140 band 15 bsl 2)
                               +
                               (_@1141 bsr 6),
                           _@1144 = 2 bsl 6 + _@1141 band 63,
                           [_@1139, _@1142, _@1143, _@1144]
                       end
                   end,
                   0);
        14433 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1145 = Acc,
                       _@1146 = 138,
                       _@1147 = Last,
                       begin
                           _@1148 = 14 bsl 4 + (_@1146 bsr 4),
                           _@1149 =
                               2 bsl 6 + (_@1146 band 15 bsl 2)
                               +
                               (_@1147 bsr 6),
                           _@1150 = 2 bsl 6 + _@1147 band 63,
                           [_@1145, _@1148, _@1149, _@1150]
                       end
                   end,
                   0);
        14434 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1151 = Acc,
                       _@1152 = 139,
                       _@1153 = Last,
                       begin
                           _@1154 = 14 bsl 4 + (_@1152 bsr 4),
                           _@1155 =
                               2 bsl 6 + (_@1152 band 15 bsl 2)
                               +
                               (_@1153 bsr 6),
                           _@1156 = 2 bsl 6 + _@1153 band 63,
                           [_@1151, _@1154, _@1155, _@1156]
                       end
                   end,
                   0);
        14435 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1157 = Acc,
                       _@1158 = 140,
                       _@1159 = Last,
                       begin
                           _@1160 = 14 bsl 4 + (_@1158 bsr 4),
                           _@1161 =
                               2 bsl 6 + (_@1158 band 15 bsl 2)
                               +
                               (_@1159 bsr 6),
                           _@1162 = 2 bsl 6 + _@1159 band 63,
                           [_@1157, _@1160, _@1161, _@1162]
                       end
                   end,
                   0);
        14436 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1163 = Acc,
                       _@1164 = 141,
                       _@1165 = Last,
                       begin
                           _@1166 = 14 bsl 4 + (_@1164 bsr 4),
                           _@1167 =
                               2 bsl 6 + (_@1164 band 15 bsl 2)
                               +
                               (_@1165 bsr 6),
                           _@1168 = 2 bsl 6 + _@1165 band 63,
                           [_@1163, _@1166, _@1167, _@1168]
                       end
                   end,
                   0);
        14437 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1169 = Acc,
                       _@1170 = 142,
                       _@1171 = Last,
                       begin
                           _@1172 = 14 bsl 4 + (_@1170 bsr 4),
                           _@1173 =
                               2 bsl 6 + (_@1170 band 15 bsl 2)
                               +
                               (_@1171 bsr 6),
                           _@1174 = 2 bsl 6 + _@1171 band 63,
                           [_@1169, _@1172, _@1173, _@1174]
                       end
                   end,
                   0);
        14438 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1175 = Acc,
                       _@1176 = 143,
                       _@1177 = Last,
                       begin
                           _@1178 = 14 bsl 4 + (_@1176 bsr 4),
                           _@1179 =
                               2 bsl 6 + (_@1176 band 15 bsl 2)
                               +
                               (_@1177 bsr 6),
                           _@1180 = 2 bsl 6 + _@1177 band 63,
                           [_@1175, _@1178, _@1179, _@1180]
                       end
                   end,
                   0);
        14640 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1181 = Acc,
                       _@1182 = 144,
                       _@1183 = Last,
                       begin
                           _@1184 = 14 bsl 4 + (_@1182 bsr 4),
                           _@1185 =
                               2 bsl 6 + (_@1182 band 15 bsl 2)
                               +
                               (_@1183 bsr 6),
                           _@1186 = 2 bsl 6 + _@1183 band 63,
                           [_@1181, _@1184, _@1185, _@1186]
                       end
                   end,
                   0);
        14641 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1187 = Acc,
                       _@1188 = 145,
                       _@1189 = Last,
                       begin
                           _@1190 = 14 bsl 4 + (_@1188 bsr 4),
                           _@1191 =
                               2 bsl 6 + (_@1188 band 15 bsl 2)
                               +
                               (_@1189 bsr 6),
                           _@1192 = 2 bsl 6 + _@1189 band 63,
                           [_@1187, _@1190, _@1191, _@1192]
                       end
                   end,
                   0);
        14642 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1193 = Acc,
                       _@1194 = 146,
                       _@1195 = Last,
                       begin
                           _@1196 = 14 bsl 4 + (_@1194 bsr 4),
                           _@1197 =
                               2 bsl 6 + (_@1194 band 15 bsl 2)
                               +
                               (_@1195 bsr 6),
                           _@1198 = 2 bsl 6 + _@1195 band 63,
                           [_@1193, _@1196, _@1197, _@1198]
                       end
                   end,
                   0);
        14643 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1199 = Acc,
                       _@1200 = 147,
                       _@1201 = Last,
                       begin
                           _@1202 = 14 bsl 4 + (_@1200 bsr 4),
                           _@1203 =
                               2 bsl 6 + (_@1200 band 15 bsl 2)
                               +
                               (_@1201 bsr 6),
                           _@1204 = 2 bsl 6 + _@1201 band 63,
                           [_@1199, _@1202, _@1203, _@1204]
                       end
                   end,
                   0);
        14644 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1205 = Acc,
                       _@1206 = 148,
                       _@1207 = Last,
                       begin
                           _@1208 = 14 bsl 4 + (_@1206 bsr 4),
                           _@1209 =
                               2 bsl 6 + (_@1206 band 15 bsl 2)
                               +
                               (_@1207 bsr 6),
                           _@1210 = 2 bsl 6 + _@1207 band 63,
                           [_@1205, _@1208, _@1209, _@1210]
                       end
                   end,
                   0);
        14645 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1211 = Acc,
                       _@1212 = 149,
                       _@1213 = Last,
                       begin
                           _@1214 = 14 bsl 4 + (_@1212 bsr 4),
                           _@1215 =
                               2 bsl 6 + (_@1212 band 15 bsl 2)
                               +
                               (_@1213 bsr 6),
                           _@1216 = 2 bsl 6 + _@1213 band 63,
                           [_@1211, _@1214, _@1215, _@1216]
                       end
                   end,
                   0);
        14646 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1217 = Acc,
                       _@1218 = 150,
                       _@1219 = Last,
                       begin
                           _@1220 = 14 bsl 4 + (_@1218 bsr 4),
                           _@1221 =
                               2 bsl 6 + (_@1218 band 15 bsl 2)
                               +
                               (_@1219 bsr 6),
                           _@1222 = 2 bsl 6 + _@1219 band 63,
                           [_@1217, _@1220, _@1221, _@1222]
                       end
                   end,
                   0);
        14647 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1223 = Acc,
                       _@1224 = 151,
                       _@1225 = Last,
                       begin
                           _@1226 = 14 bsl 4 + (_@1224 bsr 4),
                           _@1227 =
                               2 bsl 6 + (_@1224 band 15 bsl 2)
                               +
                               (_@1225 bsr 6),
                           _@1228 = 2 bsl 6 + _@1225 band 63,
                           [_@1223, _@1226, _@1227, _@1228]
                       end
                   end,
                   0);
        14648 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1229 = Acc,
                       _@1230 = 152,
                       _@1231 = Last,
                       begin
                           _@1232 = 14 bsl 4 + (_@1230 bsr 4),
                           _@1233 =
                               2 bsl 6 + (_@1230 band 15 bsl 2)
                               +
                               (_@1231 bsr 6),
                           _@1234 = 2 bsl 6 + _@1231 band 63,
                           [_@1229, _@1232, _@1233, _@1234]
                       end
                   end,
                   0);
        14649 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1235 = Acc,
                       _@1236 = 153,
                       _@1237 = Last,
                       begin
                           _@1238 = 14 bsl 4 + (_@1236 bsr 4),
                           _@1239 =
                               2 bsl 6 + (_@1236 band 15 bsl 2)
                               +
                               (_@1237 bsr 6),
                           _@1240 = 2 bsl 6 + _@1237 band 63,
                           [_@1235, _@1238, _@1239, _@1240]
                       end
                   end,
                   0);
        14657 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1241 = Acc,
                       _@1242 = 154,
                       _@1243 = Last,
                       begin
                           _@1244 = 14 bsl 4 + (_@1242 bsr 4),
                           _@1245 =
                               2 bsl 6 + (_@1242 band 15 bsl 2)
                               +
                               (_@1243 bsr 6),
                           _@1246 = 2 bsl 6 + _@1243 band 63,
                           [_@1241, _@1244, _@1245, _@1246]
                       end
                   end,
                   0);
        14658 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1247 = Acc,
                       _@1248 = 155,
                       _@1249 = Last,
                       begin
                           _@1250 = 14 bsl 4 + (_@1248 bsr 4),
                           _@1251 =
                               2 bsl 6 + (_@1248 band 15 bsl 2)
                               +
                               (_@1249 bsr 6),
                           _@1252 = 2 bsl 6 + _@1249 band 63,
                           [_@1247, _@1250, _@1251, _@1252]
                       end
                   end,
                   0);
        14659 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1253 = Acc,
                       _@1254 = 156,
                       _@1255 = Last,
                       begin
                           _@1256 = 14 bsl 4 + (_@1254 bsr 4),
                           _@1257 =
                               2 bsl 6 + (_@1254 band 15 bsl 2)
                               +
                               (_@1255 bsr 6),
                           _@1258 = 2 bsl 6 + _@1255 band 63,
                           [_@1253, _@1256, _@1257, _@1258]
                       end
                   end,
                   0);
        14660 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1259 = Acc,
                       _@1260 = 157,
                       _@1261 = Last,
                       begin
                           _@1262 = 14 bsl 4 + (_@1260 bsr 4),
                           _@1263 =
                               2 bsl 6 + (_@1260 band 15 bsl 2)
                               +
                               (_@1261 bsr 6),
                           _@1264 = 2 bsl 6 + _@1261 band 63,
                           [_@1259, _@1262, _@1263, _@1264]
                       end
                   end,
                   0);
        14661 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1265 = Acc,
                       _@1266 = 158,
                       _@1267 = Last,
                       begin
                           _@1268 = 14 bsl 4 + (_@1266 bsr 4),
                           _@1269 =
                               2 bsl 6 + (_@1266 band 15 bsl 2)
                               +
                               (_@1267 bsr 6),
                           _@1270 = 2 bsl 6 + _@1267 band 63,
                           [_@1265, _@1268, _@1269, _@1270]
                       end
                   end,
                   0);
        14662 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1271 = Acc,
                       _@1272 = 159,
                       _@1273 = Last,
                       begin
                           _@1274 = 14 bsl 4 + (_@1272 bsr 4),
                           _@1275 =
                               2 bsl 6 + (_@1272 band 15 bsl 2)
                               +
                               (_@1273 bsr 6),
                           _@1276 = 2 bsl 6 + _@1273 band 63,
                           [_@1271, _@1274, _@1275, _@1276]
                       end
                   end,
                   0);
        14689 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1277 = Acc,
                       _@1278 = 154,
                       _@1279 = Last,
                       begin
                           _@1280 = 14 bsl 4 + (_@1278 bsr 4),
                           _@1281 =
                               2 bsl 6 + (_@1278 band 15 bsl 2)
                               +
                               (_@1279 bsr 6),
                           _@1282 = 2 bsl 6 + _@1279 band 63,
                           [_@1277, _@1280, _@1281, _@1282]
                       end
                   end,
                   0);
        14690 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1283 = Acc,
                       _@1284 = 155,
                       _@1285 = Last,
                       begin
                           _@1286 = 14 bsl 4 + (_@1284 bsr 4),
                           _@1287 =
                               2 bsl 6 + (_@1284 band 15 bsl 2)
                               +
                               (_@1285 bsr 6),
                           _@1288 = 2 bsl 6 + _@1285 band 63,
                           [_@1283, _@1286, _@1287, _@1288]
                       end
                   end,
                   0);
        14691 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1289 = Acc,
                       _@1290 = 156,
                       _@1291 = Last,
                       begin
                           _@1292 = 14 bsl 4 + (_@1290 bsr 4),
                           _@1293 =
                               2 bsl 6 + (_@1290 band 15 bsl 2)
                               +
                               (_@1291 bsr 6),
                           _@1294 = 2 bsl 6 + _@1291 band 63,
                           [_@1289, _@1292, _@1293, _@1294]
                       end
                   end,
                   0);
        14692 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1295 = Acc,
                       _@1296 = 157,
                       _@1297 = Last,
                       begin
                           _@1298 = 14 bsl 4 + (_@1296 bsr 4),
                           _@1299 =
                               2 bsl 6 + (_@1296 band 15 bsl 2)
                               +
                               (_@1297 bsr 6),
                           _@1300 = 2 bsl 6 + _@1297 band 63,
                           [_@1295, _@1298, _@1299, _@1300]
                       end
                   end,
                   0);
        14693 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1301 = Acc,
                       _@1302 = 158,
                       _@1303 = Last,
                       begin
                           _@1304 = 14 bsl 4 + (_@1302 bsr 4),
                           _@1305 =
                               2 bsl 6 + (_@1302 band 15 bsl 2)
                               +
                               (_@1303 bsr 6),
                           _@1306 = 2 bsl 6 + _@1303 band 63,
                           [_@1301, _@1304, _@1305, _@1306]
                       end
                   end,
                   0);
        14694 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1307 = Acc,
                       _@1308 = 159,
                       _@1309 = Last,
                       begin
                           _@1310 = 14 bsl 4 + (_@1308 bsr 4),
                           _@1311 =
                               2 bsl 6 + (_@1308 band 15 bsl 2)
                               +
                               (_@1309 bsr 6),
                           _@1312 = 2 bsl 6 + _@1309 band 63,
                           [_@1307, _@1310, _@1311, _@1312]
                       end
                   end,
                   0);
        16688 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1313 = Acc,
                       _@1314 = 160,
                       _@1315 = Last,
                       begin
                           _@1316 = 14 bsl 4 + (_@1314 bsr 4),
                           _@1317 =
                               2 bsl 6 + (_@1314 band 15 bsl 2)
                               +
                               (_@1315 bsr 6),
                           _@1318 = 2 bsl 6 + _@1315 band 63,
                           [_@1313, _@1316, _@1317, _@1318]
                       end
                   end,
                   0);
        16689 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1319 = Acc,
                       _@1320 = 161,
                       _@1321 = Last,
                       begin
                           _@1322 = 14 bsl 4 + (_@1320 bsr 4),
                           _@1323 =
                               2 bsl 6 + (_@1320 band 15 bsl 2)
                               +
                               (_@1321 bsr 6),
                           _@1324 = 2 bsl 6 + _@1321 band 63,
                           [_@1319, _@1322, _@1323, _@1324]
                       end
                   end,
                   0);
        16690 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1325 = Acc,
                       _@1326 = 162,
                       _@1327 = Last,
                       begin
                           _@1328 = 14 bsl 4 + (_@1326 bsr 4),
                           _@1329 =
                               2 bsl 6 + (_@1326 band 15 bsl 2)
                               +
                               (_@1327 bsr 6),
                           _@1330 = 2 bsl 6 + _@1327 band 63,
                           [_@1325, _@1328, _@1329, _@1330]
                       end
                   end,
                   0);
        16691 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1331 = Acc,
                       _@1332 = 163,
                       _@1333 = Last,
                       begin
                           _@1334 = 14 bsl 4 + (_@1332 bsr 4),
                           _@1335 =
                               2 bsl 6 + (_@1332 band 15 bsl 2)
                               +
                               (_@1333 bsr 6),
                           _@1336 = 2 bsl 6 + _@1333 band 63,
                           [_@1331, _@1334, _@1335, _@1336]
                       end
                   end,
                   0);
        16692 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1337 = Acc,
                       _@1338 = 164,
                       _@1339 = Last,
                       begin
                           _@1340 = 14 bsl 4 + (_@1338 bsr 4),
                           _@1341 =
                               2 bsl 6 + (_@1338 band 15 bsl 2)
                               +
                               (_@1339 bsr 6),
                           _@1342 = 2 bsl 6 + _@1339 band 63,
                           [_@1337, _@1340, _@1341, _@1342]
                       end
                   end,
                   0);
        16693 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1343 = Acc,
                       _@1344 = 165,
                       _@1345 = Last,
                       begin
                           _@1346 = 14 bsl 4 + (_@1344 bsr 4),
                           _@1347 =
                               2 bsl 6 + (_@1344 band 15 bsl 2)
                               +
                               (_@1345 bsr 6),
                           _@1348 = 2 bsl 6 + _@1345 band 63,
                           [_@1343, _@1346, _@1347, _@1348]
                       end
                   end,
                   0);
        16694 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1349 = Acc,
                       _@1350 = 166,
                       _@1351 = Last,
                       begin
                           _@1352 = 14 bsl 4 + (_@1350 bsr 4),
                           _@1353 =
                               2 bsl 6 + (_@1350 band 15 bsl 2)
                               +
                               (_@1351 bsr 6),
                           _@1354 = 2 bsl 6 + _@1351 band 63,
                           [_@1349, _@1352, _@1353, _@1354]
                       end
                   end,
                   0);
        16695 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1355 = Acc,
                       _@1356 = 167,
                       _@1357 = Last,
                       begin
                           _@1358 = 14 bsl 4 + (_@1356 bsr 4),
                           _@1359 =
                               2 bsl 6 + (_@1356 band 15 bsl 2)
                               +
                               (_@1357 bsr 6),
                           _@1360 = 2 bsl 6 + _@1357 band 63,
                           [_@1355, _@1358, _@1359, _@1360]
                       end
                   end,
                   0);
        16696 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1361 = Acc,
                       _@1362 = 168,
                       _@1363 = Last,
                       begin
                           _@1364 = 14 bsl 4 + (_@1362 bsr 4),
                           _@1365 =
                               2 bsl 6 + (_@1362 band 15 bsl 2)
                               +
                               (_@1363 bsr 6),
                           _@1366 = 2 bsl 6 + _@1363 band 63,
                           [_@1361, _@1364, _@1365, _@1366]
                       end
                   end,
                   0);
        16697 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1367 = Acc,
                       _@1368 = 169,
                       _@1369 = Last,
                       begin
                           _@1370 = 14 bsl 4 + (_@1368 bsr 4),
                           _@1371 =
                               2 bsl 6 + (_@1368 band 15 bsl 2)
                               +
                               (_@1369 bsr 6),
                           _@1372 = 2 bsl 6 + _@1369 band 63,
                           [_@1367, _@1370, _@1371, _@1372]
                       end
                   end,
                   0);
        16705 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1373 = Acc,
                       _@1374 = 170,
                       _@1375 = Last,
                       begin
                           _@1376 = 14 bsl 4 + (_@1374 bsr 4),
                           _@1377 =
                               2 bsl 6 + (_@1374 band 15 bsl 2)
                               +
                               (_@1375 bsr 6),
                           _@1378 = 2 bsl 6 + _@1375 band 63,
                           [_@1373, _@1376, _@1377, _@1378]
                       end
                   end,
                   0);
        16706 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1379 = Acc,
                       _@1380 = 171,
                       _@1381 = Last,
                       begin
                           _@1382 = 14 bsl 4 + (_@1380 bsr 4),
                           _@1383 =
                               2 bsl 6 + (_@1380 band 15 bsl 2)
                               +
                               (_@1381 bsr 6),
                           _@1384 = 2 bsl 6 + _@1381 band 63,
                           [_@1379, _@1382, _@1383, _@1384]
                       end
                   end,
                   0);
        16707 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1385 = Acc,
                       _@1386 = 172,
                       _@1387 = Last,
                       begin
                           _@1388 = 14 bsl 4 + (_@1386 bsr 4),
                           _@1389 =
                               2 bsl 6 + (_@1386 band 15 bsl 2)
                               +
                               (_@1387 bsr 6),
                           _@1390 = 2 bsl 6 + _@1387 band 63,
                           [_@1385, _@1388, _@1389, _@1390]
                       end
                   end,
                   0);
        16708 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1391 = Acc,
                       _@1392 = 173,
                       _@1393 = Last,
                       begin
                           _@1394 = 14 bsl 4 + (_@1392 bsr 4),
                           _@1395 =
                               2 bsl 6 + (_@1392 band 15 bsl 2)
                               +
                               (_@1393 bsr 6),
                           _@1396 = 2 bsl 6 + _@1393 band 63,
                           [_@1391, _@1394, _@1395, _@1396]
                       end
                   end,
                   0);
        16709 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1397 = Acc,
                       _@1398 = 174,
                       _@1399 = Last,
                       begin
                           _@1400 = 14 bsl 4 + (_@1398 bsr 4),
                           _@1401 =
                               2 bsl 6 + (_@1398 band 15 bsl 2)
                               +
                               (_@1399 bsr 6),
                           _@1402 = 2 bsl 6 + _@1399 band 63,
                           [_@1397, _@1400, _@1401, _@1402]
                       end
                   end,
                   0);
        16710 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1403 = Acc,
                       _@1404 = 175,
                       _@1405 = Last,
                       begin
                           _@1406 = 14 bsl 4 + (_@1404 bsr 4),
                           _@1407 =
                               2 bsl 6 + (_@1404 band 15 bsl 2)
                               +
                               (_@1405 bsr 6),
                           _@1408 = 2 bsl 6 + _@1405 band 63,
                           [_@1403, _@1406, _@1407, _@1408]
                       end
                   end,
                   0);
        16737 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1409 = Acc,
                       _@1410 = 170,
                       _@1411 = Last,
                       begin
                           _@1412 = 14 bsl 4 + (_@1410 bsr 4),
                           _@1413 =
                               2 bsl 6 + (_@1410 band 15 bsl 2)
                               +
                               (_@1411 bsr 6),
                           _@1414 = 2 bsl 6 + _@1411 band 63,
                           [_@1409, _@1412, _@1413, _@1414]
                       end
                   end,
                   0);
        16738 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1415 = Acc,
                       _@1416 = 171,
                       _@1417 = Last,
                       begin
                           _@1418 = 14 bsl 4 + (_@1416 bsr 4),
                           _@1419 =
                               2 bsl 6 + (_@1416 band 15 bsl 2)
                               +
                               (_@1417 bsr 6),
                           _@1420 = 2 bsl 6 + _@1417 band 63,
                           [_@1415, _@1418, _@1419, _@1420]
                       end
                   end,
                   0);
        16739 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1421 = Acc,
                       _@1422 = 172,
                       _@1423 = Last,
                       begin
                           _@1424 = 14 bsl 4 + (_@1422 bsr 4),
                           _@1425 =
                               2 bsl 6 + (_@1422 band 15 bsl 2)
                               +
                               (_@1423 bsr 6),
                           _@1426 = 2 bsl 6 + _@1423 band 63,
                           [_@1421, _@1424, _@1425, _@1426]
                       end
                   end,
                   0);
        16740 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1427 = Acc,
                       _@1428 = 173,
                       _@1429 = Last,
                       begin
                           _@1430 = 14 bsl 4 + (_@1428 bsr 4),
                           _@1431 =
                               2 bsl 6 + (_@1428 band 15 bsl 2)
                               +
                               (_@1429 bsr 6),
                           _@1432 = 2 bsl 6 + _@1429 band 63,
                           [_@1427, _@1430, _@1431, _@1432]
                       end
                   end,
                   0);
        16741 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1433 = Acc,
                       _@1434 = 174,
                       _@1435 = Last,
                       begin
                           _@1436 = 14 bsl 4 + (_@1434 bsr 4),
                           _@1437 =
                               2 bsl 6 + (_@1434 band 15 bsl 2)
                               +
                               (_@1435 bsr 6),
                           _@1438 = 2 bsl 6 + _@1435 band 63,
                           [_@1433, _@1436, _@1437, _@1438]
                       end
                   end,
                   0);
        16742 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1439 = Acc,
                       _@1440 = 175,
                       _@1441 = Last,
                       begin
                           _@1442 = 14 bsl 4 + (_@1440 bsr 4),
                           _@1443 =
                               2 bsl 6 + (_@1440 band 15 bsl 2)
                               +
                               (_@1441 bsr 6),
                           _@1444 = 2 bsl 6 + _@1441 band 63,
                           [_@1439, _@1442, _@1443, _@1444]
                       end
                   end,
                   0);
        16944 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1445 = Acc,
                       _@1446 = 176,
                       _@1447 = Last,
                       begin
                           _@1448 = 14 bsl 4 + (_@1446 bsr 4),
                           _@1449 =
                               2 bsl 6 + (_@1446 band 15 bsl 2)
                               +
                               (_@1447 bsr 6),
                           _@1450 = 2 bsl 6 + _@1447 band 63,
                           [_@1445, _@1448, _@1449, _@1450]
                       end
                   end,
                   0);
        16945 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1451 = Acc,
                       _@1452 = 177,
                       _@1453 = Last,
                       begin
                           _@1454 = 14 bsl 4 + (_@1452 bsr 4),
                           _@1455 =
                               2 bsl 6 + (_@1452 band 15 bsl 2)
                               +
                               (_@1453 bsr 6),
                           _@1456 = 2 bsl 6 + _@1453 band 63,
                           [_@1451, _@1454, _@1455, _@1456]
                       end
                   end,
                   0);
        16946 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1457 = Acc,
                       _@1458 = 178,
                       _@1459 = Last,
                       begin
                           _@1460 = 14 bsl 4 + (_@1458 bsr 4),
                           _@1461 =
                               2 bsl 6 + (_@1458 band 15 bsl 2)
                               +
                               (_@1459 bsr 6),
                           _@1462 = 2 bsl 6 + _@1459 band 63,
                           [_@1457, _@1460, _@1461, _@1462]
                       end
                   end,
                   0);
        16947 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1463 = Acc,
                       _@1464 = 179,
                       _@1465 = Last,
                       begin
                           _@1466 = 14 bsl 4 + (_@1464 bsr 4),
                           _@1467 =
                               2 bsl 6 + (_@1464 band 15 bsl 2)
                               +
                               (_@1465 bsr 6),
                           _@1468 = 2 bsl 6 + _@1465 band 63,
                           [_@1463, _@1466, _@1467, _@1468]
                       end
                   end,
                   0);
        16948 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1469 = Acc,
                       _@1470 = 180,
                       _@1471 = Last,
                       begin
                           _@1472 = 14 bsl 4 + (_@1470 bsr 4),
                           _@1473 =
                               2 bsl 6 + (_@1470 band 15 bsl 2)
                               +
                               (_@1471 bsr 6),
                           _@1474 = 2 bsl 6 + _@1471 band 63,
                           [_@1469, _@1472, _@1473, _@1474]
                       end
                   end,
                   0);
        16949 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1475 = Acc,
                       _@1476 = 181,
                       _@1477 = Last,
                       begin
                           _@1478 = 14 bsl 4 + (_@1476 bsr 4),
                           _@1479 =
                               2 bsl 6 + (_@1476 band 15 bsl 2)
                               +
                               (_@1477 bsr 6),
                           _@1480 = 2 bsl 6 + _@1477 band 63,
                           [_@1475, _@1478, _@1479, _@1480]
                       end
                   end,
                   0);
        16950 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1481 = Acc,
                       _@1482 = 182,
                       _@1483 = Last,
                       begin
                           _@1484 = 14 bsl 4 + (_@1482 bsr 4),
                           _@1485 =
                               2 bsl 6 + (_@1482 band 15 bsl 2)
                               +
                               (_@1483 bsr 6),
                           _@1486 = 2 bsl 6 + _@1483 band 63,
                           [_@1481, _@1484, _@1485, _@1486]
                       end
                   end,
                   0);
        16951 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1487 = Acc,
                       _@1488 = 183,
                       _@1489 = Last,
                       begin
                           _@1490 = 14 bsl 4 + (_@1488 bsr 4),
                           _@1491 =
                               2 bsl 6 + (_@1488 band 15 bsl 2)
                               +
                               (_@1489 bsr 6),
                           _@1492 = 2 bsl 6 + _@1489 band 63,
                           [_@1487, _@1490, _@1491, _@1492]
                       end
                   end,
                   0);
        16952 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1493 = Acc,
                       _@1494 = 184,
                       _@1495 = Last,
                       begin
                           _@1496 = 14 bsl 4 + (_@1494 bsr 4),
                           _@1497 =
                               2 bsl 6 + (_@1494 band 15 bsl 2)
                               +
                               (_@1495 bsr 6),
                           _@1498 = 2 bsl 6 + _@1495 band 63,
                           [_@1493, _@1496, _@1497, _@1498]
                       end
                   end,
                   0);
        16953 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1499 = Acc,
                       _@1500 = 185,
                       _@1501 = Last,
                       begin
                           _@1502 = 14 bsl 4 + (_@1500 bsr 4),
                           _@1503 =
                               2 bsl 6 + (_@1500 band 15 bsl 2)
                               +
                               (_@1501 bsr 6),
                           _@1504 = 2 bsl 6 + _@1501 band 63,
                           [_@1499, _@1502, _@1503, _@1504]
                       end
                   end,
                   0);
        16961 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1505 = Acc,
                       _@1506 = 186,
                       _@1507 = Last,
                       begin
                           _@1508 = 14 bsl 4 + (_@1506 bsr 4),
                           _@1509 =
                               2 bsl 6 + (_@1506 band 15 bsl 2)
                               +
                               (_@1507 bsr 6),
                           _@1510 = 2 bsl 6 + _@1507 band 63,
                           [_@1505, _@1508, _@1509, _@1510]
                       end
                   end,
                   0);
        16962 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1511 = Acc,
                       _@1512 = 187,
                       _@1513 = Last,
                       begin
                           _@1514 = 14 bsl 4 + (_@1512 bsr 4),
                           _@1515 =
                               2 bsl 6 + (_@1512 band 15 bsl 2)
                               +
                               (_@1513 bsr 6),
                           _@1516 = 2 bsl 6 + _@1513 band 63,
                           [_@1511, _@1514, _@1515, _@1516]
                       end
                   end,
                   0);
        16963 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1517 = Acc,
                       _@1518 = 188,
                       _@1519 = Last,
                       begin
                           _@1520 = 14 bsl 4 + (_@1518 bsr 4),
                           _@1521 =
                               2 bsl 6 + (_@1518 band 15 bsl 2)
                               +
                               (_@1519 bsr 6),
                           _@1522 = 2 bsl 6 + _@1519 band 63,
                           [_@1517, _@1520, _@1521, _@1522]
                       end
                   end,
                   0);
        16964 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1523 = Acc,
                       _@1524 = 189,
                       _@1525 = Last,
                       begin
                           _@1526 = 14 bsl 4 + (_@1524 bsr 4),
                           _@1527 =
                               2 bsl 6 + (_@1524 band 15 bsl 2)
                               +
                               (_@1525 bsr 6),
                           _@1528 = 2 bsl 6 + _@1525 band 63,
                           [_@1523, _@1526, _@1527, _@1528]
                       end
                   end,
                   0);
        16965 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1529 = Acc,
                       _@1530 = 190,
                       _@1531 = Last,
                       begin
                           _@1532 = 14 bsl 4 + (_@1530 bsr 4),
                           _@1533 =
                               2 bsl 6 + (_@1530 band 15 bsl 2)
                               +
                               (_@1531 bsr 6),
                           _@1534 = 2 bsl 6 + _@1531 band 63,
                           [_@1529, _@1532, _@1533, _@1534]
                       end
                   end,
                   0);
        16966 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1535 = Acc,
                       _@1536 = 191,
                       _@1537 = Last,
                       begin
                           _@1538 = 14 bsl 4 + (_@1536 bsr 4),
                           _@1539 =
                               2 bsl 6 + (_@1536 band 15 bsl 2)
                               +
                               (_@1537 bsr 6),
                           _@1540 = 2 bsl 6 + _@1537 band 63,
                           [_@1535, _@1538, _@1539, _@1540]
                       end
                   end,
                   0);
        16993 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1541 = Acc,
                       _@1542 = 186,
                       _@1543 = Last,
                       begin
                           _@1544 = 14 bsl 4 + (_@1542 bsr 4),
                           _@1545 =
                               2 bsl 6 + (_@1542 band 15 bsl 2)
                               +
                               (_@1543 bsr 6),
                           _@1546 = 2 bsl 6 + _@1543 band 63,
                           [_@1541, _@1544, _@1545, _@1546]
                       end
                   end,
                   0);
        16994 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1547 = Acc,
                       _@1548 = 187,
                       _@1549 = Last,
                       begin
                           _@1550 = 14 bsl 4 + (_@1548 bsr 4),
                           _@1551 =
                               2 bsl 6 + (_@1548 band 15 bsl 2)
                               +
                               (_@1549 bsr 6),
                           _@1552 = 2 bsl 6 + _@1549 band 63,
                           [_@1547, _@1550, _@1551, _@1552]
                       end
                   end,
                   0);
        16995 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1553 = Acc,
                       _@1554 = 188,
                       _@1555 = Last,
                       begin
                           _@1556 = 14 bsl 4 + (_@1554 bsr 4),
                           _@1557 =
                               2 bsl 6 + (_@1554 band 15 bsl 2)
                               +
                               (_@1555 bsr 6),
                           _@1558 = 2 bsl 6 + _@1555 band 63,
                           [_@1553, _@1556, _@1557, _@1558]
                       end
                   end,
                   0);
        16996 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1559 = Acc,
                       _@1560 = 189,
                       _@1561 = Last,
                       begin
                           _@1562 = 14 bsl 4 + (_@1560 bsr 4),
                           _@1563 =
                               2 bsl 6 + (_@1560 band 15 bsl 2)
                               +
                               (_@1561 bsr 6),
                           _@1564 = 2 bsl 6 + _@1561 band 63,
                           [_@1559, _@1562, _@1563, _@1564]
                       end
                   end,
                   0);
        16997 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1565 = Acc,
                       _@1566 = 190,
                       _@1567 = Last,
                       begin
                           _@1568 = 14 bsl 4 + (_@1566 bsr 4),
                           _@1569 =
                               2 bsl 6 + (_@1566 band 15 bsl 2)
                               +
                               (_@1567 bsr 6),
                           _@1570 = 2 bsl 6 + _@1567 band 63,
                           [_@1565, _@1568, _@1569, _@1570]
                       end
                   end,
                   0);
        16998 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1571 = Acc,
                       _@1572 = 191,
                       _@1573 = Last,
                       begin
                           _@1574 = 14 bsl 4 + (_@1572 bsr 4),
                           _@1575 =
                               2 bsl 6 + (_@1572 band 15 bsl 2)
                               +
                               (_@1573 bsr 6),
                           _@1576 = 2 bsl 6 + _@1573 band 63,
                           [_@1571, _@1574, _@1575, _@1576]
                       end
                   end,
                   0);
        17200 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1577 = Acc,
                       _@1578 = 192,
                       _@1579 = Last,
                       begin
                           _@1580 = 14 bsl 4 + (_@1578 bsr 4),
                           _@1581 =
                               2 bsl 6 + (_@1578 band 15 bsl 2)
                               +
                               (_@1579 bsr 6),
                           _@1582 = 2 bsl 6 + _@1579 band 63,
                           [_@1577, _@1580, _@1581, _@1582]
                       end
                   end,
                   0);
        17201 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1583 = Acc,
                       _@1584 = 193,
                       _@1585 = Last,
                       begin
                           _@1586 = 14 bsl 4 + (_@1584 bsr 4),
                           _@1587 =
                               2 bsl 6 + (_@1584 band 15 bsl 2)
                               +
                               (_@1585 bsr 6),
                           _@1588 = 2 bsl 6 + _@1585 band 63,
                           [_@1583, _@1586, _@1587, _@1588]
                       end
                   end,
                   0);
        17202 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1589 = Acc,
                       _@1590 = 194,
                       _@1591 = Last,
                       begin
                           _@1592 = 14 bsl 4 + (_@1590 bsr 4),
                           _@1593 =
                               2 bsl 6 + (_@1590 band 15 bsl 2)
                               +
                               (_@1591 bsr 6),
                           _@1594 = 2 bsl 6 + _@1591 band 63,
                           [_@1589, _@1592, _@1593, _@1594]
                       end
                   end,
                   0);
        17203 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1595 = Acc,
                       _@1596 = 195,
                       _@1597 = Last,
                       begin
                           _@1598 = 14 bsl 4 + (_@1596 bsr 4),
                           _@1599 =
                               2 bsl 6 + (_@1596 band 15 bsl 2)
                               +
                               (_@1597 bsr 6),
                           _@1600 = 2 bsl 6 + _@1597 band 63,
                           [_@1595, _@1598, _@1599, _@1600]
                       end
                   end,
                   0);
        17204 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1601 = Acc,
                       _@1602 = 196,
                       _@1603 = Last,
                       begin
                           _@1604 = 14 bsl 4 + (_@1602 bsr 4),
                           _@1605 =
                               2 bsl 6 + (_@1602 band 15 bsl 2)
                               +
                               (_@1603 bsr 6),
                           _@1606 = 2 bsl 6 + _@1603 band 63,
                           [_@1601, _@1604, _@1605, _@1606]
                       end
                   end,
                   0);
        17205 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1607 = Acc,
                       _@1608 = 197,
                       _@1609 = Last,
                       begin
                           _@1610 = 14 bsl 4 + (_@1608 bsr 4),
                           _@1611 =
                               2 bsl 6 + (_@1608 band 15 bsl 2)
                               +
                               (_@1609 bsr 6),
                           _@1612 = 2 bsl 6 + _@1609 band 63,
                           [_@1607, _@1610, _@1611, _@1612]
                       end
                   end,
                   0);
        17206 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1613 = Acc,
                       _@1614 = 198,
                       _@1615 = Last,
                       begin
                           _@1616 = 14 bsl 4 + (_@1614 bsr 4),
                           _@1617 =
                               2 bsl 6 + (_@1614 band 15 bsl 2)
                               +
                               (_@1615 bsr 6),
                           _@1618 = 2 bsl 6 + _@1615 band 63,
                           [_@1613, _@1616, _@1617, _@1618]
                       end
                   end,
                   0);
        17207 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1619 = Acc,
                       _@1620 = 199,
                       _@1621 = Last,
                       begin
                           _@1622 = 14 bsl 4 + (_@1620 bsr 4),
                           _@1623 =
                               2 bsl 6 + (_@1620 band 15 bsl 2)
                               +
                               (_@1621 bsr 6),
                           _@1624 = 2 bsl 6 + _@1621 band 63,
                           [_@1619, _@1622, _@1623, _@1624]
                       end
                   end,
                   0);
        17208 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1625 = Acc,
                       _@1626 = 200,
                       _@1627 = Last,
                       begin
                           _@1628 = 14 bsl 4 + (_@1626 bsr 4),
                           _@1629 =
                               2 bsl 6 + (_@1626 band 15 bsl 2)
                               +
                               (_@1627 bsr 6),
                           _@1630 = 2 bsl 6 + _@1627 band 63,
                           [_@1625, _@1628, _@1629, _@1630]
                       end
                   end,
                   0);
        17209 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1631 = Acc,
                       _@1632 = 201,
                       _@1633 = Last,
                       begin
                           _@1634 = 14 bsl 4 + (_@1632 bsr 4),
                           _@1635 =
                               2 bsl 6 + (_@1632 band 15 bsl 2)
                               +
                               (_@1633 bsr 6),
                           _@1636 = 2 bsl 6 + _@1633 band 63,
                           [_@1631, _@1634, _@1635, _@1636]
                       end
                   end,
                   0);
        17217 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1637 = Acc,
                       _@1638 = 202,
                       _@1639 = Last,
                       begin
                           _@1640 = 14 bsl 4 + (_@1638 bsr 4),
                           _@1641 =
                               2 bsl 6 + (_@1638 band 15 bsl 2)
                               +
                               (_@1639 bsr 6),
                           _@1642 = 2 bsl 6 + _@1639 band 63,
                           [_@1637, _@1640, _@1641, _@1642]
                       end
                   end,
                   0);
        17218 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1643 = Acc,
                       _@1644 = 203,
                       _@1645 = Last,
                       begin
                           _@1646 = 14 bsl 4 + (_@1644 bsr 4),
                           _@1647 =
                               2 bsl 6 + (_@1644 band 15 bsl 2)
                               +
                               (_@1645 bsr 6),
                           _@1648 = 2 bsl 6 + _@1645 band 63,
                           [_@1643, _@1646, _@1647, _@1648]
                       end
                   end,
                   0);
        17219 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1649 = Acc,
                       _@1650 = 204,
                       _@1651 = Last,
                       begin
                           _@1652 = 14 bsl 4 + (_@1650 bsr 4),
                           _@1653 =
                               2 bsl 6 + (_@1650 band 15 bsl 2)
                               +
                               (_@1651 bsr 6),
                           _@1654 = 2 bsl 6 + _@1651 band 63,
                           [_@1649, _@1652, _@1653, _@1654]
                       end
                   end,
                   0);
        17220 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1655 = Acc,
                       _@1656 = 205,
                       _@1657 = Last,
                       begin
                           _@1658 = 14 bsl 4 + (_@1656 bsr 4),
                           _@1659 =
                               2 bsl 6 + (_@1656 band 15 bsl 2)
                               +
                               (_@1657 bsr 6),
                           _@1660 = 2 bsl 6 + _@1657 band 63,
                           [_@1655, _@1658, _@1659, _@1660]
                       end
                   end,
                   0);
        17221 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1661 = Acc,
                       _@1662 = 206,
                       _@1663 = Last,
                       begin
                           _@1664 = 14 bsl 4 + (_@1662 bsr 4),
                           _@1665 =
                               2 bsl 6 + (_@1662 band 15 bsl 2)
                               +
                               (_@1663 bsr 6),
                           _@1666 = 2 bsl 6 + _@1663 band 63,
                           [_@1661, _@1664, _@1665, _@1666]
                       end
                   end,
                   0);
        17222 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1667 = Acc,
                       _@1668 = 207,
                       _@1669 = Last,
                       begin
                           _@1670 = 14 bsl 4 + (_@1668 bsr 4),
                           _@1671 =
                               2 bsl 6 + (_@1668 band 15 bsl 2)
                               +
                               (_@1669 bsr 6),
                           _@1672 = 2 bsl 6 + _@1669 band 63,
                           [_@1667, _@1670, _@1671, _@1672]
                       end
                   end,
                   0);
        17249 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1673 = Acc,
                       _@1674 = 202,
                       _@1675 = Last,
                       begin
                           _@1676 = 14 bsl 4 + (_@1674 bsr 4),
                           _@1677 =
                               2 bsl 6 + (_@1674 band 15 bsl 2)
                               +
                               (_@1675 bsr 6),
                           _@1678 = 2 bsl 6 + _@1675 band 63,
                           [_@1673, _@1676, _@1677, _@1678]
                       end
                   end,
                   0);
        17250 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1679 = Acc,
                       _@1680 = 203,
                       _@1681 = Last,
                       begin
                           _@1682 = 14 bsl 4 + (_@1680 bsr 4),
                           _@1683 =
                               2 bsl 6 + (_@1680 band 15 bsl 2)
                               +
                               (_@1681 bsr 6),
                           _@1684 = 2 bsl 6 + _@1681 band 63,
                           [_@1679, _@1682, _@1683, _@1684]
                       end
                   end,
                   0);
        17251 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1685 = Acc,
                       _@1686 = 204,
                       _@1687 = Last,
                       begin
                           _@1688 = 14 bsl 4 + (_@1686 bsr 4),
                           _@1689 =
                               2 bsl 6 + (_@1686 band 15 bsl 2)
                               +
                               (_@1687 bsr 6),
                           _@1690 = 2 bsl 6 + _@1687 band 63,
                           [_@1685, _@1688, _@1689, _@1690]
                       end
                   end,
                   0);
        17252 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1691 = Acc,
                       _@1692 = 205,
                       _@1693 = Last,
                       begin
                           _@1694 = 14 bsl 4 + (_@1692 bsr 4),
                           _@1695 =
                               2 bsl 6 + (_@1692 band 15 bsl 2)
                               +
                               (_@1693 bsr 6),
                           _@1696 = 2 bsl 6 + _@1693 band 63,
                           [_@1691, _@1694, _@1695, _@1696]
                       end
                   end,
                   0);
        17253 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1697 = Acc,
                       _@1698 = 206,
                       _@1699 = Last,
                       begin
                           _@1700 = 14 bsl 4 + (_@1698 bsr 4),
                           _@1701 =
                               2 bsl 6 + (_@1698 band 15 bsl 2)
                               +
                               (_@1699 bsr 6),
                           _@1702 = 2 bsl 6 + _@1699 band 63,
                           [_@1697, _@1700, _@1701, _@1702]
                       end
                   end,
                   0);
        17254 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1703 = Acc,
                       _@1704 = 207,
                       _@1705 = Last,
                       begin
                           _@1706 = 14 bsl 4 + (_@1704 bsr 4),
                           _@1707 =
                               2 bsl 6 + (_@1704 band 15 bsl 2)
                               +
                               (_@1705 bsr 6),
                           _@1708 = 2 bsl 6 + _@1705 band 63,
                           [_@1703, _@1706, _@1707, _@1708]
                       end
                   end,
                   0);
        17456 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1709 = Acc,
                       _@1710 = 208,
                       _@1711 = Last,
                       begin
                           _@1712 = 14 bsl 4 + (_@1710 bsr 4),
                           _@1713 =
                               2 bsl 6 + (_@1710 band 15 bsl 2)
                               +
                               (_@1711 bsr 6),
                           _@1714 = 2 bsl 6 + _@1711 band 63,
                           [_@1709, _@1712, _@1713, _@1714]
                       end
                   end,
                   0);
        17457 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1715 = Acc,
                       _@1716 = 209,
                       _@1717 = Last,
                       begin
                           _@1718 = 14 bsl 4 + (_@1716 bsr 4),
                           _@1719 =
                               2 bsl 6 + (_@1716 band 15 bsl 2)
                               +
                               (_@1717 bsr 6),
                           _@1720 = 2 bsl 6 + _@1717 band 63,
                           [_@1715, _@1718, _@1719, _@1720]
                       end
                   end,
                   0);
        17458 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1721 = Acc,
                       _@1722 = 210,
                       _@1723 = Last,
                       begin
                           _@1724 = 14 bsl 4 + (_@1722 bsr 4),
                           _@1725 =
                               2 bsl 6 + (_@1722 band 15 bsl 2)
                               +
                               (_@1723 bsr 6),
                           _@1726 = 2 bsl 6 + _@1723 band 63,
                           [_@1721, _@1724, _@1725, _@1726]
                       end
                   end,
                   0);
        17459 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1727 = Acc,
                       _@1728 = 211,
                       _@1729 = Last,
                       begin
                           _@1730 = 14 bsl 4 + (_@1728 bsr 4),
                           _@1731 =
                               2 bsl 6 + (_@1728 band 15 bsl 2)
                               +
                               (_@1729 bsr 6),
                           _@1732 = 2 bsl 6 + _@1729 band 63,
                           [_@1727, _@1730, _@1731, _@1732]
                       end
                   end,
                   0);
        17460 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1733 = Acc,
                       _@1734 = 212,
                       _@1735 = Last,
                       begin
                           _@1736 = 14 bsl 4 + (_@1734 bsr 4),
                           _@1737 =
                               2 bsl 6 + (_@1734 band 15 bsl 2)
                               +
                               (_@1735 bsr 6),
                           _@1738 = 2 bsl 6 + _@1735 band 63,
                           [_@1733, _@1736, _@1737, _@1738]
                       end
                   end,
                   0);
        17461 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1739 = Acc,
                       _@1740 = 213,
                       _@1741 = Last,
                       begin
                           _@1742 = 14 bsl 4 + (_@1740 bsr 4),
                           _@1743 =
                               2 bsl 6 + (_@1740 band 15 bsl 2)
                               +
                               (_@1741 bsr 6),
                           _@1744 = 2 bsl 6 + _@1741 band 63,
                           [_@1739, _@1742, _@1743, _@1744]
                       end
                   end,
                   0);
        17462 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1745 = Acc,
                       _@1746 = 214,
                       _@1747 = Last,
                       begin
                           _@1748 = 14 bsl 4 + (_@1746 bsr 4),
                           _@1749 =
                               2 bsl 6 + (_@1746 band 15 bsl 2)
                               +
                               (_@1747 bsr 6),
                           _@1750 = 2 bsl 6 + _@1747 band 63,
                           [_@1745, _@1748, _@1749, _@1750]
                       end
                   end,
                   0);
        17463 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1751 = Acc,
                       _@1752 = 215,
                       _@1753 = Last,
                       begin
                           _@1754 = 14 bsl 4 + (_@1752 bsr 4),
                           _@1755 =
                               2 bsl 6 + (_@1752 band 15 bsl 2)
                               +
                               (_@1753 bsr 6),
                           _@1756 = 2 bsl 6 + _@1753 band 63,
                           [_@1751, _@1754, _@1755, _@1756]
                       end
                   end,
                   0);
        17464 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@1757 = 216,
                                 _@1758 = Last,
                                 65536
                                 +
                                 (_@1757 band 3 bsl 8 + _@1758 bsl 10)
                             end);
        17465 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@1759 = 217,
                                 _@1760 = Last,
                                 65536
                                 +
                                 (_@1759 band 3 bsl 8 + _@1760 bsl 10)
                             end);
        17473 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@1761 = 218,
                                 _@1762 = Last,
                                 65536
                                 +
                                 (_@1761 band 3 bsl 8 + _@1762 bsl 10)
                             end);
        17474 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@1763 = 219,
                                 _@1764 = Last,
                                 65536
                                 +
                                 (_@1763 band 3 bsl 8 + _@1764 bsl 10)
                             end);
        17505 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@1765 = 218,
                                 _@1766 = Last,
                                 65536
                                 +
                                 (_@1765 band 3 bsl 8 + _@1766 bsl 10)
                             end);
        17506 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@1767 = 219,
                                 _@1768 = Last,
                                 65536
                                 +
                                 (_@1767 band 3 bsl 8 + _@1768 bsl 10)
                             end);
        17712 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1769 = Acc,
                       _@1770 = 224,
                       _@1771 = Last,
                       begin
                           _@1772 = 14 bsl 4 + (_@1770 bsr 4),
                           _@1773 =
                               2 bsl 6 + (_@1770 band 15 bsl 2)
                               +
                               (_@1771 bsr 6),
                           _@1774 = 2 bsl 6 + _@1771 band 63,
                           [_@1769, _@1772, _@1773, _@1774]
                       end
                   end,
                   0);
        17713 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1775 = Acc,
                       _@1776 = 225,
                       _@1777 = Last,
                       begin
                           _@1778 = 14 bsl 4 + (_@1776 bsr 4),
                           _@1779 =
                               2 bsl 6 + (_@1776 band 15 bsl 2)
                               +
                               (_@1777 bsr 6),
                           _@1780 = 2 bsl 6 + _@1777 band 63,
                           [_@1775, _@1778, _@1779, _@1780]
                       end
                   end,
                   0);
        17714 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1781 = Acc,
                       _@1782 = 226,
                       _@1783 = Last,
                       begin
                           _@1784 = 14 bsl 4 + (_@1782 bsr 4),
                           _@1785 =
                               2 bsl 6 + (_@1782 band 15 bsl 2)
                               +
                               (_@1783 bsr 6),
                           _@1786 = 2 bsl 6 + _@1783 band 63,
                           [_@1781, _@1784, _@1785, _@1786]
                       end
                   end,
                   0);
        17715 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1787 = Acc,
                       _@1788 = 227,
                       _@1789 = Last,
                       begin
                           _@1790 = 14 bsl 4 + (_@1788 bsr 4),
                           _@1791 =
                               2 bsl 6 + (_@1788 band 15 bsl 2)
                               +
                               (_@1789 bsr 6),
                           _@1792 = 2 bsl 6 + _@1789 band 63,
                           [_@1787, _@1790, _@1791, _@1792]
                       end
                   end,
                   0);
        17716 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1793 = Acc,
                       _@1794 = 228,
                       _@1795 = Last,
                       begin
                           _@1796 = 14 bsl 4 + (_@1794 bsr 4),
                           _@1797 =
                               2 bsl 6 + (_@1794 band 15 bsl 2)
                               +
                               (_@1795 bsr 6),
                           _@1798 = 2 bsl 6 + _@1795 band 63,
                           [_@1793, _@1796, _@1797, _@1798]
                       end
                   end,
                   0);
        17717 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1799 = Acc,
                       _@1800 = 229,
                       _@1801 = Last,
                       begin
                           _@1802 = 14 bsl 4 + (_@1800 bsr 4),
                           _@1803 =
                               2 bsl 6 + (_@1800 band 15 bsl 2)
                               +
                               (_@1801 bsr 6),
                           _@1804 = 2 bsl 6 + _@1801 band 63,
                           [_@1799, _@1802, _@1803, _@1804]
                       end
                   end,
                   0);
        17718 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1805 = Acc,
                       _@1806 = 230,
                       _@1807 = Last,
                       begin
                           _@1808 = 14 bsl 4 + (_@1806 bsr 4),
                           _@1809 =
                               2 bsl 6 + (_@1806 band 15 bsl 2)
                               +
                               (_@1807 bsr 6),
                           _@1810 = 2 bsl 6 + _@1807 band 63,
                           [_@1805, _@1808, _@1809, _@1810]
                       end
                   end,
                   0);
        17719 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1811 = Acc,
                       _@1812 = 231,
                       _@1813 = Last,
                       begin
                           _@1814 = 14 bsl 4 + (_@1812 bsr 4),
                           _@1815 =
                               2 bsl 6 + (_@1812 band 15 bsl 2)
                               +
                               (_@1813 bsr 6),
                           _@1816 = 2 bsl 6 + _@1813 band 63,
                           [_@1811, _@1814, _@1815, _@1816]
                       end
                   end,
                   0);
        17720 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1817 = Acc,
                       _@1818 = 232,
                       _@1819 = Last,
                       begin
                           _@1820 = 14 bsl 4 + (_@1818 bsr 4),
                           _@1821 =
                               2 bsl 6 + (_@1818 band 15 bsl 2)
                               +
                               (_@1819 bsr 6),
                           _@1822 = 2 bsl 6 + _@1819 band 63,
                           [_@1817, _@1820, _@1821, _@1822]
                       end
                   end,
                   0);
        17721 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1823 = Acc,
                       _@1824 = 233,
                       _@1825 = Last,
                       begin
                           _@1826 = 14 bsl 4 + (_@1824 bsr 4),
                           _@1827 =
                               2 bsl 6 + (_@1824 band 15 bsl 2)
                               +
                               (_@1825 bsr 6),
                           _@1828 = 2 bsl 6 + _@1825 band 63,
                           [_@1823, _@1826, _@1827, _@1828]
                       end
                   end,
                   0);
        17729 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1829 = Acc,
                       _@1830 = 234,
                       _@1831 = Last,
                       begin
                           _@1832 = 14 bsl 4 + (_@1830 bsr 4),
                           _@1833 =
                               2 bsl 6 + (_@1830 band 15 bsl 2)
                               +
                               (_@1831 bsr 6),
                           _@1834 = 2 bsl 6 + _@1831 band 63,
                           [_@1829, _@1832, _@1833, _@1834]
                       end
                   end,
                   0);
        17730 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1835 = Acc,
                       _@1836 = 235,
                       _@1837 = Last,
                       begin
                           _@1838 = 14 bsl 4 + (_@1836 bsr 4),
                           _@1839 =
                               2 bsl 6 + (_@1836 band 15 bsl 2)
                               +
                               (_@1837 bsr 6),
                           _@1840 = 2 bsl 6 + _@1837 band 63,
                           [_@1835, _@1838, _@1839, _@1840]
                       end
                   end,
                   0);
        17731 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1841 = Acc,
                       _@1842 = 236,
                       _@1843 = Last,
                       begin
                           _@1844 = 14 bsl 4 + (_@1842 bsr 4),
                           _@1845 =
                               2 bsl 6 + (_@1842 band 15 bsl 2)
                               +
                               (_@1843 bsr 6),
                           _@1846 = 2 bsl 6 + _@1843 band 63,
                           [_@1841, _@1844, _@1845, _@1846]
                       end
                   end,
                   0);
        17732 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1847 = Acc,
                       _@1848 = 237,
                       _@1849 = Last,
                       begin
                           _@1850 = 14 bsl 4 + (_@1848 bsr 4),
                           _@1851 =
                               2 bsl 6 + (_@1848 band 15 bsl 2)
                               +
                               (_@1849 bsr 6),
                           _@1852 = 2 bsl 6 + _@1849 band 63,
                           [_@1847, _@1850, _@1851, _@1852]
                       end
                   end,
                   0);
        17733 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1853 = Acc,
                       _@1854 = 238,
                       _@1855 = Last,
                       begin
                           _@1856 = 14 bsl 4 + (_@1854 bsr 4),
                           _@1857 =
                               2 bsl 6 + (_@1854 band 15 bsl 2)
                               +
                               (_@1855 bsr 6),
                           _@1858 = 2 bsl 6 + _@1855 band 63,
                           [_@1853, _@1856, _@1857, _@1858]
                       end
                   end,
                   0);
        17734 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1859 = Acc,
                       _@1860 = 239,
                       _@1861 = Last,
                       begin
                           _@1862 = 14 bsl 4 + (_@1860 bsr 4),
                           _@1863 =
                               2 bsl 6 + (_@1860 band 15 bsl 2)
                               +
                               (_@1861 bsr 6),
                           _@1864 = 2 bsl 6 + _@1861 band 63,
                           [_@1859, _@1862, _@1863, _@1864]
                       end
                   end,
                   0);
        17761 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1865 = Acc,
                       _@1866 = 234,
                       _@1867 = Last,
                       begin
                           _@1868 = 14 bsl 4 + (_@1866 bsr 4),
                           _@1869 =
                               2 bsl 6 + (_@1866 band 15 bsl 2)
                               +
                               (_@1867 bsr 6),
                           _@1870 = 2 bsl 6 + _@1867 band 63,
                           [_@1865, _@1868, _@1869, _@1870]
                       end
                   end,
                   0);
        17762 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1871 = Acc,
                       _@1872 = 235,
                       _@1873 = Last,
                       begin
                           _@1874 = 14 bsl 4 + (_@1872 bsr 4),
                           _@1875 =
                               2 bsl 6 + (_@1872 band 15 bsl 2)
                               +
                               (_@1873 bsr 6),
                           _@1876 = 2 bsl 6 + _@1873 band 63,
                           [_@1871, _@1874, _@1875, _@1876]
                       end
                   end,
                   0);
        17763 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1877 = Acc,
                       _@1878 = 236,
                       _@1879 = Last,
                       begin
                           _@1880 = 14 bsl 4 + (_@1878 bsr 4),
                           _@1881 =
                               2 bsl 6 + (_@1878 band 15 bsl 2)
                               +
                               (_@1879 bsr 6),
                           _@1882 = 2 bsl 6 + _@1879 band 63,
                           [_@1877, _@1880, _@1881, _@1882]
                       end
                   end,
                   0);
        17764 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1883 = Acc,
                       _@1884 = 237,
                       _@1885 = Last,
                       begin
                           _@1886 = 14 bsl 4 + (_@1884 bsr 4),
                           _@1887 =
                               2 bsl 6 + (_@1884 band 15 bsl 2)
                               +
                               (_@1885 bsr 6),
                           _@1888 = 2 bsl 6 + _@1885 band 63,
                           [_@1883, _@1886, _@1887, _@1888]
                       end
                   end,
                   0);
        17765 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1889 = Acc,
                       _@1890 = 238,
                       _@1891 = Last,
                       begin
                           _@1892 = 14 bsl 4 + (_@1890 bsr 4),
                           _@1893 =
                               2 bsl 6 + (_@1890 band 15 bsl 2)
                               +
                               (_@1891 bsr 6),
                           _@1894 = 2 bsl 6 + _@1891 band 63,
                           [_@1889, _@1892, _@1893, _@1894]
                       end
                   end,
                   0);
        17766 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1895 = Acc,
                       _@1896 = 239,
                       _@1897 = Last,
                       begin
                           _@1898 = 14 bsl 4 + (_@1896 bsr 4),
                           _@1899 =
                               2 bsl 6 + (_@1896 band 15 bsl 2)
                               +
                               (_@1897 bsr 6),
                           _@1900 = 2 bsl 6 + _@1897 band 63,
                           [_@1895, _@1898, _@1899, _@1900]
                       end
                   end,
                   0);
        17968 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1901 = Acc,
                       _@1902 = 240,
                       _@1903 = Last,
                       begin
                           _@1904 = 14 bsl 4 + (_@1902 bsr 4),
                           _@1905 =
                               2 bsl 6 + (_@1902 band 15 bsl 2)
                               +
                               (_@1903 bsr 6),
                           _@1906 = 2 bsl 6 + _@1903 band 63,
                           [_@1901, _@1904, _@1905, _@1906]
                       end
                   end,
                   0);
        17969 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1907 = Acc,
                       _@1908 = 241,
                       _@1909 = Last,
                       begin
                           _@1910 = 14 bsl 4 + (_@1908 bsr 4),
                           _@1911 =
                               2 bsl 6 + (_@1908 band 15 bsl 2)
                               +
                               (_@1909 bsr 6),
                           _@1912 = 2 bsl 6 + _@1909 band 63,
                           [_@1907, _@1910, _@1911, _@1912]
                       end
                   end,
                   0);
        17970 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1913 = Acc,
                       _@1914 = 242,
                       _@1915 = Last,
                       begin
                           _@1916 = 14 bsl 4 + (_@1914 bsr 4),
                           _@1917 =
                               2 bsl 6 + (_@1914 band 15 bsl 2)
                               +
                               (_@1915 bsr 6),
                           _@1918 = 2 bsl 6 + _@1915 band 63,
                           [_@1913, _@1916, _@1917, _@1918]
                       end
                   end,
                   0);
        17971 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1919 = Acc,
                       _@1920 = 243,
                       _@1921 = Last,
                       begin
                           _@1922 = 14 bsl 4 + (_@1920 bsr 4),
                           _@1923 =
                               2 bsl 6 + (_@1920 band 15 bsl 2)
                               +
                               (_@1921 bsr 6),
                           _@1924 = 2 bsl 6 + _@1921 band 63,
                           [_@1919, _@1922, _@1923, _@1924]
                       end
                   end,
                   0);
        17972 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1925 = Acc,
                       _@1926 = 244,
                       _@1927 = Last,
                       begin
                           _@1928 = 14 bsl 4 + (_@1926 bsr 4),
                           _@1929 =
                               2 bsl 6 + (_@1926 band 15 bsl 2)
                               +
                               (_@1927 bsr 6),
                           _@1930 = 2 bsl 6 + _@1927 band 63,
                           [_@1925, _@1928, _@1929, _@1930]
                       end
                   end,
                   0);
        17973 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1931 = Acc,
                       _@1932 = 245,
                       _@1933 = Last,
                       begin
                           _@1934 = 14 bsl 4 + (_@1932 bsr 4),
                           _@1935 =
                               2 bsl 6 + (_@1932 band 15 bsl 2)
                               +
                               (_@1933 bsr 6),
                           _@1936 = 2 bsl 6 + _@1933 band 63,
                           [_@1931, _@1934, _@1935, _@1936]
                       end
                   end,
                   0);
        17974 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1937 = Acc,
                       _@1938 = 246,
                       _@1939 = Last,
                       begin
                           _@1940 = 14 bsl 4 + (_@1938 bsr 4),
                           _@1941 =
                               2 bsl 6 + (_@1938 band 15 bsl 2)
                               +
                               (_@1939 bsr 6),
                           _@1942 = 2 bsl 6 + _@1939 band 63,
                           [_@1937, _@1940, _@1941, _@1942]
                       end
                   end,
                   0);
        17975 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1943 = Acc,
                       _@1944 = 247,
                       _@1945 = Last,
                       begin
                           _@1946 = 14 bsl 4 + (_@1944 bsr 4),
                           _@1947 =
                               2 bsl 6 + (_@1944 band 15 bsl 2)
                               +
                               (_@1945 bsr 6),
                           _@1948 = 2 bsl 6 + _@1945 band 63,
                           [_@1943, _@1946, _@1947, _@1948]
                       end
                   end,
                   0);
        17976 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1949 = Acc,
                       _@1950 = 248,
                       _@1951 = Last,
                       begin
                           _@1952 = 14 bsl 4 + (_@1950 bsr 4),
                           _@1953 =
                               2 bsl 6 + (_@1950 band 15 bsl 2)
                               +
                               (_@1951 bsr 6),
                           _@1954 = 2 bsl 6 + _@1951 band 63,
                           [_@1949, _@1952, _@1953, _@1954]
                       end
                   end,
                   0);
        17977 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1955 = Acc,
                       _@1956 = 249,
                       _@1957 = Last,
                       begin
                           _@1958 = 14 bsl 4 + (_@1956 bsr 4),
                           _@1959 =
                               2 bsl 6 + (_@1956 band 15 bsl 2)
                               +
                               (_@1957 bsr 6),
                           _@1960 = 2 bsl 6 + _@1957 band 63,
                           [_@1955, _@1958, _@1959, _@1960]
                       end
                   end,
                   0);
        17985 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1961 = Acc,
                       _@1962 = 250,
                       _@1963 = Last,
                       begin
                           _@1964 = 14 bsl 4 + (_@1962 bsr 4),
                           _@1965 =
                               2 bsl 6 + (_@1962 band 15 bsl 2)
                               +
                               (_@1963 bsr 6),
                           _@1966 = 2 bsl 6 + _@1963 band 63,
                           [_@1961, _@1964, _@1965, _@1966]
                       end
                   end,
                   0);
        17986 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1967 = Acc,
                       _@1968 = 251,
                       _@1969 = Last,
                       begin
                           _@1970 = 14 bsl 4 + (_@1968 bsr 4),
                           _@1971 =
                               2 bsl 6 + (_@1968 band 15 bsl 2)
                               +
                               (_@1969 bsr 6),
                           _@1972 = 2 bsl 6 + _@1969 band 63,
                           [_@1967, _@1970, _@1971, _@1972]
                       end
                   end,
                   0);
        17987 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1973 = Acc,
                       _@1974 = 252,
                       _@1975 = Last,
                       begin
                           _@1976 = 14 bsl 4 + (_@1974 bsr 4),
                           _@1977 =
                               2 bsl 6 + (_@1974 band 15 bsl 2)
                               +
                               (_@1975 bsr 6),
                           _@1978 = 2 bsl 6 + _@1975 band 63,
                           [_@1973, _@1976, _@1977, _@1978]
                       end
                   end,
                   0);
        17988 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1979 = Acc,
                       _@1980 = 253,
                       _@1981 = Last,
                       begin
                           _@1982 = 14 bsl 4 + (_@1980 bsr 4),
                           _@1983 =
                               2 bsl 6 + (_@1980 band 15 bsl 2)
                               +
                               (_@1981 bsr 6),
                           _@1984 = 2 bsl 6 + _@1981 band 63,
                           [_@1979, _@1982, _@1983, _@1984]
                       end
                   end,
                   0);
        17989 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1985 = Acc,
                       _@1986 = 254,
                       _@1987 = Last,
                       begin
                           _@1988 = 14 bsl 4 + (_@1986 bsr 4),
                           _@1989 =
                               2 bsl 6 + (_@1986 band 15 bsl 2)
                               +
                               (_@1987 bsr 6),
                           _@1990 = 2 bsl 6 + _@1987 band 63,
                           [_@1985, _@1988, _@1989, _@1990]
                       end
                   end,
                   0);
        17990 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1991 = Acc,
                       _@1992 = 255,
                       _@1993 = Last,
                       begin
                           _@1994 = 14 bsl 4 + (_@1992 bsr 4),
                           _@1995 =
                               2 bsl 6 + (_@1992 band 15 bsl 2)
                               +
                               (_@1993 bsr 6),
                           _@1996 = 2 bsl 6 + _@1993 band 63,
                           [_@1991, _@1994, _@1995, _@1996]
                       end
                   end,
                   0);
        18017 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@1997 = Acc,
                       _@1998 = 250,
                       _@1999 = Last,
                       begin
                           _@2000 = 14 bsl 4 + (_@1998 bsr 4),
                           _@2001 =
                               2 bsl 6 + (_@1998 band 15 bsl 2)
                               +
                               (_@1999 bsr 6),
                           _@2002 = 2 bsl 6 + _@1999 band 63,
                           [_@1997, _@2000, _@2001, _@2002]
                       end
                   end,
                   0);
        18018 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2003 = Acc,
                       _@2004 = 251,
                       _@2005 = Last,
                       begin
                           _@2006 = 14 bsl 4 + (_@2004 bsr 4),
                           _@2007 =
                               2 bsl 6 + (_@2004 band 15 bsl 2)
                               +
                               (_@2005 bsr 6),
                           _@2008 = 2 bsl 6 + _@2005 band 63,
                           [_@2003, _@2006, _@2007, _@2008]
                       end
                   end,
                   0);
        18019 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2009 = Acc,
                       _@2010 = 252,
                       _@2011 = Last,
                       begin
                           _@2012 = 14 bsl 4 + (_@2010 bsr 4),
                           _@2013 =
                               2 bsl 6 + (_@2010 band 15 bsl 2)
                               +
                               (_@2011 bsr 6),
                           _@2014 = 2 bsl 6 + _@2011 band 63,
                           [_@2009, _@2012, _@2013, _@2014]
                       end
                   end,
                   0);
        18020 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2015 = Acc,
                       _@2016 = 253,
                       _@2017 = Last,
                       begin
                           _@2018 = 14 bsl 4 + (_@2016 bsr 4),
                           _@2019 =
                               2 bsl 6 + (_@2016 band 15 bsl 2)
                               +
                               (_@2017 bsr 6),
                           _@2020 = 2 bsl 6 + _@2017 band 63,
                           [_@2015, _@2018, _@2019, _@2020]
                       end
                   end,
                   0);
        18021 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2021 = Acc,
                       _@2022 = 254,
                       _@2023 = Last,
                       begin
                           _@2024 = 14 bsl 4 + (_@2022 bsr 4),
                           _@2025 =
                               2 bsl 6 + (_@2022 band 15 bsl 2)
                               +
                               (_@2023 bsr 6),
                           _@2026 = 2 bsl 6 + _@2023 band 63,
                           [_@2021, _@2024, _@2025, _@2026]
                       end
                   end,
                   0);
        18022 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2027 = Acc,
                       _@2028 = 255,
                       _@2029 = Last,
                       begin
                           _@2030 = 14 bsl 4 + (_@2028 bsr 4),
                           _@2031 =
                               2 bsl 6 + (_@2028 band 15 bsl 2)
                               +
                               (_@2029 bsr 6),
                           _@2032 = 2 bsl 6 + _@2029 band 63,
                           [_@2027, _@2030, _@2031, _@2032]
                       end
                   end,
                   0);
        24880 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2033 = Acc,
                       _@2034 = 160,
                       _@2035 = Last,
                       begin
                           _@2036 = 14 bsl 4 + (_@2034 bsr 4),
                           _@2037 =
                               2 bsl 6 + (_@2034 band 15 bsl 2)
                               +
                               (_@2035 bsr 6),
                           _@2038 = 2 bsl 6 + _@2035 band 63,
                           [_@2033, _@2036, _@2037, _@2038]
                       end
                   end,
                   0);
        24881 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2039 = Acc,
                       _@2040 = 161,
                       _@2041 = Last,
                       begin
                           _@2042 = 14 bsl 4 + (_@2040 bsr 4),
                           _@2043 =
                               2 bsl 6 + (_@2040 band 15 bsl 2)
                               +
                               (_@2041 bsr 6),
                           _@2044 = 2 bsl 6 + _@2041 band 63,
                           [_@2039, _@2042, _@2043, _@2044]
                       end
                   end,
                   0);
        24882 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2045 = Acc,
                       _@2046 = 162,
                       _@2047 = Last,
                       begin
                           _@2048 = 14 bsl 4 + (_@2046 bsr 4),
                           _@2049 =
                               2 bsl 6 + (_@2046 band 15 bsl 2)
                               +
                               (_@2047 bsr 6),
                           _@2050 = 2 bsl 6 + _@2047 band 63,
                           [_@2045, _@2048, _@2049, _@2050]
                       end
                   end,
                   0);
        24883 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2051 = Acc,
                       _@2052 = 163,
                       _@2053 = Last,
                       begin
                           _@2054 = 14 bsl 4 + (_@2052 bsr 4),
                           _@2055 =
                               2 bsl 6 + (_@2052 band 15 bsl 2)
                               +
                               (_@2053 bsr 6),
                           _@2056 = 2 bsl 6 + _@2053 band 63,
                           [_@2051, _@2054, _@2055, _@2056]
                       end
                   end,
                   0);
        24884 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2057 = Acc,
                       _@2058 = 164,
                       _@2059 = Last,
                       begin
                           _@2060 = 14 bsl 4 + (_@2058 bsr 4),
                           _@2061 =
                               2 bsl 6 + (_@2058 band 15 bsl 2)
                               +
                               (_@2059 bsr 6),
                           _@2062 = 2 bsl 6 + _@2059 band 63,
                           [_@2057, _@2060, _@2061, _@2062]
                       end
                   end,
                   0);
        24885 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2063 = Acc,
                       _@2064 = 165,
                       _@2065 = Last,
                       begin
                           _@2066 = 14 bsl 4 + (_@2064 bsr 4),
                           _@2067 =
                               2 bsl 6 + (_@2064 band 15 bsl 2)
                               +
                               (_@2065 bsr 6),
                           _@2068 = 2 bsl 6 + _@2065 band 63,
                           [_@2063, _@2066, _@2067, _@2068]
                       end
                   end,
                   0);
        24886 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2069 = Acc,
                       _@2070 = 166,
                       _@2071 = Last,
                       begin
                           _@2072 = 14 bsl 4 + (_@2070 bsr 4),
                           _@2073 =
                               2 bsl 6 + (_@2070 band 15 bsl 2)
                               +
                               (_@2071 bsr 6),
                           _@2074 = 2 bsl 6 + _@2071 band 63,
                           [_@2069, _@2072, _@2073, _@2074]
                       end
                   end,
                   0);
        24887 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2075 = Acc,
                       _@2076 = 167,
                       _@2077 = Last,
                       begin
                           _@2078 = 14 bsl 4 + (_@2076 bsr 4),
                           _@2079 =
                               2 bsl 6 + (_@2076 band 15 bsl 2)
                               +
                               (_@2077 bsr 6),
                           _@2080 = 2 bsl 6 + _@2077 band 63,
                           [_@2075, _@2078, _@2079, _@2080]
                       end
                   end,
                   0);
        24888 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2081 = Acc,
                       _@2082 = 168,
                       _@2083 = Last,
                       begin
                           _@2084 = 14 bsl 4 + (_@2082 bsr 4),
                           _@2085 =
                               2 bsl 6 + (_@2082 band 15 bsl 2)
                               +
                               (_@2083 bsr 6),
                           _@2086 = 2 bsl 6 + _@2083 band 63,
                           [_@2081, _@2084, _@2085, _@2086]
                       end
                   end,
                   0);
        24889 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2087 = Acc,
                       _@2088 = 169,
                       _@2089 = Last,
                       begin
                           _@2090 = 14 bsl 4 + (_@2088 bsr 4),
                           _@2091 =
                               2 bsl 6 + (_@2088 band 15 bsl 2)
                               +
                               (_@2089 bsr 6),
                           _@2092 = 2 bsl 6 + _@2089 band 63,
                           [_@2087, _@2090, _@2091, _@2092]
                       end
                   end,
                   0);
        24897 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2093 = Acc,
                       _@2094 = 170,
                       _@2095 = Last,
                       begin
                           _@2096 = 14 bsl 4 + (_@2094 bsr 4),
                           _@2097 =
                               2 bsl 6 + (_@2094 band 15 bsl 2)
                               +
                               (_@2095 bsr 6),
                           _@2098 = 2 bsl 6 + _@2095 band 63,
                           [_@2093, _@2096, _@2097, _@2098]
                       end
                   end,
                   0);
        24898 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2099 = Acc,
                       _@2100 = 171,
                       _@2101 = Last,
                       begin
                           _@2102 = 14 bsl 4 + (_@2100 bsr 4),
                           _@2103 =
                               2 bsl 6 + (_@2100 band 15 bsl 2)
                               +
                               (_@2101 bsr 6),
                           _@2104 = 2 bsl 6 + _@2101 band 63,
                           [_@2099, _@2102, _@2103, _@2104]
                       end
                   end,
                   0);
        24899 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2105 = Acc,
                       _@2106 = 172,
                       _@2107 = Last,
                       begin
                           _@2108 = 14 bsl 4 + (_@2106 bsr 4),
                           _@2109 =
                               2 bsl 6 + (_@2106 band 15 bsl 2)
                               +
                               (_@2107 bsr 6),
                           _@2110 = 2 bsl 6 + _@2107 band 63,
                           [_@2105, _@2108, _@2109, _@2110]
                       end
                   end,
                   0);
        24900 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2111 = Acc,
                       _@2112 = 173,
                       _@2113 = Last,
                       begin
                           _@2114 = 14 bsl 4 + (_@2112 bsr 4),
                           _@2115 =
                               2 bsl 6 + (_@2112 band 15 bsl 2)
                               +
                               (_@2113 bsr 6),
                           _@2116 = 2 bsl 6 + _@2113 band 63,
                           [_@2111, _@2114, _@2115, _@2116]
                       end
                   end,
                   0);
        24901 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2117 = Acc,
                       _@2118 = 174,
                       _@2119 = Last,
                       begin
                           _@2120 = 14 bsl 4 + (_@2118 bsr 4),
                           _@2121 =
                               2 bsl 6 + (_@2118 band 15 bsl 2)
                               +
                               (_@2119 bsr 6),
                           _@2122 = 2 bsl 6 + _@2119 band 63,
                           [_@2117, _@2120, _@2121, _@2122]
                       end
                   end,
                   0);
        24902 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2123 = Acc,
                       _@2124 = 175,
                       _@2125 = Last,
                       begin
                           _@2126 = 14 bsl 4 + (_@2124 bsr 4),
                           _@2127 =
                               2 bsl 6 + (_@2124 band 15 bsl 2)
                               +
                               (_@2125 bsr 6),
                           _@2128 = 2 bsl 6 + _@2125 band 63,
                           [_@2123, _@2126, _@2127, _@2128]
                       end
                   end,
                   0);
        24929 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2129 = Acc,
                       _@2130 = 170,
                       _@2131 = Last,
                       begin
                           _@2132 = 14 bsl 4 + (_@2130 bsr 4),
                           _@2133 =
                               2 bsl 6 + (_@2130 band 15 bsl 2)
                               +
                               (_@2131 bsr 6),
                           _@2134 = 2 bsl 6 + _@2131 band 63,
                           [_@2129, _@2132, _@2133, _@2134]
                       end
                   end,
                   0);
        24930 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2135 = Acc,
                       _@2136 = 171,
                       _@2137 = Last,
                       begin
                           _@2138 = 14 bsl 4 + (_@2136 bsr 4),
                           _@2139 =
                               2 bsl 6 + (_@2136 band 15 bsl 2)
                               +
                               (_@2137 bsr 6),
                           _@2140 = 2 bsl 6 + _@2137 band 63,
                           [_@2135, _@2138, _@2139, _@2140]
                       end
                   end,
                   0);
        24931 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2141 = Acc,
                       _@2142 = 172,
                       _@2143 = Last,
                       begin
                           _@2144 = 14 bsl 4 + (_@2142 bsr 4),
                           _@2145 =
                               2 bsl 6 + (_@2142 band 15 bsl 2)
                               +
                               (_@2143 bsr 6),
                           _@2146 = 2 bsl 6 + _@2143 band 63,
                           [_@2141, _@2144, _@2145, _@2146]
                       end
                   end,
                   0);
        24932 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2147 = Acc,
                       _@2148 = 173,
                       _@2149 = Last,
                       begin
                           _@2150 = 14 bsl 4 + (_@2148 bsr 4),
                           _@2151 =
                               2 bsl 6 + (_@2148 band 15 bsl 2)
                               +
                               (_@2149 bsr 6),
                           _@2152 = 2 bsl 6 + _@2149 band 63,
                           [_@2147, _@2150, _@2151, _@2152]
                       end
                   end,
                   0);
        24933 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2153 = Acc,
                       _@2154 = 174,
                       _@2155 = Last,
                       begin
                           _@2156 = 14 bsl 4 + (_@2154 bsr 4),
                           _@2157 =
                               2 bsl 6 + (_@2154 band 15 bsl 2)
                               +
                               (_@2155 bsr 6),
                           _@2158 = 2 bsl 6 + _@2155 band 63,
                           [_@2153, _@2156, _@2157, _@2158]
                       end
                   end,
                   0);
        24934 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2159 = Acc,
                       _@2160 = 175,
                       _@2161 = Last,
                       begin
                           _@2162 = 14 bsl 4 + (_@2160 bsr 4),
                           _@2163 =
                               2 bsl 6 + (_@2160 band 15 bsl 2)
                               +
                               (_@2161 bsr 6),
                           _@2164 = 2 bsl 6 + _@2161 band 63,
                           [_@2159, _@2162, _@2163, _@2164]
                       end
                   end,
                   0);
        25136 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2165 = Acc,
                       _@2166 = 176,
                       _@2167 = Last,
                       begin
                           _@2168 = 14 bsl 4 + (_@2166 bsr 4),
                           _@2169 =
                               2 bsl 6 + (_@2166 band 15 bsl 2)
                               +
                               (_@2167 bsr 6),
                           _@2170 = 2 bsl 6 + _@2167 band 63,
                           [_@2165, _@2168, _@2169, _@2170]
                       end
                   end,
                   0);
        25137 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2171 = Acc,
                       _@2172 = 177,
                       _@2173 = Last,
                       begin
                           _@2174 = 14 bsl 4 + (_@2172 bsr 4),
                           _@2175 =
                               2 bsl 6 + (_@2172 band 15 bsl 2)
                               +
                               (_@2173 bsr 6),
                           _@2176 = 2 bsl 6 + _@2173 band 63,
                           [_@2171, _@2174, _@2175, _@2176]
                       end
                   end,
                   0);
        25138 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2177 = Acc,
                       _@2178 = 178,
                       _@2179 = Last,
                       begin
                           _@2180 = 14 bsl 4 + (_@2178 bsr 4),
                           _@2181 =
                               2 bsl 6 + (_@2178 band 15 bsl 2)
                               +
                               (_@2179 bsr 6),
                           _@2182 = 2 bsl 6 + _@2179 band 63,
                           [_@2177, _@2180, _@2181, _@2182]
                       end
                   end,
                   0);
        25139 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2183 = Acc,
                       _@2184 = 179,
                       _@2185 = Last,
                       begin
                           _@2186 = 14 bsl 4 + (_@2184 bsr 4),
                           _@2187 =
                               2 bsl 6 + (_@2184 band 15 bsl 2)
                               +
                               (_@2185 bsr 6),
                           _@2188 = 2 bsl 6 + _@2185 band 63,
                           [_@2183, _@2186, _@2187, _@2188]
                       end
                   end,
                   0);
        25140 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2189 = Acc,
                       _@2190 = 180,
                       _@2191 = Last,
                       begin
                           _@2192 = 14 bsl 4 + (_@2190 bsr 4),
                           _@2193 =
                               2 bsl 6 + (_@2190 band 15 bsl 2)
                               +
                               (_@2191 bsr 6),
                           _@2194 = 2 bsl 6 + _@2191 band 63,
                           [_@2189, _@2192, _@2193, _@2194]
                       end
                   end,
                   0);
        25141 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2195 = Acc,
                       _@2196 = 181,
                       _@2197 = Last,
                       begin
                           _@2198 = 14 bsl 4 + (_@2196 bsr 4),
                           _@2199 =
                               2 bsl 6 + (_@2196 band 15 bsl 2)
                               +
                               (_@2197 bsr 6),
                           _@2200 = 2 bsl 6 + _@2197 band 63,
                           [_@2195, _@2198, _@2199, _@2200]
                       end
                   end,
                   0);
        25142 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2201 = Acc,
                       _@2202 = 182,
                       _@2203 = Last,
                       begin
                           _@2204 = 14 bsl 4 + (_@2202 bsr 4),
                           _@2205 =
                               2 bsl 6 + (_@2202 band 15 bsl 2)
                               +
                               (_@2203 bsr 6),
                           _@2206 = 2 bsl 6 + _@2203 band 63,
                           [_@2201, _@2204, _@2205, _@2206]
                       end
                   end,
                   0);
        25143 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2207 = Acc,
                       _@2208 = 183,
                       _@2209 = Last,
                       begin
                           _@2210 = 14 bsl 4 + (_@2208 bsr 4),
                           _@2211 =
                               2 bsl 6 + (_@2208 band 15 bsl 2)
                               +
                               (_@2209 bsr 6),
                           _@2212 = 2 bsl 6 + _@2209 band 63,
                           [_@2207, _@2210, _@2211, _@2212]
                       end
                   end,
                   0);
        25144 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2213 = Acc,
                       _@2214 = 184,
                       _@2215 = Last,
                       begin
                           _@2216 = 14 bsl 4 + (_@2214 bsr 4),
                           _@2217 =
                               2 bsl 6 + (_@2214 band 15 bsl 2)
                               +
                               (_@2215 bsr 6),
                           _@2218 = 2 bsl 6 + _@2215 band 63,
                           [_@2213, _@2216, _@2217, _@2218]
                       end
                   end,
                   0);
        25145 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2219 = Acc,
                       _@2220 = 185,
                       _@2221 = Last,
                       begin
                           _@2222 = 14 bsl 4 + (_@2220 bsr 4),
                           _@2223 =
                               2 bsl 6 + (_@2220 band 15 bsl 2)
                               +
                               (_@2221 bsr 6),
                           _@2224 = 2 bsl 6 + _@2221 band 63,
                           [_@2219, _@2222, _@2223, _@2224]
                       end
                   end,
                   0);
        25153 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2225 = Acc,
                       _@2226 = 186,
                       _@2227 = Last,
                       begin
                           _@2228 = 14 bsl 4 + (_@2226 bsr 4),
                           _@2229 =
                               2 bsl 6 + (_@2226 band 15 bsl 2)
                               +
                               (_@2227 bsr 6),
                           _@2230 = 2 bsl 6 + _@2227 band 63,
                           [_@2225, _@2228, _@2229, _@2230]
                       end
                   end,
                   0);
        25154 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2231 = Acc,
                       _@2232 = 187,
                       _@2233 = Last,
                       begin
                           _@2234 = 14 bsl 4 + (_@2232 bsr 4),
                           _@2235 =
                               2 bsl 6 + (_@2232 band 15 bsl 2)
                               +
                               (_@2233 bsr 6),
                           _@2236 = 2 bsl 6 + _@2233 band 63,
                           [_@2231, _@2234, _@2235, _@2236]
                       end
                   end,
                   0);
        25155 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2237 = Acc,
                       _@2238 = 188,
                       _@2239 = Last,
                       begin
                           _@2240 = 14 bsl 4 + (_@2238 bsr 4),
                           _@2241 =
                               2 bsl 6 + (_@2238 band 15 bsl 2)
                               +
                               (_@2239 bsr 6),
                           _@2242 = 2 bsl 6 + _@2239 band 63,
                           [_@2237, _@2240, _@2241, _@2242]
                       end
                   end,
                   0);
        25156 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2243 = Acc,
                       _@2244 = 189,
                       _@2245 = Last,
                       begin
                           _@2246 = 14 bsl 4 + (_@2244 bsr 4),
                           _@2247 =
                               2 bsl 6 + (_@2244 band 15 bsl 2)
                               +
                               (_@2245 bsr 6),
                           _@2248 = 2 bsl 6 + _@2245 band 63,
                           [_@2243, _@2246, _@2247, _@2248]
                       end
                   end,
                   0);
        25157 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2249 = Acc,
                       _@2250 = 190,
                       _@2251 = Last,
                       begin
                           _@2252 = 14 bsl 4 + (_@2250 bsr 4),
                           _@2253 =
                               2 bsl 6 + (_@2250 band 15 bsl 2)
                               +
                               (_@2251 bsr 6),
                           _@2254 = 2 bsl 6 + _@2251 band 63,
                           [_@2249, _@2252, _@2253, _@2254]
                       end
                   end,
                   0);
        25158 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2255 = Acc,
                       _@2256 = 191,
                       _@2257 = Last,
                       begin
                           _@2258 = 14 bsl 4 + (_@2256 bsr 4),
                           _@2259 =
                               2 bsl 6 + (_@2256 band 15 bsl 2)
                               +
                               (_@2257 bsr 6),
                           _@2260 = 2 bsl 6 + _@2257 band 63,
                           [_@2255, _@2258, _@2259, _@2260]
                       end
                   end,
                   0);
        25185 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2261 = Acc,
                       _@2262 = 186,
                       _@2263 = Last,
                       begin
                           _@2264 = 14 bsl 4 + (_@2262 bsr 4),
                           _@2265 =
                               2 bsl 6 + (_@2262 band 15 bsl 2)
                               +
                               (_@2263 bsr 6),
                           _@2266 = 2 bsl 6 + _@2263 band 63,
                           [_@2261, _@2264, _@2265, _@2266]
                       end
                   end,
                   0);
        25186 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2267 = Acc,
                       _@2268 = 187,
                       _@2269 = Last,
                       begin
                           _@2270 = 14 bsl 4 + (_@2268 bsr 4),
                           _@2271 =
                               2 bsl 6 + (_@2268 band 15 bsl 2)
                               +
                               (_@2269 bsr 6),
                           _@2272 = 2 bsl 6 + _@2269 band 63,
                           [_@2267, _@2270, _@2271, _@2272]
                       end
                   end,
                   0);
        25187 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2273 = Acc,
                       _@2274 = 188,
                       _@2275 = Last,
                       begin
                           _@2276 = 14 bsl 4 + (_@2274 bsr 4),
                           _@2277 =
                               2 bsl 6 + (_@2274 band 15 bsl 2)
                               +
                               (_@2275 bsr 6),
                           _@2278 = 2 bsl 6 + _@2275 band 63,
                           [_@2273, _@2276, _@2277, _@2278]
                       end
                   end,
                   0);
        25188 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2279 = Acc,
                       _@2280 = 189,
                       _@2281 = Last,
                       begin
                           _@2282 = 14 bsl 4 + (_@2280 bsr 4),
                           _@2283 =
                               2 bsl 6 + (_@2280 band 15 bsl 2)
                               +
                               (_@2281 bsr 6),
                           _@2284 = 2 bsl 6 + _@2281 band 63,
                           [_@2279, _@2282, _@2283, _@2284]
                       end
                   end,
                   0);
        25189 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2285 = Acc,
                       _@2286 = 190,
                       _@2287 = Last,
                       begin
                           _@2288 = 14 bsl 4 + (_@2286 bsr 4),
                           _@2289 =
                               2 bsl 6 + (_@2286 band 15 bsl 2)
                               +
                               (_@2287 bsr 6),
                           _@2290 = 2 bsl 6 + _@2287 band 63,
                           [_@2285, _@2288, _@2289, _@2290]
                       end
                   end,
                   0);
        25190 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2291 = Acc,
                       _@2292 = 191,
                       _@2293 = Last,
                       begin
                           _@2294 = 14 bsl 4 + (_@2292 bsr 4),
                           _@2295 =
                               2 bsl 6 + (_@2292 band 15 bsl 2)
                               +
                               (_@2293 bsr 6),
                           _@2296 = 2 bsl 6 + _@2293 band 63,
                           [_@2291, _@2294, _@2295, _@2296]
                       end
                   end,
                   0);
        25392 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2297 = Acc,
                       _@2298 = 192,
                       _@2299 = Last,
                       begin
                           _@2300 = 14 bsl 4 + (_@2298 bsr 4),
                           _@2301 =
                               2 bsl 6 + (_@2298 band 15 bsl 2)
                               +
                               (_@2299 bsr 6),
                           _@2302 = 2 bsl 6 + _@2299 band 63,
                           [_@2297, _@2300, _@2301, _@2302]
                       end
                   end,
                   0);
        25393 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2303 = Acc,
                       _@2304 = 193,
                       _@2305 = Last,
                       begin
                           _@2306 = 14 bsl 4 + (_@2304 bsr 4),
                           _@2307 =
                               2 bsl 6 + (_@2304 band 15 bsl 2)
                               +
                               (_@2305 bsr 6),
                           _@2308 = 2 bsl 6 + _@2305 band 63,
                           [_@2303, _@2306, _@2307, _@2308]
                       end
                   end,
                   0);
        25394 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2309 = Acc,
                       _@2310 = 194,
                       _@2311 = Last,
                       begin
                           _@2312 = 14 bsl 4 + (_@2310 bsr 4),
                           _@2313 =
                               2 bsl 6 + (_@2310 band 15 bsl 2)
                               +
                               (_@2311 bsr 6),
                           _@2314 = 2 bsl 6 + _@2311 band 63,
                           [_@2309, _@2312, _@2313, _@2314]
                       end
                   end,
                   0);
        25395 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2315 = Acc,
                       _@2316 = 195,
                       _@2317 = Last,
                       begin
                           _@2318 = 14 bsl 4 + (_@2316 bsr 4),
                           _@2319 =
                               2 bsl 6 + (_@2316 band 15 bsl 2)
                               +
                               (_@2317 bsr 6),
                           _@2320 = 2 bsl 6 + _@2317 band 63,
                           [_@2315, _@2318, _@2319, _@2320]
                       end
                   end,
                   0);
        25396 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2321 = Acc,
                       _@2322 = 196,
                       _@2323 = Last,
                       begin
                           _@2324 = 14 bsl 4 + (_@2322 bsr 4),
                           _@2325 =
                               2 bsl 6 + (_@2322 band 15 bsl 2)
                               +
                               (_@2323 bsr 6),
                           _@2326 = 2 bsl 6 + _@2323 band 63,
                           [_@2321, _@2324, _@2325, _@2326]
                       end
                   end,
                   0);
        25397 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2327 = Acc,
                       _@2328 = 197,
                       _@2329 = Last,
                       begin
                           _@2330 = 14 bsl 4 + (_@2328 bsr 4),
                           _@2331 =
                               2 bsl 6 + (_@2328 band 15 bsl 2)
                               +
                               (_@2329 bsr 6),
                           _@2332 = 2 bsl 6 + _@2329 band 63,
                           [_@2327, _@2330, _@2331, _@2332]
                       end
                   end,
                   0);
        25398 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2333 = Acc,
                       _@2334 = 198,
                       _@2335 = Last,
                       begin
                           _@2336 = 14 bsl 4 + (_@2334 bsr 4),
                           _@2337 =
                               2 bsl 6 + (_@2334 band 15 bsl 2)
                               +
                               (_@2335 bsr 6),
                           _@2338 = 2 bsl 6 + _@2335 band 63,
                           [_@2333, _@2336, _@2337, _@2338]
                       end
                   end,
                   0);
        25399 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2339 = Acc,
                       _@2340 = 199,
                       _@2341 = Last,
                       begin
                           _@2342 = 14 bsl 4 + (_@2340 bsr 4),
                           _@2343 =
                               2 bsl 6 + (_@2340 band 15 bsl 2)
                               +
                               (_@2341 bsr 6),
                           _@2344 = 2 bsl 6 + _@2341 band 63,
                           [_@2339, _@2342, _@2343, _@2344]
                       end
                   end,
                   0);
        25400 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2345 = Acc,
                       _@2346 = 200,
                       _@2347 = Last,
                       begin
                           _@2348 = 14 bsl 4 + (_@2346 bsr 4),
                           _@2349 =
                               2 bsl 6 + (_@2346 band 15 bsl 2)
                               +
                               (_@2347 bsr 6),
                           _@2350 = 2 bsl 6 + _@2347 band 63,
                           [_@2345, _@2348, _@2349, _@2350]
                       end
                   end,
                   0);
        25401 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2351 = Acc,
                       _@2352 = 201,
                       _@2353 = Last,
                       begin
                           _@2354 = 14 bsl 4 + (_@2352 bsr 4),
                           _@2355 =
                               2 bsl 6 + (_@2352 band 15 bsl 2)
                               +
                               (_@2353 bsr 6),
                           _@2356 = 2 bsl 6 + _@2353 band 63,
                           [_@2351, _@2354, _@2355, _@2356]
                       end
                   end,
                   0);
        25409 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2357 = Acc,
                       _@2358 = 202,
                       _@2359 = Last,
                       begin
                           _@2360 = 14 bsl 4 + (_@2358 bsr 4),
                           _@2361 =
                               2 bsl 6 + (_@2358 band 15 bsl 2)
                               +
                               (_@2359 bsr 6),
                           _@2362 = 2 bsl 6 + _@2359 band 63,
                           [_@2357, _@2360, _@2361, _@2362]
                       end
                   end,
                   0);
        25410 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2363 = Acc,
                       _@2364 = 203,
                       _@2365 = Last,
                       begin
                           _@2366 = 14 bsl 4 + (_@2364 bsr 4),
                           _@2367 =
                               2 bsl 6 + (_@2364 band 15 bsl 2)
                               +
                               (_@2365 bsr 6),
                           _@2368 = 2 bsl 6 + _@2365 band 63,
                           [_@2363, _@2366, _@2367, _@2368]
                       end
                   end,
                   0);
        25411 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2369 = Acc,
                       _@2370 = 204,
                       _@2371 = Last,
                       begin
                           _@2372 = 14 bsl 4 + (_@2370 bsr 4),
                           _@2373 =
                               2 bsl 6 + (_@2370 band 15 bsl 2)
                               +
                               (_@2371 bsr 6),
                           _@2374 = 2 bsl 6 + _@2371 band 63,
                           [_@2369, _@2372, _@2373, _@2374]
                       end
                   end,
                   0);
        25412 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2375 = Acc,
                       _@2376 = 205,
                       _@2377 = Last,
                       begin
                           _@2378 = 14 bsl 4 + (_@2376 bsr 4),
                           _@2379 =
                               2 bsl 6 + (_@2376 band 15 bsl 2)
                               +
                               (_@2377 bsr 6),
                           _@2380 = 2 bsl 6 + _@2377 band 63,
                           [_@2375, _@2378, _@2379, _@2380]
                       end
                   end,
                   0);
        25413 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2381 = Acc,
                       _@2382 = 206,
                       _@2383 = Last,
                       begin
                           _@2384 = 14 bsl 4 + (_@2382 bsr 4),
                           _@2385 =
                               2 bsl 6 + (_@2382 band 15 bsl 2)
                               +
                               (_@2383 bsr 6),
                           _@2386 = 2 bsl 6 + _@2383 band 63,
                           [_@2381, _@2384, _@2385, _@2386]
                       end
                   end,
                   0);
        25414 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2387 = Acc,
                       _@2388 = 207,
                       _@2389 = Last,
                       begin
                           _@2390 = 14 bsl 4 + (_@2388 bsr 4),
                           _@2391 =
                               2 bsl 6 + (_@2388 band 15 bsl 2)
                               +
                               (_@2389 bsr 6),
                           _@2392 = 2 bsl 6 + _@2389 band 63,
                           [_@2387, _@2390, _@2391, _@2392]
                       end
                   end,
                   0);
        25441 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2393 = Acc,
                       _@2394 = 202,
                       _@2395 = Last,
                       begin
                           _@2396 = 14 bsl 4 + (_@2394 bsr 4),
                           _@2397 =
                               2 bsl 6 + (_@2394 band 15 bsl 2)
                               +
                               (_@2395 bsr 6),
                           _@2398 = 2 bsl 6 + _@2395 band 63,
                           [_@2393, _@2396, _@2397, _@2398]
                       end
                   end,
                   0);
        25442 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2399 = Acc,
                       _@2400 = 203,
                       _@2401 = Last,
                       begin
                           _@2402 = 14 bsl 4 + (_@2400 bsr 4),
                           _@2403 =
                               2 bsl 6 + (_@2400 band 15 bsl 2)
                               +
                               (_@2401 bsr 6),
                           _@2404 = 2 bsl 6 + _@2401 band 63,
                           [_@2399, _@2402, _@2403, _@2404]
                       end
                   end,
                   0);
        25443 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2405 = Acc,
                       _@2406 = 204,
                       _@2407 = Last,
                       begin
                           _@2408 = 14 bsl 4 + (_@2406 bsr 4),
                           _@2409 =
                               2 bsl 6 + (_@2406 band 15 bsl 2)
                               +
                               (_@2407 bsr 6),
                           _@2410 = 2 bsl 6 + _@2407 band 63,
                           [_@2405, _@2408, _@2409, _@2410]
                       end
                   end,
                   0);
        25444 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2411 = Acc,
                       _@2412 = 205,
                       _@2413 = Last,
                       begin
                           _@2414 = 14 bsl 4 + (_@2412 bsr 4),
                           _@2415 =
                               2 bsl 6 + (_@2412 band 15 bsl 2)
                               +
                               (_@2413 bsr 6),
                           _@2416 = 2 bsl 6 + _@2413 band 63,
                           [_@2411, _@2414, _@2415, _@2416]
                       end
                   end,
                   0);
        25445 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2417 = Acc,
                       _@2418 = 206,
                       _@2419 = Last,
                       begin
                           _@2420 = 14 bsl 4 + (_@2418 bsr 4),
                           _@2421 =
                               2 bsl 6 + (_@2418 band 15 bsl 2)
                               +
                               (_@2419 bsr 6),
                           _@2422 = 2 bsl 6 + _@2419 band 63,
                           [_@2417, _@2420, _@2421, _@2422]
                       end
                   end,
                   0);
        25446 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2423 = Acc,
                       _@2424 = 207,
                       _@2425 = Last,
                       begin
                           _@2426 = 14 bsl 4 + (_@2424 bsr 4),
                           _@2427 =
                               2 bsl 6 + (_@2424 band 15 bsl 2)
                               +
                               (_@2425 bsr 6),
                           _@2428 = 2 bsl 6 + _@2425 band 63,
                           [_@2423, _@2426, _@2427, _@2428]
                       end
                   end,
                   0);
        25648 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2429 = Acc,
                       _@2430 = 208,
                       _@2431 = Last,
                       begin
                           _@2432 = 14 bsl 4 + (_@2430 bsr 4),
                           _@2433 =
                               2 bsl 6 + (_@2430 band 15 bsl 2)
                               +
                               (_@2431 bsr 6),
                           _@2434 = 2 bsl 6 + _@2431 band 63,
                           [_@2429, _@2432, _@2433, _@2434]
                       end
                   end,
                   0);
        25649 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2435 = Acc,
                       _@2436 = 209,
                       _@2437 = Last,
                       begin
                           _@2438 = 14 bsl 4 + (_@2436 bsr 4),
                           _@2439 =
                               2 bsl 6 + (_@2436 band 15 bsl 2)
                               +
                               (_@2437 bsr 6),
                           _@2440 = 2 bsl 6 + _@2437 band 63,
                           [_@2435, _@2438, _@2439, _@2440]
                       end
                   end,
                   0);
        25650 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2441 = Acc,
                       _@2442 = 210,
                       _@2443 = Last,
                       begin
                           _@2444 = 14 bsl 4 + (_@2442 bsr 4),
                           _@2445 =
                               2 bsl 6 + (_@2442 band 15 bsl 2)
                               +
                               (_@2443 bsr 6),
                           _@2446 = 2 bsl 6 + _@2443 band 63,
                           [_@2441, _@2444, _@2445, _@2446]
                       end
                   end,
                   0);
        25651 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2447 = Acc,
                       _@2448 = 211,
                       _@2449 = Last,
                       begin
                           _@2450 = 14 bsl 4 + (_@2448 bsr 4),
                           _@2451 =
                               2 bsl 6 + (_@2448 band 15 bsl 2)
                               +
                               (_@2449 bsr 6),
                           _@2452 = 2 bsl 6 + _@2449 band 63,
                           [_@2447, _@2450, _@2451, _@2452]
                       end
                   end,
                   0);
        25652 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2453 = Acc,
                       _@2454 = 212,
                       _@2455 = Last,
                       begin
                           _@2456 = 14 bsl 4 + (_@2454 bsr 4),
                           _@2457 =
                               2 bsl 6 + (_@2454 band 15 bsl 2)
                               +
                               (_@2455 bsr 6),
                           _@2458 = 2 bsl 6 + _@2455 band 63,
                           [_@2453, _@2456, _@2457, _@2458]
                       end
                   end,
                   0);
        25653 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2459 = Acc,
                       _@2460 = 213,
                       _@2461 = Last,
                       begin
                           _@2462 = 14 bsl 4 + (_@2460 bsr 4),
                           _@2463 =
                               2 bsl 6 + (_@2460 band 15 bsl 2)
                               +
                               (_@2461 bsr 6),
                           _@2464 = 2 bsl 6 + _@2461 band 63,
                           [_@2459, _@2462, _@2463, _@2464]
                       end
                   end,
                   0);
        25654 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2465 = Acc,
                       _@2466 = 214,
                       _@2467 = Last,
                       begin
                           _@2468 = 14 bsl 4 + (_@2466 bsr 4),
                           _@2469 =
                               2 bsl 6 + (_@2466 band 15 bsl 2)
                               +
                               (_@2467 bsr 6),
                           _@2470 = 2 bsl 6 + _@2467 band 63,
                           [_@2465, _@2468, _@2469, _@2470]
                       end
                   end,
                   0);
        25655 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2471 = Acc,
                       _@2472 = 215,
                       _@2473 = Last,
                       begin
                           _@2474 = 14 bsl 4 + (_@2472 bsr 4),
                           _@2475 =
                               2 bsl 6 + (_@2472 band 15 bsl 2)
                               +
                               (_@2473 bsr 6),
                           _@2476 = 2 bsl 6 + _@2473 band 63,
                           [_@2471, _@2474, _@2475, _@2476]
                       end
                   end,
                   0);
        25656 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@2477 = 216,
                                 _@2478 = Last,
                                 65536
                                 +
                                 (_@2477 band 3 bsl 8 + _@2478 bsl 10)
                             end);
        25657 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@2479 = 217,
                                 _@2480 = Last,
                                 65536
                                 +
                                 (_@2479 band 3 bsl 8 + _@2480 bsl 10)
                             end);
        25665 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@2481 = 218,
                                 _@2482 = Last,
                                 65536
                                 +
                                 (_@2481 band 3 bsl 8 + _@2482 bsl 10)
                             end);
        25666 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@2483 = 219,
                                 _@2484 = Last,
                                 65536
                                 +
                                 (_@2483 band 3 bsl 8 + _@2484 bsl 10)
                             end);
        25697 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@2485 = 218,
                                 _@2486 = Last,
                                 65536
                                 +
                                 (_@2485 band 3 bsl 8 + _@2486 bsl 10)
                             end);
        25698 ->
            escape_surrogate(Rest, Input, Skip, Stack,
                             StringDecode, Acc,
                             begin
                                 _@2487 = 219,
                                 _@2488 = Last,
                                 65536
                                 +
                                 (_@2487 band 3 bsl 8 + _@2488 bsl 10)
                             end);
        25904 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2489 = Acc,
                       _@2490 = 224,
                       _@2491 = Last,
                       begin
                           _@2492 = 14 bsl 4 + (_@2490 bsr 4),
                           _@2493 =
                               2 bsl 6 + (_@2490 band 15 bsl 2)
                               +
                               (_@2491 bsr 6),
                           _@2494 = 2 bsl 6 + _@2491 band 63,
                           [_@2489, _@2492, _@2493, _@2494]
                       end
                   end,
                   0);
        25905 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2495 = Acc,
                       _@2496 = 225,
                       _@2497 = Last,
                       begin
                           _@2498 = 14 bsl 4 + (_@2496 bsr 4),
                           _@2499 =
                               2 bsl 6 + (_@2496 band 15 bsl 2)
                               +
                               (_@2497 bsr 6),
                           _@2500 = 2 bsl 6 + _@2497 band 63,
                           [_@2495, _@2498, _@2499, _@2500]
                       end
                   end,
                   0);
        25906 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2501 = Acc,
                       _@2502 = 226,
                       _@2503 = Last,
                       begin
                           _@2504 = 14 bsl 4 + (_@2502 bsr 4),
                           _@2505 =
                               2 bsl 6 + (_@2502 band 15 bsl 2)
                               +
                               (_@2503 bsr 6),
                           _@2506 = 2 bsl 6 + _@2503 band 63,
                           [_@2501, _@2504, _@2505, _@2506]
                       end
                   end,
                   0);
        25907 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2507 = Acc,
                       _@2508 = 227,
                       _@2509 = Last,
                       begin
                           _@2510 = 14 bsl 4 + (_@2508 bsr 4),
                           _@2511 =
                               2 bsl 6 + (_@2508 band 15 bsl 2)
                               +
                               (_@2509 bsr 6),
                           _@2512 = 2 bsl 6 + _@2509 band 63,
                           [_@2507, _@2510, _@2511, _@2512]
                       end
                   end,
                   0);
        25908 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2513 = Acc,
                       _@2514 = 228,
                       _@2515 = Last,
                       begin
                           _@2516 = 14 bsl 4 + (_@2514 bsr 4),
                           _@2517 =
                               2 bsl 6 + (_@2514 band 15 bsl 2)
                               +
                               (_@2515 bsr 6),
                           _@2518 = 2 bsl 6 + _@2515 band 63,
                           [_@2513, _@2516, _@2517, _@2518]
                       end
                   end,
                   0);
        25909 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2519 = Acc,
                       _@2520 = 229,
                       _@2521 = Last,
                       begin
                           _@2522 = 14 bsl 4 + (_@2520 bsr 4),
                           _@2523 =
                               2 bsl 6 + (_@2520 band 15 bsl 2)
                               +
                               (_@2521 bsr 6),
                           _@2524 = 2 bsl 6 + _@2521 band 63,
                           [_@2519, _@2522, _@2523, _@2524]
                       end
                   end,
                   0);
        25910 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2525 = Acc,
                       _@2526 = 230,
                       _@2527 = Last,
                       begin
                           _@2528 = 14 bsl 4 + (_@2526 bsr 4),
                           _@2529 =
                               2 bsl 6 + (_@2526 band 15 bsl 2)
                               +
                               (_@2527 bsr 6),
                           _@2530 = 2 bsl 6 + _@2527 band 63,
                           [_@2525, _@2528, _@2529, _@2530]
                       end
                   end,
                   0);
        25911 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2531 = Acc,
                       _@2532 = 231,
                       _@2533 = Last,
                       begin
                           _@2534 = 14 bsl 4 + (_@2532 bsr 4),
                           _@2535 =
                               2 bsl 6 + (_@2532 band 15 bsl 2)
                               +
                               (_@2533 bsr 6),
                           _@2536 = 2 bsl 6 + _@2533 band 63,
                           [_@2531, _@2534, _@2535, _@2536]
                       end
                   end,
                   0);
        25912 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2537 = Acc,
                       _@2538 = 232,
                       _@2539 = Last,
                       begin
                           _@2540 = 14 bsl 4 + (_@2538 bsr 4),
                           _@2541 =
                               2 bsl 6 + (_@2538 band 15 bsl 2)
                               +
                               (_@2539 bsr 6),
                           _@2542 = 2 bsl 6 + _@2539 band 63,
                           [_@2537, _@2540, _@2541, _@2542]
                       end
                   end,
                   0);
        25913 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2543 = Acc,
                       _@2544 = 233,
                       _@2545 = Last,
                       begin
                           _@2546 = 14 bsl 4 + (_@2544 bsr 4),
                           _@2547 =
                               2 bsl 6 + (_@2544 band 15 bsl 2)
                               +
                               (_@2545 bsr 6),
                           _@2548 = 2 bsl 6 + _@2545 band 63,
                           [_@2543, _@2546, _@2547, _@2548]
                       end
                   end,
                   0);
        25921 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2549 = Acc,
                       _@2550 = 234,
                       _@2551 = Last,
                       begin
                           _@2552 = 14 bsl 4 + (_@2550 bsr 4),
                           _@2553 =
                               2 bsl 6 + (_@2550 band 15 bsl 2)
                               +
                               (_@2551 bsr 6),
                           _@2554 = 2 bsl 6 + _@2551 band 63,
                           [_@2549, _@2552, _@2553, _@2554]
                       end
                   end,
                   0);
        25922 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2555 = Acc,
                       _@2556 = 235,
                       _@2557 = Last,
                       begin
                           _@2558 = 14 bsl 4 + (_@2556 bsr 4),
                           _@2559 =
                               2 bsl 6 + (_@2556 band 15 bsl 2)
                               +
                               (_@2557 bsr 6),
                           _@2560 = 2 bsl 6 + _@2557 band 63,
                           [_@2555, _@2558, _@2559, _@2560]
                       end
                   end,
                   0);
        25923 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2561 = Acc,
                       _@2562 = 236,
                       _@2563 = Last,
                       begin
                           _@2564 = 14 bsl 4 + (_@2562 bsr 4),
                           _@2565 =
                               2 bsl 6 + (_@2562 band 15 bsl 2)
                               +
                               (_@2563 bsr 6),
                           _@2566 = 2 bsl 6 + _@2563 band 63,
                           [_@2561, _@2564, _@2565, _@2566]
                       end
                   end,
                   0);
        25924 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2567 = Acc,
                       _@2568 = 237,
                       _@2569 = Last,
                       begin
                           _@2570 = 14 bsl 4 + (_@2568 bsr 4),
                           _@2571 =
                               2 bsl 6 + (_@2568 band 15 bsl 2)
                               +
                               (_@2569 bsr 6),
                           _@2572 = 2 bsl 6 + _@2569 band 63,
                           [_@2567, _@2570, _@2571, _@2572]
                       end
                   end,
                   0);
        25925 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2573 = Acc,
                       _@2574 = 238,
                       _@2575 = Last,
                       begin
                           _@2576 = 14 bsl 4 + (_@2574 bsr 4),
                           _@2577 =
                               2 bsl 6 + (_@2574 band 15 bsl 2)
                               +
                               (_@2575 bsr 6),
                           _@2578 = 2 bsl 6 + _@2575 band 63,
                           [_@2573, _@2576, _@2577, _@2578]
                       end
                   end,
                   0);
        25926 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2579 = Acc,
                       _@2580 = 239,
                       _@2581 = Last,
                       begin
                           _@2582 = 14 bsl 4 + (_@2580 bsr 4),
                           _@2583 =
                               2 bsl 6 + (_@2580 band 15 bsl 2)
                               +
                               (_@2581 bsr 6),
                           _@2584 = 2 bsl 6 + _@2581 band 63,
                           [_@2579, _@2582, _@2583, _@2584]
                       end
                   end,
                   0);
        25953 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2585 = Acc,
                       _@2586 = 234,
                       _@2587 = Last,
                       begin
                           _@2588 = 14 bsl 4 + (_@2586 bsr 4),
                           _@2589 =
                               2 bsl 6 + (_@2586 band 15 bsl 2)
                               +
                               (_@2587 bsr 6),
                           _@2590 = 2 bsl 6 + _@2587 band 63,
                           [_@2585, _@2588, _@2589, _@2590]
                       end
                   end,
                   0);
        25954 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2591 = Acc,
                       _@2592 = 235,
                       _@2593 = Last,
                       begin
                           _@2594 = 14 bsl 4 + (_@2592 bsr 4),
                           _@2595 =
                               2 bsl 6 + (_@2592 band 15 bsl 2)
                               +
                               (_@2593 bsr 6),
                           _@2596 = 2 bsl 6 + _@2593 band 63,
                           [_@2591, _@2594, _@2595, _@2596]
                       end
                   end,
                   0);
        25955 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2597 = Acc,
                       _@2598 = 236,
                       _@2599 = Last,
                       begin
                           _@2600 = 14 bsl 4 + (_@2598 bsr 4),
                           _@2601 =
                               2 bsl 6 + (_@2598 band 15 bsl 2)
                               +
                               (_@2599 bsr 6),
                           _@2602 = 2 bsl 6 + _@2599 band 63,
                           [_@2597, _@2600, _@2601, _@2602]
                       end
                   end,
                   0);
        25956 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2603 = Acc,
                       _@2604 = 237,
                       _@2605 = Last,
                       begin
                           _@2606 = 14 bsl 4 + (_@2604 bsr 4),
                           _@2607 =
                               2 bsl 6 + (_@2604 band 15 bsl 2)
                               +
                               (_@2605 bsr 6),
                           _@2608 = 2 bsl 6 + _@2605 band 63,
                           [_@2603, _@2606, _@2607, _@2608]
                       end
                   end,
                   0);
        25957 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2609 = Acc,
                       _@2610 = 238,
                       _@2611 = Last,
                       begin
                           _@2612 = 14 bsl 4 + (_@2610 bsr 4),
                           _@2613 =
                               2 bsl 6 + (_@2610 band 15 bsl 2)
                               +
                               (_@2611 bsr 6),
                           _@2614 = 2 bsl 6 + _@2611 band 63,
                           [_@2609, _@2612, _@2613, _@2614]
                       end
                   end,
                   0);
        25958 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2615 = Acc,
                       _@2616 = 239,
                       _@2617 = Last,
                       begin
                           _@2618 = 14 bsl 4 + (_@2616 bsr 4),
                           _@2619 =
                               2 bsl 6 + (_@2616 band 15 bsl 2)
                               +
                               (_@2617 bsr 6),
                           _@2620 = 2 bsl 6 + _@2617 band 63,
                           [_@2615, _@2618, _@2619, _@2620]
                       end
                   end,
                   0);
        26160 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2621 = Acc,
                       _@2622 = 240,
                       _@2623 = Last,
                       begin
                           _@2624 = 14 bsl 4 + (_@2622 bsr 4),
                           _@2625 =
                               2 bsl 6 + (_@2622 band 15 bsl 2)
                               +
                               (_@2623 bsr 6),
                           _@2626 = 2 bsl 6 + _@2623 band 63,
                           [_@2621, _@2624, _@2625, _@2626]
                       end
                   end,
                   0);
        26161 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2627 = Acc,
                       _@2628 = 241,
                       _@2629 = Last,
                       begin
                           _@2630 = 14 bsl 4 + (_@2628 bsr 4),
                           _@2631 =
                               2 bsl 6 + (_@2628 band 15 bsl 2)
                               +
                               (_@2629 bsr 6),
                           _@2632 = 2 bsl 6 + _@2629 band 63,
                           [_@2627, _@2630, _@2631, _@2632]
                       end
                   end,
                   0);
        26162 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2633 = Acc,
                       _@2634 = 242,
                       _@2635 = Last,
                       begin
                           _@2636 = 14 bsl 4 + (_@2634 bsr 4),
                           _@2637 =
                               2 bsl 6 + (_@2634 band 15 bsl 2)
                               +
                               (_@2635 bsr 6),
                           _@2638 = 2 bsl 6 + _@2635 band 63,
                           [_@2633, _@2636, _@2637, _@2638]
                       end
                   end,
                   0);
        26163 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2639 = Acc,
                       _@2640 = 243,
                       _@2641 = Last,
                       begin
                           _@2642 = 14 bsl 4 + (_@2640 bsr 4),
                           _@2643 =
                               2 bsl 6 + (_@2640 band 15 bsl 2)
                               +
                               (_@2641 bsr 6),
                           _@2644 = 2 bsl 6 + _@2641 band 63,
                           [_@2639, _@2642, _@2643, _@2644]
                       end
                   end,
                   0);
        26164 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2645 = Acc,
                       _@2646 = 244,
                       _@2647 = Last,
                       begin
                           _@2648 = 14 bsl 4 + (_@2646 bsr 4),
                           _@2649 =
                               2 bsl 6 + (_@2646 band 15 bsl 2)
                               +
                               (_@2647 bsr 6),
                           _@2650 = 2 bsl 6 + _@2647 band 63,
                           [_@2645, _@2648, _@2649, _@2650]
                       end
                   end,
                   0);
        26165 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2651 = Acc,
                       _@2652 = 245,
                       _@2653 = Last,
                       begin
                           _@2654 = 14 bsl 4 + (_@2652 bsr 4),
                           _@2655 =
                               2 bsl 6 + (_@2652 band 15 bsl 2)
                               +
                               (_@2653 bsr 6),
                           _@2656 = 2 bsl 6 + _@2653 band 63,
                           [_@2651, _@2654, _@2655, _@2656]
                       end
                   end,
                   0);
        26166 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2657 = Acc,
                       _@2658 = 246,
                       _@2659 = Last,
                       begin
                           _@2660 = 14 bsl 4 + (_@2658 bsr 4),
                           _@2661 =
                               2 bsl 6 + (_@2658 band 15 bsl 2)
                               +
                               (_@2659 bsr 6),
                           _@2662 = 2 bsl 6 + _@2659 band 63,
                           [_@2657, _@2660, _@2661, _@2662]
                       end
                   end,
                   0);
        26167 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2663 = Acc,
                       _@2664 = 247,
                       _@2665 = Last,
                       begin
                           _@2666 = 14 bsl 4 + (_@2664 bsr 4),
                           _@2667 =
                               2 bsl 6 + (_@2664 band 15 bsl 2)
                               +
                               (_@2665 bsr 6),
                           _@2668 = 2 bsl 6 + _@2665 band 63,
                           [_@2663, _@2666, _@2667, _@2668]
                       end
                   end,
                   0);
        26168 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2669 = Acc,
                       _@2670 = 248,
                       _@2671 = Last,
                       begin
                           _@2672 = 14 bsl 4 + (_@2670 bsr 4),
                           _@2673 =
                               2 bsl 6 + (_@2670 band 15 bsl 2)
                               +
                               (_@2671 bsr 6),
                           _@2674 = 2 bsl 6 + _@2671 band 63,
                           [_@2669, _@2672, _@2673, _@2674]
                       end
                   end,
                   0);
        26169 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2675 = Acc,
                       _@2676 = 249,
                       _@2677 = Last,
                       begin
                           _@2678 = 14 bsl 4 + (_@2676 bsr 4),
                           _@2679 =
                               2 bsl 6 + (_@2676 band 15 bsl 2)
                               +
                               (_@2677 bsr 6),
                           _@2680 = 2 bsl 6 + _@2677 band 63,
                           [_@2675, _@2678, _@2679, _@2680]
                       end
                   end,
                   0);
        26177 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2681 = Acc,
                       _@2682 = 250,
                       _@2683 = Last,
                       begin
                           _@2684 = 14 bsl 4 + (_@2682 bsr 4),
                           _@2685 =
                               2 bsl 6 + (_@2682 band 15 bsl 2)
                               +
                               (_@2683 bsr 6),
                           _@2686 = 2 bsl 6 + _@2683 band 63,
                           [_@2681, _@2684, _@2685, _@2686]
                       end
                   end,
                   0);
        26178 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2687 = Acc,
                       _@2688 = 251,
                       _@2689 = Last,
                       begin
                           _@2690 = 14 bsl 4 + (_@2688 bsr 4),
                           _@2691 =
                               2 bsl 6 + (_@2688 band 15 bsl 2)
                               +
                               (_@2689 bsr 6),
                           _@2692 = 2 bsl 6 + _@2689 band 63,
                           [_@2687, _@2690, _@2691, _@2692]
                       end
                   end,
                   0);
        26179 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2693 = Acc,
                       _@2694 = 252,
                       _@2695 = Last,
                       begin
                           _@2696 = 14 bsl 4 + (_@2694 bsr 4),
                           _@2697 =
                               2 bsl 6 + (_@2694 band 15 bsl 2)
                               +
                               (_@2695 bsr 6),
                           _@2698 = 2 bsl 6 + _@2695 band 63,
                           [_@2693, _@2696, _@2697, _@2698]
                       end
                   end,
                   0);
        26180 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2699 = Acc,
                       _@2700 = 253,
                       _@2701 = Last,
                       begin
                           _@2702 = 14 bsl 4 + (_@2700 bsr 4),
                           _@2703 =
                               2 bsl 6 + (_@2700 band 15 bsl 2)
                               +
                               (_@2701 bsr 6),
                           _@2704 = 2 bsl 6 + _@2701 band 63,
                           [_@2699, _@2702, _@2703, _@2704]
                       end
                   end,
                   0);
        26181 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2705 = Acc,
                       _@2706 = 254,
                       _@2707 = Last,
                       begin
                           _@2708 = 14 bsl 4 + (_@2706 bsr 4),
                           _@2709 =
                               2 bsl 6 + (_@2706 band 15 bsl 2)
                               +
                               (_@2707 bsr 6),
                           _@2710 = 2 bsl 6 + _@2707 band 63,
                           [_@2705, _@2708, _@2709, _@2710]
                       end
                   end,
                   0);
        26182 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2711 = Acc,
                       _@2712 = 255,
                       _@2713 = Last,
                       begin
                           _@2714 = 14 bsl 4 + (_@2712 bsr 4),
                           _@2715 =
                               2 bsl 6 + (_@2712 band 15 bsl 2)
                               +
                               (_@2713 bsr 6),
                           _@2716 = 2 bsl 6 + _@2713 band 63,
                           [_@2711, _@2714, _@2715, _@2716]
                       end
                   end,
                   0);
        26209 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2717 = Acc,
                       _@2718 = 250,
                       _@2719 = Last,
                       begin
                           _@2720 = 14 bsl 4 + (_@2718 bsr 4),
                           _@2721 =
                               2 bsl 6 + (_@2718 band 15 bsl 2)
                               +
                               (_@2719 bsr 6),
                           _@2722 = 2 bsl 6 + _@2719 band 63,
                           [_@2717, _@2720, _@2721, _@2722]
                       end
                   end,
                   0);
        26210 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2723 = Acc,
                       _@2724 = 251,
                       _@2725 = Last,
                       begin
                           _@2726 = 14 bsl 4 + (_@2724 bsr 4),
                           _@2727 =
                               2 bsl 6 + (_@2724 band 15 bsl 2)
                               +
                               (_@2725 bsr 6),
                           _@2728 = 2 bsl 6 + _@2725 band 63,
                           [_@2723, _@2726, _@2727, _@2728]
                       end
                   end,
                   0);
        26211 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2729 = Acc,
                       _@2730 = 252,
                       _@2731 = Last,
                       begin
                           _@2732 = 14 bsl 4 + (_@2730 bsr 4),
                           _@2733 =
                               2 bsl 6 + (_@2730 band 15 bsl 2)
                               +
                               (_@2731 bsr 6),
                           _@2734 = 2 bsl 6 + _@2731 band 63,
                           [_@2729, _@2732, _@2733, _@2734]
                       end
                   end,
                   0);
        26212 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2735 = Acc,
                       _@2736 = 253,
                       _@2737 = Last,
                       begin
                           _@2738 = 14 bsl 4 + (_@2736 bsr 4),
                           _@2739 =
                               2 bsl 6 + (_@2736 band 15 bsl 2)
                               +
                               (_@2737 bsr 6),
                           _@2740 = 2 bsl 6 + _@2737 band 63,
                           [_@2735, _@2738, _@2739, _@2740]
                       end
                   end,
                   0);
        26213 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2741 = Acc,
                       _@2742 = 254,
                       _@2743 = Last,
                       begin
                           _@2744 = 14 bsl 4 + (_@2742 bsr 4),
                           _@2745 =
                               2 bsl 6 + (_@2742 band 15 bsl 2)
                               +
                               (_@2743 bsr 6),
                           _@2746 = 2 bsl 6 + _@2743 band 63,
                           [_@2741, _@2744, _@2745, _@2746]
                       end
                   end,
                   0);
        26214 ->
            string(Rest, Input,
                   Skip + 6,
                   Stack, StringDecode,
                   begin
                       _@2747 = Acc,
                       _@2748 = 255,
                       _@2749 = Last,
                       begin
                           _@2750 = 14 bsl 4 + (_@2748 bsr 4),
                           _@2751 =
                               2 bsl 6 + (_@2748 band 15 bsl 2)
                               +
                               (_@2749 bsr 6),
                           _@2752 = 2 bsl 6 + _@2749 band 63,
                           [_@2747, _@2750, _@2751, _@2752]
                       end
                   end,
                   0);
        _ ->
            token_error(Input, Skip, 6)
    end;
escapeu(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, _Acc) ->
    empty_error(Input, Skip).

escapeu_last(Int, Input, Skip) ->
    case Int of
        12336 -> 0;
        12337 -> 1;
        12338 -> 2;
        12339 -> 3;
        12340 -> 4;
        12341 -> 5;
        12342 -> 6;
        12343 -> 7;
        12344 -> 8;
        12345 -> 9;
        12353 -> 10;
        12354 -> 11;
        12355 -> 12;
        12356 -> 13;
        12357 -> 14;
        12358 -> 15;
        12385 -> 10;
        12386 -> 11;
        12387 -> 12;
        12388 -> 13;
        12389 -> 14;
        12390 -> 15;
        12592 -> 16;
        12593 -> 17;
        12594 -> 18;
        12595 -> 19;
        12596 -> 20;
        12597 -> 21;
        12598 -> 22;
        12599 -> 23;
        12600 -> 24;
        12601 -> 25;
        12609 -> 26;
        12610 -> 27;
        12611 -> 28;
        12612 -> 29;
        12613 -> 30;
        12614 -> 31;
        12641 -> 26;
        12642 -> 27;
        12643 -> 28;
        12644 -> 29;
        12645 -> 30;
        12646 -> 31;
        12848 -> 32;
        12849 -> 33;
        12850 -> 34;
        12851 -> 35;
        12852 -> 36;
        12853 -> 37;
        12854 -> 38;
        12855 -> 39;
        12856 -> 40;
        12857 -> 41;
        12865 -> 42;
        12866 -> 43;
        12867 -> 44;
        12868 -> 45;
        12869 -> 46;
        12870 -> 47;
        12897 -> 42;
        12898 -> 43;
        12899 -> 44;
        12900 -> 45;
        12901 -> 46;
        12902 -> 47;
        13104 -> 48;
        13105 -> 49;
        13106 -> 50;
        13107 -> 51;
        13108 -> 52;
        13109 -> 53;
        13110 -> 54;
        13111 -> 55;
        13112 -> 56;
        13113 -> 57;
        13121 -> 58;
        13122 -> 59;
        13123 -> 60;
        13124 -> 61;
        13125 -> 62;
        13126 -> 63;
        13153 -> 58;
        13154 -> 59;
        13155 -> 60;
        13156 -> 61;
        13157 -> 62;
        13158 -> 63;
        13360 -> 64;
        13361 -> 65;
        13362 -> 66;
        13363 -> 67;
        13364 -> 68;
        13365 -> 69;
        13366 -> 70;
        13367 -> 71;
        13368 -> 72;
        13369 -> 73;
        13377 -> 74;
        13378 -> 75;
        13379 -> 76;
        13380 -> 77;
        13381 -> 78;
        13382 -> 79;
        13409 -> 74;
        13410 -> 75;
        13411 -> 76;
        13412 -> 77;
        13413 -> 78;
        13414 -> 79;
        13616 -> 80;
        13617 -> 81;
        13618 -> 82;
        13619 -> 83;
        13620 -> 84;
        13621 -> 85;
        13622 -> 86;
        13623 -> 87;
        13624 -> 88;
        13625 -> 89;
        13633 -> 90;
        13634 -> 91;
        13635 -> 92;
        13636 -> 93;
        13637 -> 94;
        13638 -> 95;
        13665 -> 90;
        13666 -> 91;
        13667 -> 92;
        13668 -> 93;
        13669 -> 94;
        13670 -> 95;
        13872 -> 96;
        13873 -> 97;
        13874 -> 98;
        13875 -> 99;
        13876 -> 100;
        13877 -> 101;
        13878 -> 102;
        13879 -> 103;
        13880 -> 104;
        13881 -> 105;
        13889 -> 106;
        13890 -> 107;
        13891 -> 108;
        13892 -> 109;
        13893 -> 110;
        13894 -> 111;
        13921 -> 106;
        13922 -> 107;
        13923 -> 108;
        13924 -> 109;
        13925 -> 110;
        13926 -> 111;
        14128 -> 112;
        14129 -> 113;
        14130 -> 114;
        14131 -> 115;
        14132 -> 116;
        14133 -> 117;
        14134 -> 118;
        14135 -> 119;
        14136 -> 120;
        14137 -> 121;
        14145 -> 122;
        14146 -> 123;
        14147 -> 124;
        14148 -> 125;
        14149 -> 126;
        14150 -> 127;
        14177 -> 122;
        14178 -> 123;
        14179 -> 124;
        14180 -> 125;
        14181 -> 126;
        14182 -> 127;
        14384 -> 128;
        14385 -> 129;
        14386 -> 130;
        14387 -> 131;
        14388 -> 132;
        14389 -> 133;
        14390 -> 134;
        14391 -> 135;
        14392 -> 136;
        14393 -> 137;
        14401 -> 138;
        14402 -> 139;
        14403 -> 140;
        14404 -> 141;
        14405 -> 142;
        14406 -> 143;
        14433 -> 138;
        14434 -> 139;
        14435 -> 140;
        14436 -> 141;
        14437 -> 142;
        14438 -> 143;
        14640 -> 144;
        14641 -> 145;
        14642 -> 146;
        14643 -> 147;
        14644 -> 148;
        14645 -> 149;
        14646 -> 150;
        14647 -> 151;
        14648 -> 152;
        14649 -> 153;
        14657 -> 154;
        14658 -> 155;
        14659 -> 156;
        14660 -> 157;
        14661 -> 158;
        14662 -> 159;
        14689 -> 154;
        14690 -> 155;
        14691 -> 156;
        14692 -> 157;
        14693 -> 158;
        14694 -> 159;
        16688 -> 160;
        16689 -> 161;
        16690 -> 162;
        16691 -> 163;
        16692 -> 164;
        16693 -> 165;
        16694 -> 166;
        16695 -> 167;
        16696 -> 168;
        16697 -> 169;
        16705 -> 170;
        16706 -> 171;
        16707 -> 172;
        16708 -> 173;
        16709 -> 174;
        16710 -> 175;
        16737 -> 170;
        16738 -> 171;
        16739 -> 172;
        16740 -> 173;
        16741 -> 174;
        16742 -> 175;
        16944 -> 176;
        16945 -> 177;
        16946 -> 178;
        16947 -> 179;
        16948 -> 180;
        16949 -> 181;
        16950 -> 182;
        16951 -> 183;
        16952 -> 184;
        16953 -> 185;
        16961 -> 186;
        16962 -> 187;
        16963 -> 188;
        16964 -> 189;
        16965 -> 190;
        16966 -> 191;
        16993 -> 186;
        16994 -> 187;
        16995 -> 188;
        16996 -> 189;
        16997 -> 190;
        16998 -> 191;
        17200 -> 192;
        17201 -> 193;
        17202 -> 194;
        17203 -> 195;
        17204 -> 196;
        17205 -> 197;
        17206 -> 198;
        17207 -> 199;
        17208 -> 200;
        17209 -> 201;
        17217 -> 202;
        17218 -> 203;
        17219 -> 204;
        17220 -> 205;
        17221 -> 206;
        17222 -> 207;
        17249 -> 202;
        17250 -> 203;
        17251 -> 204;
        17252 -> 205;
        17253 -> 206;
        17254 -> 207;
        17456 -> 208;
        17457 -> 209;
        17458 -> 210;
        17459 -> 211;
        17460 -> 212;
        17461 -> 213;
        17462 -> 214;
        17463 -> 215;
        17464 -> 216;
        17465 -> 217;
        17473 -> 218;
        17474 -> 219;
        17475 -> 220;
        17476 -> 221;
        17477 -> 222;
        17478 -> 223;
        17505 -> 218;
        17506 -> 219;
        17507 -> 220;
        17508 -> 221;
        17509 -> 222;
        17510 -> 223;
        17712 -> 224;
        17713 -> 225;
        17714 -> 226;
        17715 -> 227;
        17716 -> 228;
        17717 -> 229;
        17718 -> 230;
        17719 -> 231;
        17720 -> 232;
        17721 -> 233;
        17729 -> 234;
        17730 -> 235;
        17731 -> 236;
        17732 -> 237;
        17733 -> 238;
        17734 -> 239;
        17761 -> 234;
        17762 -> 235;
        17763 -> 236;
        17764 -> 237;
        17765 -> 238;
        17766 -> 239;
        17968 -> 240;
        17969 -> 241;
        17970 -> 242;
        17971 -> 243;
        17972 -> 244;
        17973 -> 245;
        17974 -> 246;
        17975 -> 247;
        17976 -> 248;
        17977 -> 249;
        17985 -> 250;
        17986 -> 251;
        17987 -> 252;
        17988 -> 253;
        17989 -> 254;
        17990 -> 255;
        18017 -> 250;
        18018 -> 251;
        18019 -> 252;
        18020 -> 253;
        18021 -> 254;
        18022 -> 255;
        24880 -> 160;
        24881 -> 161;
        24882 -> 162;
        24883 -> 163;
        24884 -> 164;
        24885 -> 165;
        24886 -> 166;
        24887 -> 167;
        24888 -> 168;
        24889 -> 169;
        24897 -> 170;
        24898 -> 171;
        24899 -> 172;
        24900 -> 173;
        24901 -> 174;
        24902 -> 175;
        24929 -> 170;
        24930 -> 171;
        24931 -> 172;
        24932 -> 173;
        24933 -> 174;
        24934 -> 175;
        25136 -> 176;
        25137 -> 177;
        25138 -> 178;
        25139 -> 179;
        25140 -> 180;
        25141 -> 181;
        25142 -> 182;
        25143 -> 183;
        25144 -> 184;
        25145 -> 185;
        25153 -> 186;
        25154 -> 187;
        25155 -> 188;
        25156 -> 189;
        25157 -> 190;
        25158 -> 191;
        25185 -> 186;
        25186 -> 187;
        25187 -> 188;
        25188 -> 189;
        25189 -> 190;
        25190 -> 191;
        25392 -> 192;
        25393 -> 193;
        25394 -> 194;
        25395 -> 195;
        25396 -> 196;
        25397 -> 197;
        25398 -> 198;
        25399 -> 199;
        25400 -> 200;
        25401 -> 201;
        25409 -> 202;
        25410 -> 203;
        25411 -> 204;
        25412 -> 205;
        25413 -> 206;
        25414 -> 207;
        25441 -> 202;
        25442 -> 203;
        25443 -> 204;
        25444 -> 205;
        25445 -> 206;
        25446 -> 207;
        25648 -> 208;
        25649 -> 209;
        25650 -> 210;
        25651 -> 211;
        25652 -> 212;
        25653 -> 213;
        25654 -> 214;
        25655 -> 215;
        25656 -> 216;
        25657 -> 217;
        25665 -> 218;
        25666 -> 219;
        25667 -> 220;
        25668 -> 221;
        25669 -> 222;
        25670 -> 223;
        25697 -> 218;
        25698 -> 219;
        25699 -> 220;
        25700 -> 221;
        25701 -> 222;
        25702 -> 223;
        25904 -> 224;
        25905 -> 225;
        25906 -> 226;
        25907 -> 227;
        25908 -> 228;
        25909 -> 229;
        25910 -> 230;
        25911 -> 231;
        25912 -> 232;
        25913 -> 233;
        25921 -> 234;
        25922 -> 235;
        25923 -> 236;
        25924 -> 237;
        25925 -> 238;
        25926 -> 239;
        25953 -> 234;
        25954 -> 235;
        25955 -> 236;
        25956 -> 237;
        25957 -> 238;
        25958 -> 239;
        26160 -> 240;
        26161 -> 241;
        26162 -> 242;
        26163 -> 243;
        26164 -> 244;
        26165 -> 245;
        26166 -> 246;
        26167 -> 247;
        26168 -> 248;
        26169 -> 249;
        26177 -> 250;
        26178 -> 251;
        26179 -> 252;
        26180 -> 253;
        26181 -> 254;
        26182 -> 255;
        26209 -> 250;
        26210 -> 251;
        26211 -> 252;
        26212 -> 253;
        26213 -> 254;
        26214 -> 255;
        _ -> token_error(Input, Skip, 6)
    end.

key(Data, Input, Skip, Stack, StringDecode) ->
    case Data of
        <<$\t/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode);
        <<$\n/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode);
        <<$\r/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode);
        <<$\s/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode);
        <<$"/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 1, [?key | Stack], StringDecode, 0);
        <<$}/integer,Rest/bitstring>> ->
            case Stack of
                [[] | Stack2] ->
                    continue(Rest, Input, Skip + 1, Stack2, StringDecode, #{});
                _ ->
                    throw_error(Input, Skip)
            end;
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip);
        <<_/bitstring>> ->
            empty_error(Input, Skip)
    end.

key(Data, Input, Skip, Stack, StringDecode, Value) ->
    case Data of
        <<$\t/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$\n/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$\r/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$\s/integer,Rest/bitstring>> ->
            key(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<$:/integer,Rest/bitstring>> ->
            value(Rest, Input, Skip + 1, [?object, Value | Stack], StringDecode);
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip);
        <<_/bitstring>> ->
            empty_error(Input, Skip)
    end.

number(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number(<<$./integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len) ->
    number_frac(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number(<<E/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when E =:= $e orelse E =:= $E ->
    Prefix = binary_part(Input, Skip, Len),
    number_exp_copy(Rest, Input, Skip + Len + 1, Stack, StringDecode, Prefix);
number(<<Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len) ->
    Int = binary_to_integer(binary_part(Input, Skip, Len)),
    continue(Rest, Input, Skip + Len, Stack, StringDecode, Int).

number_exp(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_exp(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when Byte =:= $+ orelse Byte =:= $- ->
    number_exp_sign(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_exp(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, Len) ->
    throw_error(Input, Skip + Len).

number_exp_cont(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_exp_cont(<<Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len) ->
    Token = binary_part(Input, Skip, Len),
    Float = try_parse_float(Token, Token, Skip),
    continue(Rest, Input, Skip + Len, Stack, StringDecode, Float).

number_exp_cont(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Prefix, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Input, Skip, Stack, StringDecode, Prefix, Len + 1);
number_exp_cont(<<Rest/bitstring>>, Input, Skip, Stack, StringDecode, Prefix, Len) ->
    Suffix = binary_part(Input, Skip, Len),
    String = <<Prefix/binary,".0e",Suffix/binary>>,
    PrefixSize = byte_size(Prefix),
    InitialSkip = Skip - PrefixSize - 1,
    FinalSkip = Skip + Len,
    Token = binary_part(Input, InitialSkip, PrefixSize + Len + 1),
    Float = try_parse_float(String, Token, InitialSkip),
    continue(Rest, Input, FinalSkip, Stack, StringDecode, Float).

number_exp_copy(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Prefix)
when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Input, Skip, Stack, StringDecode, Prefix, 1);
number_exp_copy(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Prefix)
when Byte =:= $+ orelse Byte =:= $- ->
    number_exp_sign(Rest, Input, Skip, Stack, StringDecode, Prefix, 1);
number_exp_copy(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, _Prefix) ->
    throw_error(Input, Skip).

number_exp_sign(<<_byte@1/integer,Rest/bitstring>>,
                Input, Skip, Stack, StringDecode,
                Len)
    when
        ((((((((_byte@1 =:= 48
                orelse
                _byte@1 =:= 49)
               orelse
               _byte@1 =:= 50)
              orelse
              _byte@1 =:= 51)
             orelse
             _byte@1 =:= 52)
            orelse
            _byte@1 =:= 53)
           orelse
           _byte@1 =:= 54)
          orelse
          _byte@1 =:= 55)
         orelse
         _byte@1 =:= 56)
        orelse
        _byte@1 =:= 57 ->
    number_exp_cont(Rest, Input, Skip, Stack,
                    StringDecode,
                    Len + 1);
number_exp_sign(<<_Rest/bitstring>>,
                Input, Skip, _Stack, _StringDecode,
                Len) ->
    throw_error(Input, Skip + Len).

number_exp_sign(<<_byte@1/integer,Rest/bitstring>>,
                Input, Skip, Stack, StringDecode,
                _prefix@1, Len)
    when
        ((((((((_byte@1 =:= 48
                orelse
                _byte@1 =:= 49)
               orelse
               _byte@1 =:= 50)
              orelse
              _byte@1 =:= 51)
             orelse
             _byte@1 =:= 52)
            orelse
            _byte@1 =:= 53)
           orelse
           _byte@1 =:= 54)
          orelse
          _byte@1 =:= 55)
         orelse
         _byte@1 =:= 56)
        orelse
        _byte@1 =:= 57 ->
    number_exp_cont(Rest, Input, Skip, Stack,
                    StringDecode, _prefix@1,
                    Len + 1);
number_exp_sign(<<_Rest/bitstring>>,
                Input, Skip, _Stack, _StringDecode,
                __prefix@1, Len) ->
    throw_error(Input, Skip + Len).

number_frac(<<_byte@1/integer,Rest/bitstring>>,
            Input, Skip, Stack, StringDecode, Len)
    when
        ((((((((_byte@1 =:= 48
                orelse
                _byte@1 =:= 49)
               orelse
               _byte@1 =:= 50)
              orelse
              _byte@1 =:= 51)
             orelse
             _byte@1 =:= 52)
            orelse
            _byte@1 =:= 53)
           orelse
           _byte@1 =:= 54)
          orelse
          _byte@1 =:= 55)
         orelse
         _byte@1 =:= 56)
        orelse
        _byte@1 =:= 57 ->
    number_frac_cont(Rest, Input, Skip, Stack,
                     StringDecode,
                     Len + 1);
number_frac(<<_Rest/bitstring>>,
            Input, Skip, _Stack, _StringDecode, Len) ->
    throw_error(Input, Skip + Len).

number_frac_cont(<<_byte@1/integer,Rest/bitstring>>,
                 Input, Skip, Stack, StringDecode,
                 Len)
    when
        ((((((((_byte@1 =:= 48
                orelse
                _byte@1 =:= 49)
               orelse
               _byte@1 =:= 50)
              orelse
              _byte@1 =:= 51)
             orelse
             _byte@1 =:= 52)
            orelse
            _byte@1 =:= 53)
           orelse
           _byte@1 =:= 54)
          orelse
          _byte@1 =:= 55)
         orelse
         _byte@1 =:= 56)
        orelse
        _byte@1 =:= 57 ->
    number_frac_cont(Rest, Input, Skip, Stack,
                     StringDecode,
                     Len + 1);
number_frac_cont(<<_e@1/integer,Rest/bitstring>>,
                 Input, Skip, Stack, StringDecode,
                 Len)
    when
        _e@1 =:= 101
        orelse
        _e@1 =:= 69 ->
    number_exp(Rest, Input, Skip, Stack,
               StringDecode,
               Len + 1);
number_frac_cont(<<Rest/bitstring>>,
                 Input, Skip, Stack, StringDecode,
                 Len) ->
    _token@1 = binary_part(Input, Skip, Len),
    _float@1 = try_parse_float(_token@1, _token@1, Skip),
    continue(Rest, Input,
             Skip + Len,
             Stack, StringDecode, _float@1).

number_minus(<<48/integer,Rest/bitstring>>,
             Input, Skip, Stack, StringDecode) ->
    number_zero(Rest, Input, Skip, Stack,
                StringDecode, 2);
number_minus(<<_byte@1/integer,Rest/bitstring>>,
             Input, Skip, Stack, StringDecode)
    when
        (((((((_byte@1 =:= 49
               orelse
               _byte@1 =:= 50)
              orelse
              _byte@1 =:= 51)
             orelse
             _byte@1 =:= 52)
            orelse
            _byte@1 =:= 53)
           orelse
           _byte@1 =:= 54)
          orelse
          _byte@1 =:= 55)
         orelse
         _byte@1 =:= 56)
        orelse
        _byte@1 =:= 57 ->
    number(Rest, Input, Skip, Stack, StringDecode, 2);
number_minus(<<_Rest/bitstring>>,
             Input, Skip, _Stack, _StringDecode) ->
    throw_error(Input, Skip + 1).

number_zero(<<46/integer,Rest/bitstring>>,
            Input, Skip, Stack, StringDecode, Len) ->
    number_frac(Rest, Input, Skip, Stack,
                StringDecode,
                Len + 1);
number_zero(<<_e@1/integer,Rest/bitstring>>,
            Input, Skip, Stack, StringDecode, Len)
    when
        _e@1 =:= 101
        orelse
        _e@1 =:= 69 ->
    number_exp_copy(Rest, Input,
                    Skip + Len + 1,
                    Stack, StringDecode,
                    <<"0">>);
number_zero(<<Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len) ->
    continue(Rest, Input, Skip + Len, Stack, StringDecode, 0).

object(Rest, Input, Skip, Stack, StringDecode) ->
    key(Rest, Input, Skip, [[] | Stack], StringDecode).

object(Data, Input, Skip, Stack, StringDecode, Value) ->
    case Data of
        <<9/integer,Rest/bitstring>> ->
            object(Rest, Input,
                   Skip + 1,
                   Stack, StringDecode, Value);
        <<10/integer,_rest@2/bitstring>> ->
            object(_rest@2, Input,
                   Skip + 1,
                   Stack, StringDecode, Value);
        <<13/integer,_rest@3/bitstring>> ->
            object(_rest@3, Input,
                   Skip + 1,
                   Stack, StringDecode, Value);
        <<32/integer,_rest@4/bitstring>> ->
            object(_rest@4, Input,
                   Skip + 1,
                   Stack, StringDecode, Value);
        <<44/integer,_rest@5/bitstring>> ->
            _skip@2 = Skip + 1,
            [_key@1, Acc | _stack@2] = Stack,
            _acc@2 = [{_key@1, Value} | Acc],
            key(_rest@5, Input, _skip@2,
                [_acc@2 | _stack@2],
                StringDecode);
        <<125/integer,_rest@6/bitstring>> ->
            _skip@3 = Skip + 1,
            [_key@2, _acc@3 | _stack@3] = Stack,
            _final@1 = [{_key@2, Value} | _acc@3],
            continue(_rest@6, Input, _skip@3, _stack@3,
                     StringDecode,
                     maps:from_list(_final@1));
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip);
        <<_/bitstring>> ->
            empty_error(Input, Skip)
    end.

parse(Data, _opts@1) when is_binary(Data) ->
    StringDecode = string_decode_function(_opts@1),
    try
        {ok, value(Data, Data, 0, [?terminate], StringDecode)}
    catch
        throw:{position, _position@1}:_ ->
            case _position@1 == byte_size(Data) of
                true ->
                    {error, unexpected_end_of_input};
                false ->
                    _byte@1 = binary:at(Data, _position@1),
                    _hex@1 = integer_to_binary(_byte@1, 16),
                    {error, {unexpected_byte, <<"0x"/utf8,_hex@1/binary>>, _position@1}}
            end;
        throw:{token, _token@1, _position@2}:_ ->
            {error, {unexpected_sequence, _token@1, _position@2}}
    end.

string(Data, Input, Skip, Stack, StringDecode, Len) ->
    case Data of
        <<X/integer,_Rest/bitstring>> when X < 32 ->
            throw_error(Input, Skip);
        <<X/integer,Rest/bitstring>> when X < 34 ->
            string(Rest, Input, Skip, Stack, StringDecode, Len + 1);
        <<34/integer,Rest/bitstring>> ->
            String = StringDecode(binary_part(Input, Skip, Len)),
            continue(Rest, Input, Skip + Len + 1, Stack, StringDecode, String);
        <<X/integer,Rest/bitstring>> when X < 92 ->
            string(Rest, Input, Skip, Stack, StringDecode, Len + 1);
        <<92/integer,Rest/bitstring>> ->
            Part = binary_part(Input, Skip, Len),
            escape(Rest, Input, Skip + Len, Stack, StringDecode, Part);
        <<X/integer,Rest/bitstring>> when X < 128 ->
            string(Rest, Input, Skip, Stack, StringDecode, Len + 1);
        <<Char/utf8,Rest/bitstring>> when Char =< 2047 ->
            string(Rest, Input, Skip, Stack, StringDecode, Len + 2);
        <<Char/utf8,Rest/bitstring>> when Char =< 65535 ->
            string(Rest, Input, Skip, Stack, StringDecode, Len + 3);
        <<_Char/utf8,Rest/bitstring>> ->
            string(Rest, Input, Skip, Stack, StringDecode, Len + 4);
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip);
        <<_/bitstring>> ->
            empty_error(Input, Skip + Len)
    end.

string(Data, Input, Skip, Stack, StringDecode, Acc, Len) ->
    case Data of
        <<0/integer,_Rest/bitstring>> ->
            throw_error(Input, Skip);
        <<1/integer,__rest@2/bitstring>> ->
            throw_error(Input, Skip);
        <<2/integer,__rest@3/bitstring>> ->
            throw_error(Input, Skip);
        <<3/integer,__rest@4/bitstring>> ->
            throw_error(Input, Skip);
        <<4/integer,__rest@5/bitstring>> ->
            throw_error(Input, Skip);
        <<5/integer,__rest@6/bitstring>> ->
            throw_error(Input, Skip);
        <<6/integer,__rest@7/bitstring>> ->
            throw_error(Input, Skip);
        <<7/integer,__rest@8/bitstring>> ->
            throw_error(Input, Skip);
        <<8/integer,__rest@9/bitstring>> ->
            throw_error(Input, Skip);
        <<9/integer,_Rest0/bitstring>> ->
            throw_error(Input, Skip);
        <<10/integer,_Rest1/bitstring>> ->
            throw_error(Input, Skip);
        <<11/integer,_Rest2/bitstring>> ->
            throw_error(Input, Skip);
        <<12/integer,_Rest3/bitstring>> ->
            throw_error(Input, Skip);
        <<13/integer,_Rest4/bitstring>> ->
            throw_error(Input, Skip);
        <<14/integer,_Rest5/bitstring>> ->
            throw_error(Input, Skip);
        <<15/integer,_Rest6/bitstring>> ->
            throw_error(Input, Skip);
        <<16/integer,_Rest7/bitstring>> ->
            throw_error(Input, Skip);
        <<17/integer,_Rest8/bitstring>> ->
            throw_error(Input, Skip);
        <<18/integer,_Rest9/bitstring>> ->
            throw_error(Input, Skip);
        <<19/integer,__rest@20/bitstring>> ->
            throw_error(Input, Skip);
        <<20/integer,__rest@21/bitstring>> ->
            throw_error(Input, Skip);
        <<21/integer,__rest@22/bitstring>> ->
            throw_error(Input, Skip);
        <<22/integer,__rest@23/bitstring>> ->
            throw_error(Input, Skip);
        <<23/integer,__rest@24/bitstring>> ->
            throw_error(Input, Skip);
        <<24/integer,__rest@25/bitstring>> ->
            throw_error(Input, Skip);
        <<25/integer,__rest@26/bitstring>> ->
            throw_error(Input, Skip);
        <<26/integer,__rest@27/bitstring>> ->
            throw_error(Input, Skip);
        <<27/integer,__rest@28/bitstring>> ->
            throw_error(Input, Skip);
        <<28/integer,__rest@29/bitstring>> ->
            throw_error(Input, Skip);
        <<29/integer,__rest@30/bitstring>> ->
            throw_error(Input, Skip);
        <<30/integer,__rest@31/bitstring>> ->
            throw_error(Input, Skip);
        <<31/integer,__rest@32/bitstring>> ->
            throw_error(Input, Skip);
        <<32/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<33/integer,_rest@2/bitstring>> ->
            string(_rest@2, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<34/integer,_rest@3/bitstring>> ->
            Last = binary_part(Input, Skip, Len),
            _string@1 = iolist_to_binary([Acc | Last]),
            continue(_rest@3, Input,
                     Skip + Len + 1,
                     Stack, StringDecode, _string@1);
        <<35/integer,_rest@4/bitstring>> ->
            string(_rest@4, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<36/integer,_rest@5/bitstring>> ->
            string(_rest@5, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<37/integer,_rest@6/bitstring>> ->
            string(_rest@6, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<38/integer,_rest@7/bitstring>> ->
            string(_rest@7, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<39/integer,_rest@8/bitstring>> ->
            string(_rest@8, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<40/integer,_rest@9/bitstring>> ->
            string(_rest@9, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<41/integer,Rest0/bitstring>> ->
            string(Rest0, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<42/integer,Rest1/bitstring>> ->
            string(Rest1, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<43/integer,Rest2/bitstring>> ->
            string(Rest2, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<44/integer,Rest3/bitstring>> ->
            string(Rest3, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<45/integer,Rest4/bitstring>> ->
            string(Rest4, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<46/integer,Rest5/bitstring>> ->
            string(Rest5, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<47/integer,Rest6/bitstring>> ->
            string(Rest6, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<48/integer,Rest7/bitstring>> ->
            string(Rest7, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<49/integer,Rest8/bitstring>> ->
            string(Rest8, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<50/integer,Rest9/bitstring>> ->
            string(Rest9, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<51/integer,_rest@20/bitstring>> ->
            string(_rest@20, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<52/integer,_rest@21/bitstring>> ->
            string(_rest@21, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<53/integer,_rest@22/bitstring>> ->
            string(_rest@22, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<54/integer,_rest@23/bitstring>> ->
            string(_rest@23, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<55/integer,_rest@24/bitstring>> ->
            string(_rest@24, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<56/integer,_rest@25/bitstring>> ->
            string(_rest@25, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<57/integer,_rest@26/bitstring>> ->
            string(_rest@26, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<58/integer,_rest@27/bitstring>> ->
            string(_rest@27, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<59/integer,_rest@28/bitstring>> ->
            string(_rest@28, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<60/integer,_rest@29/bitstring>> ->
            string(_rest@29, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<61/integer,_rest@30/bitstring>> ->
            string(_rest@30, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<62/integer,_rest@31/bitstring>> ->
            string(_rest@31, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<63/integer,_rest@32/bitstring>> ->
            string(_rest@32, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<64/integer,_rest@33/bitstring>> ->
            string(_rest@33, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<65/integer,_rest@34/bitstring>> ->
            string(_rest@34, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<66/integer,_rest@35/bitstring>> ->
            string(_rest@35, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<67/integer,_rest@36/bitstring>> ->
            string(_rest@36, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<68/integer,_rest@37/bitstring>> ->
            string(_rest@37, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<69/integer,_rest@38/bitstring>> ->
            string(_rest@38, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<70/integer,_rest@39/bitstring>> ->
            string(_rest@39, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<71/integer,_rest@40/bitstring>> ->
            string(_rest@40, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<72/integer,_rest@41/bitstring>> ->
            string(_rest@41, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<73/integer,_rest@42/bitstring>> ->
            string(_rest@42, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<74/integer,_rest@43/bitstring>> ->
            string(_rest@43, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<75/integer,_rest@44/bitstring>> ->
            string(_rest@44, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<76/integer,_rest@45/bitstring>> ->
            string(_rest@45, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<77/integer,_rest@46/bitstring>> ->
            string(_rest@46, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<78/integer,_rest@47/bitstring>> ->
            string(_rest@47, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<79/integer,_rest@48/bitstring>> ->
            string(_rest@48, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<80/integer,_rest@49/bitstring>> ->
            string(_rest@49, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<81/integer,_rest@50/bitstring>> ->
            string(_rest@50, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<82/integer,_rest@51/bitstring>> ->
            string(_rest@51, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<83/integer,_rest@52/bitstring>> ->
            string(_rest@52, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<84/integer,_rest@53/bitstring>> ->
            string(_rest@53, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<85/integer,_rest@54/bitstring>> ->
            string(_rest@54, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<86/integer,_rest@55/bitstring>> ->
            string(_rest@55, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<87/integer,_rest@56/bitstring>> ->
            string(_rest@56, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<88/integer,_rest@57/bitstring>> ->
            string(_rest@57, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<89/integer,_rest@58/bitstring>> ->
            string(_rest@58, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<90/integer,_rest@59/bitstring>> ->
            string(_rest@59, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<91/integer,_rest@60/bitstring>> ->
            string(_rest@60, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<92/integer,_rest@61/bitstring>> ->
            _part@1 = binary_part(Input, Skip, Len),
            escape(_rest@61, Input,
                   Skip + Len,
                   Stack, StringDecode,
                   [Acc | _part@1]);
        <<93/integer,_rest@62/bitstring>> ->
            string(_rest@62, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<94/integer,_rest@63/bitstring>> ->
            string(_rest@63, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<95/integer,_rest@64/bitstring>> ->
            string(_rest@64, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<96/integer,_rest@65/bitstring>> ->
            string(_rest@65, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<97/integer,_rest@66/bitstring>> ->
            string(_rest@66, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<98/integer,_rest@67/bitstring>> ->
            string(_rest@67, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<99/integer,_rest@68/bitstring>> ->
            string(_rest@68, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<100/integer,_rest@69/bitstring>> ->
            string(_rest@69, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<101/integer,_rest@70/bitstring>> ->
            string(_rest@70, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<102/integer,_rest@71/bitstring>> ->
            string(_rest@71, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<103/integer,_rest@72/bitstring>> ->
            string(_rest@72, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<104/integer,_rest@73/bitstring>> ->
            string(_rest@73, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<105/integer,_rest@74/bitstring>> ->
            string(_rest@74, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<106/integer,_rest@75/bitstring>> ->
            string(_rest@75, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<107/integer,_rest@76/bitstring>> ->
            string(_rest@76, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<108/integer,_rest@77/bitstring>> ->
            string(_rest@77, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<109/integer,_rest@78/bitstring>> ->
            string(_rest@78, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<110/integer,_rest@79/bitstring>> ->
            string(_rest@79, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<111/integer,_rest@80/bitstring>> ->
            string(_rest@80, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<112/integer,_rest@81/bitstring>> ->
            string(_rest@81, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<113/integer,_rest@82/bitstring>> ->
            string(_rest@82, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<114/integer,_rest@83/bitstring>> ->
            string(_rest@83, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<115/integer,_rest@84/bitstring>> ->
            string(_rest@84, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<116/integer,_rest@85/bitstring>> ->
            string(_rest@85, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<117/integer,_rest@86/bitstring>> ->
            string(_rest@86, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<118/integer,_rest@87/bitstring>> ->
            string(_rest@87, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<119/integer,_rest@88/bitstring>> ->
            string(_rest@88, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<120/integer,_rest@89/bitstring>> ->
            string(_rest@89, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<121/integer,_rest@90/bitstring>> ->
            string(_rest@90, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<122/integer,_rest@91/bitstring>> ->
            string(_rest@91, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<123/integer,_rest@92/bitstring>> ->
            string(_rest@92, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<124/integer,_rest@93/bitstring>> ->
            string(_rest@93, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<125/integer,_rest@94/bitstring>> ->
            string(_rest@94, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<126/integer,_rest@95/bitstring>> ->
            string(_rest@95, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<127/integer,_rest@96/bitstring>> ->
            string(_rest@96, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 1);
        <<_char@1/utf8,_rest@97/bitstring>> when _char@1 =< 2047 ->
            string(_rest@97, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 2);
        <<_char@2/utf8,_rest@98/bitstring>> when _char@2 =< 65535 ->
            string(_rest@98, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 3);
        <<__char@1/utf8,_rest@99/bitstring>> ->
            string(_rest@99, Input, Skip, Stack,
                   StringDecode, Acc,
                   Len + 4);
        <<_/bitstring>> ->
            empty_error(Input, Skip + Len)
    end.

string_decode_function(#{strings := copy}) ->
    fun binary:copy/1;
string_decode_function(#{strings := reference}) ->
    fun(_@1) ->
           _@1
    end.

terminate(<<_byte@1/integer,Rest/bitstring>>,
          Input, Skip, Stack, StringDecode, Value)
    when
        ((_byte@1 =:= 32
          orelse
          _byte@1 =:= 10)
         orelse
         _byte@1 =:= 13)
        orelse
        _byte@1 =:= 9 ->
    terminate(Rest, Input,
              Skip + 1,
              Stack, StringDecode, Value);
terminate(<<>>, _Input, _Skip, _Stack, _StringDecode,
          Value) ->
    Value;
terminate(<<_Rest/bitstring>>,
          Input, Skip, _Stack, _StringDecode, _Value) ->
    throw_error(Input, Skip).

throw_error(_Input, Skip) ->
    throw({position, Skip}).

throw_error(<<_Rest/bitstring>>,
            _Input, Skip, _Stack, _StringDecode) ->
    throw({position, Skip - 1}).

token_error(_token@1, _position@1) ->
    throw({token, _token@1, _position@1}).

token_error(_token@1, _position@1, Len) ->
    throw({token,
           binary_part(_token@1, _position@1, Len),
           _position@1}).

try_parse_float(_string@1, _token@1, Skip) ->
    try
        binary_to_float(_string@1)
    catch
        error:badarg:___STACKTRACE__@1 ->
            token_error(_token@1, Skip)
    end.

value(Data, Input, Skip, Stack, StringDecode) ->
    case Data of
        <<0/integer,Rest/bitstring>> ->
            throw_error(Rest, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<1/integer,_rest@2/bitstring>> ->
            throw_error(_rest@2, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<2/integer,_rest@3/bitstring>> ->
            throw_error(_rest@3, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<3/integer,_rest@4/bitstring>> ->
            throw_error(_rest@4, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<4/integer,_rest@5/bitstring>> ->
            throw_error(_rest@5, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<5/integer,_rest@6/bitstring>> ->
            throw_error(_rest@6, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<6/integer,_rest@7/bitstring>> ->
            throw_error(_rest@7, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<7/integer,_rest@8/bitstring>> ->
            throw_error(_rest@8, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<8/integer,_rest@9/bitstring>> ->
            throw_error(_rest@9, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<9/integer,Rest0/bitstring>> ->
            value(Rest0, Input,
                  Skip + 1,
                  Stack, StringDecode);
        <<10/integer,Rest1/bitstring>> ->
            value(Rest1, Input,
                  Skip + 1,
                  Stack, StringDecode);
        <<11/integer,Rest2/bitstring>> ->
            throw_error(Rest2, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<12/integer,Rest3/bitstring>> ->
            throw_error(Rest3, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<13/integer,Rest4/bitstring>> ->
            value(Rest4, Input,
                  Skip + 1,
                  Stack, StringDecode);
        <<14/integer,Rest5/bitstring>> ->
            throw_error(Rest5, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<15/integer,Rest6/bitstring>> ->
            throw_error(Rest6, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<16/integer,Rest7/bitstring>> ->
            throw_error(Rest7, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<17/integer,Rest8/bitstring>> ->
            throw_error(Rest8, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<18/integer,Rest9/bitstring>> ->
            throw_error(Rest9, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<19/integer,_rest@20/bitstring>> ->
            throw_error(_rest@20, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<20/integer,_rest@21/bitstring>> ->
            throw_error(_rest@21, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<21/integer,_rest@22/bitstring>> ->
            throw_error(_rest@22, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<22/integer,_rest@23/bitstring>> ->
            throw_error(_rest@23, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<23/integer,_rest@24/bitstring>> ->
            throw_error(_rest@24, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<24/integer,_rest@25/bitstring>> ->
            throw_error(_rest@25, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<25/integer,_rest@26/bitstring>> ->
            throw_error(_rest@26, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<26/integer,_rest@27/bitstring>> ->
            throw_error(_rest@27, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<27/integer,_rest@28/bitstring>> ->
            throw_error(_rest@28, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<28/integer,_rest@29/bitstring>> ->
            throw_error(_rest@29, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<29/integer,_rest@30/bitstring>> ->
            throw_error(_rest@30, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<30/integer,_rest@31/bitstring>> ->
            throw_error(_rest@31, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<31/integer,_rest@32/bitstring>> ->
            throw_error(_rest@32, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<32/integer,_rest@33/bitstring>> ->
            value(_rest@33, Input,
                  Skip + 1,
                  Stack, StringDecode);
        <<33/integer,_rest@34/bitstring>> ->
            throw_error(_rest@34, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<34/integer,_rest@35/bitstring>> ->
            string(_rest@35, Input,
                   Skip + 1,
                   Stack, StringDecode, 0);
        <<35/integer,_rest@36/bitstring>> ->
            throw_error(_rest@36, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<36/integer,_rest@37/bitstring>> ->
            throw_error(_rest@37, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<37/integer,_rest@38/bitstring>> ->
            throw_error(_rest@38, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<38/integer,_rest@39/bitstring>> ->
            throw_error(_rest@39, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<39/integer,_rest@40/bitstring>> ->
            throw_error(_rest@40, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<40/integer,_rest@41/bitstring>> ->
            throw_error(_rest@41, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<41/integer,_rest@42/bitstring>> ->
            throw_error(_rest@42, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<42/integer,_rest@43/bitstring>> ->
            throw_error(_rest@43, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<43/integer,_rest@44/bitstring>> ->
            throw_error(_rest@44, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<44/integer,_rest@45/bitstring>> ->
            throw_error(_rest@45, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<45/integer,_rest@46/bitstring>> ->
            number_minus(_rest@46, Input, Skip, Stack,
                         StringDecode);
        <<46/integer,_rest@47/bitstring>> ->
            throw_error(_rest@47, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<47/integer,_rest@48/bitstring>> ->
            throw_error(_rest@48, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<48/integer,_rest@49/bitstring>> ->
            number_zero(_rest@49, Input, Skip, Stack,
                        StringDecode, 1);
        <<49/integer,_rest@50/bitstring>> ->
            number(_rest@50, Input, Skip, Stack,
                   StringDecode, 1);
        <<50/integer,_rest@51/bitstring>> ->
            number(_rest@51, Input, Skip, Stack,
                   StringDecode, 1);
        <<51/integer,_rest@52/bitstring>> ->
            number(_rest@52, Input, Skip, Stack,
                   StringDecode, 1);
        <<52/integer,_rest@53/bitstring>> ->
            number(_rest@53, Input, Skip, Stack,
                   StringDecode, 1);
        <<53/integer,_rest@54/bitstring>> ->
            number(_rest@54, Input, Skip, Stack,
                   StringDecode, 1);
        <<54/integer,_rest@55/bitstring>> ->
            number(_rest@55, Input, Skip, Stack,
                   StringDecode, 1);
        <<55/integer,_rest@56/bitstring>> ->
            number(_rest@56, Input, Skip, Stack,
                   StringDecode, 1);
        <<56/integer,_rest@57/bitstring>> ->
            number(_rest@57, Input, Skip, Stack,
                   StringDecode, 1);
        <<57/integer,_rest@58/bitstring>> ->
            number(_rest@58, Input, Skip, Stack,
                   StringDecode, 1);
        <<58/integer,_rest@59/bitstring>> ->
            throw_error(_rest@59, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<59/integer,_rest@60/bitstring>> ->
            throw_error(_rest@60, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<60/integer,_rest@61/bitstring>> ->
            throw_error(_rest@61, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<61/integer,_rest@62/bitstring>> ->
            throw_error(_rest@62, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<62/integer,_rest@63/bitstring>> ->
            throw_error(_rest@63, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<63/integer,_rest@64/bitstring>> ->
            throw_error(_rest@64, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<64/integer,_rest@65/bitstring>> ->
            throw_error(_rest@65, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<65/integer,_rest@66/bitstring>> ->
            throw_error(_rest@66, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<66/integer,_rest@67/bitstring>> ->
            throw_error(_rest@67, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<67/integer,_rest@68/bitstring>> ->
            throw_error(_rest@68, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<68/integer,_rest@69/bitstring>> ->
            throw_error(_rest@69, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<69/integer,_rest@70/bitstring>> ->
            throw_error(_rest@70, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<70/integer,_rest@71/bitstring>> ->
            throw_error(_rest@71, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<71/integer,_rest@72/bitstring>> ->
            throw_error(_rest@72, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<72/integer,_rest@73/bitstring>> ->
            throw_error(_rest@73, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<73/integer,_rest@74/bitstring>> ->
            throw_error(_rest@74, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<74/integer,_rest@75/bitstring>> ->
            throw_error(_rest@75, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<75/integer,_rest@76/bitstring>> ->
            throw_error(_rest@76, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<76/integer,_rest@77/bitstring>> ->
            throw_error(_rest@77, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<77/integer,_rest@78/bitstring>> ->
            throw_error(_rest@78, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<78/integer,_rest@79/bitstring>> ->
            throw_error(_rest@79, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<79/integer,_rest@80/bitstring>> ->
            throw_error(_rest@80, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<80/integer,_rest@81/bitstring>> ->
            throw_error(_rest@81, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<81/integer,_rest@82/bitstring>> ->
            throw_error(_rest@82, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<82/integer,_rest@83/bitstring>> ->
            throw_error(_rest@83, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<83/integer,_rest@84/bitstring>> ->
            throw_error(_rest@84, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<84/integer,_rest@85/bitstring>> ->
            throw_error(_rest@85, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<85/integer,_rest@86/bitstring>> ->
            throw_error(_rest@86, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<86/integer,_rest@87/bitstring>> ->
            throw_error(_rest@87, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<87/integer,_rest@88/bitstring>> ->
            throw_error(_rest@88, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<88/integer,_rest@89/bitstring>> ->
            throw_error(_rest@89, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<89/integer,_rest@90/bitstring>> ->
            throw_error(_rest@90, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<90/integer,_rest@91/bitstring>> ->
            throw_error(_rest@91, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<91/integer,_rest@92/bitstring>> ->
            array(_rest@92, Input,
                  Skip + 1,
                  Stack, StringDecode);
        <<92/integer,_rest@93/bitstring>> ->
            throw_error(_rest@93, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<93/integer,_rest@94/bitstring>> ->
            empty_array(_rest@94, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<94/integer,_rest@95/bitstring>> ->
            throw_error(_rest@95, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<95/integer,_rest@96/bitstring>> ->
            throw_error(_rest@96, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<96/integer,_rest@97/bitstring>> ->
            throw_error(_rest@97, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<97/integer,_rest@98/bitstring>> ->
            throw_error(_rest@98, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<98/integer,_rest@99/bitstring>> ->
            throw_error(_rest@99, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<99/integer,Rest00/bitstring>> ->
            throw_error(Rest00, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<100/integer,Rest01/bitstring>> ->
            throw_error(Rest01, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<101/integer,Rest02/bitstring>> ->
            throw_error(Rest02, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<102/integer,Rest03/bitstring>> ->
            case Rest03 of
                <<"alse",Rest04/bitstring>> ->
                    continue(Rest04, Input,
                             Skip + 5,
                             Stack, StringDecode, false);
                <<_/bitstring>> ->
                    throw_error(Input, Skip)
            end;
        <<103/integer,Rest05/bitstring>> ->
            throw_error(Rest05, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<104/integer,Rest06/bitstring>> ->
            throw_error(Rest06, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<105/integer,Rest07/bitstring>> ->
            throw_error(Rest07, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<106/integer,Rest08/bitstring>> ->
            throw_error(Rest08, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<107/integer,Rest09/bitstring>> ->
            throw_error(Rest09, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<108/integer,Rest10/bitstring>> ->
            throw_error(Rest10, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<109/integer,Rest11/bitstring>> ->
            throw_error(Rest11, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<110/integer,Rest12/bitstring>> ->
            case Rest12 of
                <<"ull",Rest13/bitstring>> ->
                    continue(Rest13, Input,
                             Skip + 4,
                             Stack, StringDecode, nil);
                <<_/bitstring>> ->
                    throw_error(Input, Skip)
            end;
        <<111/integer,Rest14/bitstring>> ->
            throw_error(Rest14, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<112/integer,Rest15/bitstring>> ->
            throw_error(Rest15, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<113/integer,Rest16/bitstring>> ->
            throw_error(Rest16, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<114/integer,Rest17/bitstring>> ->
            throw_error(Rest17, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<115/integer,Rest18/bitstring>> ->
            throw_error(Rest18, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<116/integer,Rest19/bitstring>> ->
            case Rest19 of
                <<"rue",Rest20/bitstring>> ->
                    continue(Rest20, Input,
                             Skip + 4,
                             Stack, StringDecode, true);
                <<_/bitstring>> ->
                    throw_error(Input, Skip)
            end;
        <<117/integer,Rest21/bitstring>> ->
            throw_error(Rest21, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<118/integer,Rest22/bitstring>> ->
            throw_error(Rest22, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<119/integer,Rest23/bitstring>> ->
            throw_error(Rest23, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<120/integer,Rest24/bitstring>> ->
            throw_error(Rest24, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<121/integer,Rest25/bitstring>> ->
            throw_error(Rest25, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<122/integer,Rest26/bitstring>> ->
            throw_error(Rest26, Input,
                        Skip + 1,
                        Stack, StringDecode);
        <<123/integer,Rest27/bitstring>> ->
            object(Rest27, Input,
                   Skip + 1,
                   Stack, StringDecode);
        <<_/bitstring>> ->
            throw_error(Input, Skip)
    end.

