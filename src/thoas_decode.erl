-module(thoas_decode).

-compile([{inline, [
    {array, 6}, {object, 6}, {token_error, 3}, {token_error, 2}, 
    {throw_error, 2}, {continue, 6}
]}]).

-export([decode/2]).

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
    X = case Int1 of
        17475 -> 220;
        17476 -> 221;
        17477 -> 222;
        17478 -> 223;
        17507 -> 220;
        17508 -> 221;
        17509 -> 222;
        17510 -> 223;
        25667 -> 220;
        25668 -> 221;
        25669 -> 222;
        25670 -> 223;
        25699 -> 220;
        25700 -> 221;
        25701 -> 222;
        25702 -> 223;
        _ -> token_error(Input, Skip, 12)
    end,
    string(Rest, Input,
            Skip + 12,
            Stack, StringDecode,
            begin
                Y = X band 3 bsl 8 + Last,
                [Acc | <<(Hi + Y)/utf8>>]
            end,
        0);
escape_surrogate(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, _Acc, _Hi) ->
    throw_error(Input, Skip + 6).


escapeu_1(<<_/bitstring>> = Rest, Input, Skip, Stack, StringDecode, Acc, Last, X) ->
    A = 6 bsl 5 + (X bsl 2) + (Last bsr 6),
    B = 2 bsl 6 + Last band 63,
    C = [Acc, A, B],
    string(Rest, Input, Skip + 6, Stack, StringDecode, C, 0).

escapeu_2(<<_/bitstring>> = Rest, Input, Skip, Stack, StringDecode, Acc, Last, X) ->
    A = 14 bsl 4 + (X bsr 4),
    B = 2 bsl 6 + (X band 15 bsl 2) + (Last bsr 6),
    C = 2 bsl 6 + Last band 63,
    D = [Acc, A, B, C],
    string(Rest, Input, Skip + 6, Stack, StringDecode, D, 0).

escapeu(<<Int1:16/integer,Int2:16/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Acc) ->
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
        12337 -> escapeu_1(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 1);
        12338 -> escapeu_1(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 2);
        12339 -> escapeu_1(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 3);
        12340 -> escapeu_1(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 4);
        12341 -> escapeu_1(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 5);
        12342 -> escapeu_1(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 6);
        12343 -> escapeu_1(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 7);
        12344 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 8);
        12345 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 9);
        12353 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 10);
        12354 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 11);
        12355 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 12);
        12356 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 13);
        12357 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 14);
        12358 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 15);
        12385 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 10);
        12386 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 11);
        12387 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 12);
        12388 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 13);
        12389 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 14);
        12390 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 15);
        12592 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 16);
        12593 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 17);
        12594 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 18);
        12595 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 19);
        12596 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 20);
        12597 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 21);
        12598 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 22);
        12599 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 23);
        12600 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 24);
        12601 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 25);
        12609 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 26);
        12610 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 27);
        12611 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 28);
        12612 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 29);
        12613 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 30);
        12614 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 31);
        12641 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 26);
        12642 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 27);
        12643 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 28);
        12644 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 29);
        12645 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 30);
        12646 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 31);
        12848 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 32);
        12849 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 33);
        12850 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 34);
        12851 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 35);
        12852 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 36);
        12853 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 37);
        12854 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 38);
        12855 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 39);
        12856 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 40);
        12857 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 41);
        12865 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 42);
        12866 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 43);
        12867 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 44);
        12868 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 45);
        12869 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 46);
        12870 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 47);
        12897 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 42);
        12898 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 43);
        12899 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 44);
        12900 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 45);
        12901 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 46);
        12902 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 47);
        13104 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 48);
        13105 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 49);
        13106 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 50);
        13107 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 51);
        13108 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 52);
        13109 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 53);
        13110 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 54);
        13111 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 55);
        13112 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 56);
        13113 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 57);
        13121 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 58);
        13122 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 59);
        13123 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 60);
        13124 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 61);
        13125 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 62);
        13126 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 63);
        13153 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 58);
        13154 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 59);
        13155 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 60);
        13156 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 61);
        13157 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 62);
        13158 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 63);
        13360 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 64);
        13361 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 65);
        13362 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 66);
        13363 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 67);
        13364 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 68);
        13365 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 69);
        13366 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 70);
        13367 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 71);
        13368 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 72);
        13369 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 73);
        13377 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 74);
        13378 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 75);
        13379 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 76);
        13380 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 77);
        13381 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 78);
        13382 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 79);
        13409 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 74);
        13410 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 75);
        13411 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 76);
        13412 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 77);
        13413 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 78);
        13414 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 79);
        13616 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 80);
        13617 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 81);
        13618 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 82);
        13619 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 83);
        13620 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 84);
        13621 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 85);
        13622 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 86);
        13623 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 87);
        13624 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 88);
        13625 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 89);
        13633 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 90);
        13634 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 91);
        13635 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 92);
        13636 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 93);
        13637 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 94);
        13638 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 95);
        13665 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 90);
        13666 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 91);
        13667 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 92);
        13668 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 93);
        13669 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 94);
        13670 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 95);
        13872 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 96);
        13873 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 97);
        13874 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 98);
        13875 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 99);
        13876 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 100);
        13877 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 101);
        13878 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 102);
        13879 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 103);
        13880 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 104);
        13881 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 105);
        13889 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 106);
        13890 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 107);
        13891 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 108);
        13892 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 109);
        13893 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 110);
        13894 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 111);
        13921 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 106);
        13922 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 107);
        13923 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 108);
        13924 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 109);
        13925 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 110);
        13926 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 111);
        14128 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 112);
        14129 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 113);
        14130 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 114);
        14131 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 115);
        14132 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 116);
        14133 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 117);
        14134 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 118);
        14135 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 119);
        14136 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 120);
        14137 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 121);
        14145 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 122);
        14146 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 123);
        14147 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 124);
        14148 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 125);
        14149 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 126);
        14150 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 127);
        14177 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 122);
        14178 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 123);
        14179 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 124);
        14180 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 125);
        14181 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 126);
        14182 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 127);
        14384 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 128);
        14385 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 129);
        14386 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 130);
        14387 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 131);
        14388 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 132);
        14389 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 133);
        14390 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 134);
        14391 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 135);
        14392 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 136);
        14393 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 137);
        14401 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 138);
        14402 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 139);
        14403 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 140);
        14404 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 141);
        14405 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 142);
        14406 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 143);
        14433 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 138);
        14434 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 139);
        14435 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 140);
        14436 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 141);
        14437 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 142);
        14438 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 143);
        14640 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 144);
        14641 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 145);
        14642 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 146);
        14643 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 147);
        14644 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 148);
        14645 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 149);
        14646 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 150);
        14647 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 151);
        14648 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 152);
        14649 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 153);
        14657 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 154);
        14658 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 155);
        14659 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 156);
        14660 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 157);
        14661 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 158);
        14662 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 159);
        14689 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 154);
        14690 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 155);
        14691 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 156);
        14692 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 157);
        14693 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 158);
        14694 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 159);
        16688 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 160);
        16689 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 161);
        16690 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 162);
        16691 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 163);
        16692 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 164);
        16693 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 165);
        16694 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 166);
        16695 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 167);
        16696 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 168);
        16697 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 169);
        16705 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 170);
        16706 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 171);
        16707 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 172);
        16708 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 173);
        16709 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 174);
        16710 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 175);
        16737 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 170);
        16738 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 171);
        16739 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 172);
        16740 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 173);
        16741 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 174);
        16742 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 175);
        16944 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 176);
        16945 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 177);
        16946 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 178);
        16947 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 179);
        16948 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 180);
        16949 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 181);
        16950 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 182);
        16951 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 183);
        16952 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 184);
        16953 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 185);
        16961 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 186);
        16962 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 187);
        16963 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 188);
        16964 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 189);
        16965 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 190);
        16966 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 191);
        16993 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 186);
        16994 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 187);
        16995 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 188);
        16996 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 189);
        16997 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 190);
        16998 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 191);
        17200 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 192);
        17201 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 193);
        17202 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 194);
        17203 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 195);
        17204 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 196);
        17205 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 197);
        17206 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 198);
        17207 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 199);
        17208 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 200);
        17209 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 201);
        17217 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 202);
        17218 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 203);
        17219 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 204);
        17220 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 205);
        17221 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 206);
        17222 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 207);
        17249 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 202);
        17250 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 203);
        17251 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 204);
        17252 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 205);
        17253 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 206);
        17254 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 207);
        17456 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 208);
        17457 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 209);
        17458 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 210);
        17459 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 211);
        17460 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 212);
        17461 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 213);
        17462 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 214);
        17463 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 215);
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
        17712 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 224);
        17713 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 225);
        17714 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 226);
        17715 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 227);
        17716 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 228);
        17717 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 229);
        17718 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 230);
        17719 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 231);
        17720 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 232);
        17721 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 233);
        17729 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 234);
        17730 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 235);
        17731 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 236);
        17732 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 237);
        17733 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 238);
        17734 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 239);
        17761 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 234);
        17762 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 235);
        17763 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 236);
        17764 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 237);
        17765 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 238);
        17766 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 239);
        17968 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 240);
        17969 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 241);
        17970 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 242);
        17971 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 243);
        17972 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 244);
        17973 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 245);
        17974 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 246);
        17975 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 247);
        17976 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 248);
        17977 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 249);
        17985 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 250);
        17986 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 251);
        17987 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 252);
        17988 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 253);
        17989 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 254);
        17990 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 255);
        18017 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 250);
        18018 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 251);
        18019 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 252);
        18020 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 253);
        18021 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 254);
        18022 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 255);
        24880 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 160);
        24881 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 161);
        24882 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 162);
        24883 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 163);
        24884 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 164);
        24885 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 165);
        24886 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 166);
        24887 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 167);
        24888 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 168);
        24889 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 169);
        24897 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 170);
        24898 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 171);
        24899 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 172);
        24900 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 173);
        24901 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 174);
        24902 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 175);
        24929 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 170);
        24930 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 171);
        24931 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 172);
        24932 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 173);
        24933 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 174);
        24934 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 175);
        25136 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 176);
        25137 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 177);
        25138 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 178);
        25139 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 179);
        25140 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 180);
        25141 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 181);
        25142 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 182);
        25143 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 183);
        25144 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 184);
        25145 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 185);
        25153 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 186);
        25154 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 187);
        25155 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 188);
        25156 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 189);
        25157 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 190);
        25158 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 191);
        25185 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 186);
        25186 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 187);
        25187 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 188);
        25188 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 189);
        25189 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 190);
        25190 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 191);
        25392 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 192);
        25393 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 193);
        25394 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 194);
        25395 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 195);
        25396 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 196);
        25397 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 197);
        25398 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 198);
        25399 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 199);
        25400 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 200);
        25401 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 201);
        25409 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 202);
        25410 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 203);
        25411 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 204);
        25412 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 205);
        25413 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 206);
        25414 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 207);
        25441 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 202);
        25442 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 203);
        25443 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 204);
        25444 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 205);
        25445 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 206);
        25446 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 207);
        25648 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 208);
        25649 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 209);
        25650 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 210);
        25651 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 211);
        25652 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 212);
        25653 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 213);
        25654 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 214);
        25655 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 215);
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
        25904 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 224);
        25905 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 225);
        25906 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 226);
        25907 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 227);
        25908 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 228);
        25909 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 229);
        25910 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 230);
        25911 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 231);
        25912 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 232);
        25913 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 233);
        25921 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 234);
        25922 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 235);
        25923 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 236);
        25924 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 237);
        25925 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 238);
        25926 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 239);
        25953 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 234);
        25954 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 235);
        25955 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 236);
        25956 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 237);
        25957 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 238);
        25958 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 239);
        26160 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 240);
        26161 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 241);
        26162 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 242);
        26163 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 243);
        26164 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 244);
        26165 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 245);
        26166 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 246);
        26167 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 247);
        26168 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 248);
        26169 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 249);
        26177 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 250);
        26178 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 251);
        26179 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 252);
        26180 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 253);
        26181 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 254);
        26182 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 255);
        26209 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 250);
        26210 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 251);
        26211 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 252);
        26212 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 253);
        26213 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 254);
        26214 -> escapeu_2(Rest, Input, Skip, Stack, StringDecode, Acc, Last, 255);
        _ ->
            token_error(Input, Skip, 6)
    end;
escapeu(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, _Acc) ->
    empty_error(Input, Skip).

escapeu_last(Int, Input, Skip) ->
    case Int of
        12336 -> 0; 12337 -> 1; 12338 -> 2; 12339 -> 3; 12340 -> 4; 12341 -> 5; 12342 -> 6;
        12343 -> 7; 12344 -> 8; 12345 -> 9; 12353 -> 10; 12354 -> 11; 12355 -> 12;
        12356 -> 13; 12357 -> 14; 12358 -> 15; 12385 -> 10; 12386 -> 11; 12387 -> 12;
        12388 -> 13; 12389 -> 14; 12390 -> 15; 12592 -> 16; 12593 -> 17; 12594 -> 18;
        12595 -> 19; 12596 -> 20; 12597 -> 21; 12598 -> 22; 12599 -> 23; 12600 -> 24;
        12601 -> 25; 12609 -> 26; 12610 -> 27; 12611 -> 28; 12612 -> 29; 12613 -> 30;
        12614 -> 31; 12641 -> 26; 12642 -> 27; 12643 -> 28; 12644 -> 29; 12645 -> 30;
        12646 -> 31; 12848 -> 32; 12849 -> 33; 12850 -> 34; 12851 -> 35; 12852 -> 36;
        12853 -> 37; 12854 -> 38; 12855 -> 39; 12856 -> 40; 12857 -> 41; 12865 -> 42;
        12866 -> 43; 12867 -> 44; 12868 -> 45; 12869 -> 46; 12870 -> 47; 12897 -> 42;
        12898 -> 43; 12899 -> 44; 12900 -> 45; 12901 -> 46; 12902 -> 47; 13104 -> 48;
        13105 -> 49; 13106 -> 50; 13107 -> 51; 13108 -> 52; 13109 -> 53; 13110 -> 54;
        13111 -> 55; 13112 -> 56; 13113 -> 57; 13121 -> 58; 13122 -> 59; 13123 -> 60;
        13124 -> 61; 13125 -> 62; 13126 -> 63; 13153 -> 58; 13154 -> 59; 13155 -> 60;
        13156 -> 61; 13157 -> 62; 13158 -> 63; 13360 -> 64; 13361 -> 65; 13362 -> 66;
        13363 -> 67; 13364 -> 68; 13365 -> 69; 13366 -> 70; 13367 -> 71; 13368 -> 72;
        13369 -> 73; 13377 -> 74; 13378 -> 75; 13379 -> 76; 13380 -> 77; 13381 -> 78;
        13382 -> 79; 13409 -> 74; 13410 -> 75; 13411 -> 76; 13412 -> 77; 13413 -> 78;
        13414 -> 79; 13616 -> 80; 13617 -> 81; 13618 -> 82; 13619 -> 83; 13620 -> 84;
        13621 -> 85; 13622 -> 86; 13623 -> 87; 13624 -> 88; 13625 -> 89; 13633 -> 90;
        13634 -> 91; 13635 -> 92; 13636 -> 93; 13637 -> 94; 13638 -> 95; 13665 -> 90;
        13666 -> 91; 13667 -> 92; 13668 -> 93; 13669 -> 94; 13670 -> 95; 13872 -> 96;
        13873 -> 97; 13874 -> 98; 13875 -> 99; 13876 -> 100; 13877 -> 101; 13878 -> 102;
        13879 -> 103; 13880 -> 104; 13881 -> 105; 13889 -> 106; 13890 -> 107; 13891 -> 108;
        13892 -> 109; 13893 -> 110; 13894 -> 111; 13921 -> 106; 13922 -> 107; 13923 -> 108;
        13924 -> 109; 13925 -> 110; 13926 -> 111; 14128 -> 112; 14129 -> 113; 14130 -> 114;
        14131 -> 115; 14132 -> 116; 14133 -> 117; 14134 -> 118; 14135 -> 119; 14136 -> 120;
        14137 -> 121; 14145 -> 122; 14146 -> 123; 14147 -> 124; 14148 -> 125; 14149 -> 126;
        14150 -> 127; 14177 -> 122; 14178 -> 123; 14179 -> 124; 14180 -> 125; 14181 -> 126;
        14182 -> 127; 14384 -> 128; 14385 -> 129; 14386 -> 130; 14387 -> 131; 14388 -> 132;
        14389 -> 133; 14390 -> 134; 14391 -> 135; 14392 -> 136; 14393 -> 137; 14401 -> 138;
        14402 -> 139; 14403 -> 140; 14404 -> 141; 14405 -> 142; 14406 -> 143; 14433 -> 138;
        14434 -> 139; 14435 -> 140; 14436 -> 141; 14437 -> 142; 14438 -> 143; 14640 -> 144;
        14641 -> 145; 14642 -> 146; 14643 -> 147; 14644 -> 148; 14645 -> 149; 14646 -> 150;
        14647 -> 151; 14648 -> 152; 14649 -> 153; 14657 -> 154; 14658 -> 155; 14659 -> 156;
        14660 -> 157; 14661 -> 158; 14662 -> 159; 14689 -> 154; 14690 -> 155; 14691 -> 156;
        14692 -> 157; 14693 -> 158; 14694 -> 159; 16688 -> 160; 16689 -> 161; 16690 -> 162;
        16691 -> 163; 16692 -> 164; 16693 -> 165; 16694 -> 166; 16695 -> 167; 16696 -> 168;
        16697 -> 169; 16705 -> 170; 16706 -> 171; 16707 -> 172; 16708 -> 173; 16709 -> 174;
        16710 -> 175; 16737 -> 170; 16738 -> 171; 16739 -> 172; 16740 -> 173; 16741 -> 174;
        16742 -> 175; 16944 -> 176; 16945 -> 177; 16946 -> 178; 16947 -> 179; 16948 -> 180;
        16949 -> 181; 16950 -> 182; 16951 -> 183; 16952 -> 184; 16953 -> 185; 16961 -> 186;
        16962 -> 187; 16963 -> 188; 16964 -> 189; 16965 -> 190; 16966 -> 191; 16993 -> 186;
        16994 -> 187; 16995 -> 188; 16996 -> 189; 16997 -> 190; 16998 -> 191; 17200 -> 192;
        17201 -> 193; 17202 -> 194; 17203 -> 195; 17204 -> 196; 17205 -> 197; 17206 -> 198;
        17207 -> 199; 17208 -> 200; 17209 -> 201; 17217 -> 202; 17218 -> 203; 17219 -> 204;
        17220 -> 205; 17221 -> 206; 17222 -> 207; 17249 -> 202; 17250 -> 203; 17251 -> 204;
        17252 -> 205; 17253 -> 206; 17254 -> 207; 17456 -> 208; 17457 -> 209; 17458 -> 210;
        17459 -> 211; 17460 -> 212; 17461 -> 213; 17462 -> 214; 17463 -> 215; 17464 -> 216;
        17465 -> 217; 17473 -> 218; 17474 -> 219; 17475 -> 220; 17476 -> 221; 17477 -> 222;
        17478 -> 223; 17505 -> 218; 17506 -> 219; 17507 -> 220; 17508 -> 221; 17509 -> 222;
        17510 -> 223; 17712 -> 224; 17713 -> 225; 17714 -> 226; 17715 -> 227; 17716 -> 228;
        17717 -> 229; 17718 -> 230; 17719 -> 231; 17720 -> 232; 17721 -> 233; 17729 -> 234;
        17730 -> 235; 17731 -> 236; 17732 -> 237; 17733 -> 238; 17734 -> 239; 17761 -> 234;
        17762 -> 235; 17763 -> 236; 17764 -> 237; 17765 -> 238; 17766 -> 239; 17968 -> 240;
        17969 -> 241; 17970 -> 242; 17971 -> 243; 17972 -> 244; 17973 -> 245; 17974 -> 246;
        17975 -> 247; 17976 -> 248; 17977 -> 249; 17985 -> 250; 17986 -> 251; 17987 -> 252;
        17988 -> 253; 17989 -> 254; 17990 -> 255; 18017 -> 250; 18018 -> 251; 18019 -> 252;
        18020 -> 253; 18021 -> 254; 18022 -> 255; 24880 -> 160; 24881 -> 161; 24882 -> 162;
        24883 -> 163; 24884 -> 164; 24885 -> 165; 24886 -> 166; 24887 -> 167; 24888 -> 168;
        24889 -> 169; 24897 -> 170; 24898 -> 171; 24899 -> 172; 24900 -> 173; 24901 -> 174;
        24902 -> 175; 24929 -> 170; 24930 -> 171; 24931 -> 172; 24932 -> 173; 24933 -> 174;
        24934 -> 175; 25136 -> 176; 25137 -> 177; 25138 -> 178; 25139 -> 179; 25140 -> 180;
        25141 -> 181; 25142 -> 182; 25143 -> 183; 25144 -> 184; 25145 -> 185; 25153 -> 186;
        25154 -> 187; 25155 -> 188; 25156 -> 189; 25157 -> 190; 25158 -> 191; 25185 -> 186;
        25186 -> 187; 25187 -> 188; 25188 -> 189; 25189 -> 190; 25190 -> 191; 25392 -> 192;
        25393 -> 193; 25394 -> 194; 25395 -> 195; 25396 -> 196; 25397 -> 197; 25398 -> 198;
        25399 -> 199; 25400 -> 200; 25401 -> 201; 25409 -> 202; 25410 -> 203; 25411 -> 204;
        25412 -> 205; 25413 -> 206; 25414 -> 207; 25441 -> 202; 25442 -> 203; 25443 -> 204;
        25444 -> 205; 25445 -> 206; 25446 -> 207; 25648 -> 208; 25649 -> 209; 25650 -> 210;
        25651 -> 211; 25652 -> 212; 25653 -> 213; 25654 -> 214; 25655 -> 215; 25656 -> 216;
        25657 -> 217; 25665 -> 218; 25666 -> 219; 25667 -> 220; 25668 -> 221; 25669 -> 222;
        25670 -> 223; 25697 -> 218; 25698 -> 219; 25699 -> 220; 25700 -> 221; 25701 -> 222;
        25702 -> 223; 25904 -> 224; 25905 -> 225; 25906 -> 226; 25907 -> 227; 25908 -> 228;
        25909 -> 229; 25910 -> 230; 25911 -> 231; 25912 -> 232; 25913 -> 233; 25921 -> 234;
        25922 -> 235; 25923 -> 236; 25924 -> 237; 25925 -> 238; 25926 -> 239; 25953 -> 234;
        25954 -> 235; 25955 -> 236; 25956 -> 237; 25957 -> 238; 25958 -> 239; 26160 -> 240;
        26161 -> 241; 26162 -> 242; 26163 -> 243; 26164 -> 244; 26165 -> 245; 26166 -> 246;
        26167 -> 247; 26168 -> 248; 26169 -> 249; 26177 -> 250; 26178 -> 251; 26179 -> 252;
        26180 -> 253; 26181 -> 254; 26182 -> 255; 26209 -> 250; 26210 -> 251; 26211 -> 252;
        26212 -> 253; 26213 -> 254; 26214 -> 255;
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

number_exp_sign(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_exp_sign(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, Len) ->
    throw_error(Input, Skip + Len).

number_exp_sign(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Prefix, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Input, Skip, Stack, StringDecode, Prefix, Len + 1);
number_exp_sign(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, _Prefix, Len) ->
    throw_error(Input, Skip + Len).

number_frac(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number_frac_cont(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_frac(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, Len) ->
    throw_error(Input, Skip + Len).

number_frac_cont(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when Byte >= $0 andalso Byte =< $9 ->
    number_frac_cont(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_frac_cont(<<E/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when E =:= $e orelse E =:= $E ->
    number_exp(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_frac_cont(<<Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len) ->
    Token = binary_part(Input, Skip, Len),
    Float = try_parse_float(Token, Token, Skip),
    continue(Rest, Input, Skip + Len, Stack, StringDecode, Float).

number_minus(<<48/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode) ->
    number_zero(Rest, Input, Skip, Stack, StringDecode, 2);
number_minus(<<Byte/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode)
when Byte >= $0 andalso Byte =< $9 ->
    number(Rest, Input, Skip, Stack, StringDecode, 2);
number_minus(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode) ->
    throw_error(Input, Skip + 1).

number_zero(<<46/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len) ->
    number_frac(Rest, Input, Skip, Stack, StringDecode, Len + 1);
number_zero(<<E/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len)
when E =:= $e orelse E =:= $E ->
    number_exp_copy(Rest, Input, Skip + Len + 1, Stack, StringDecode, <<"0">>);
number_zero(<<Rest/bitstring>>, Input, Skip, Stack, StringDecode, Len) ->
    continue(Rest, Input, Skip + Len, Stack, StringDecode, 0).

object(Rest, Input, Skip, Stack, StringDecode) ->
    key(Rest, Input, Skip, [[] | Stack], StringDecode).

object(Data, Input, Skip, Stack, StringDecode, Value) ->
    case Data of
        <<9/integer,Rest/bitstring>> ->
            object(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<10/integer,Rest/bitstring>> ->
            object(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<13/integer,Rest/bitstring>> ->
            object(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<32/integer,Rest/bitstring>> ->
            object(Rest, Input, Skip + 1, Stack, StringDecode, Value);
        <<44/integer,Rest/bitstring>> ->
            Skip2 = Skip + 1,
            [Key, Acc | Stack2] = Stack,
            Acc2 = [{Key, Value} | Acc],
            key(Rest, Input, Skip2, [Acc2 | Stack2], StringDecode);
        <<125/integer,Rest/bitstring>> ->
            Skip2 = Skip + 1,
            [Key, Acc2 | Stack2] = Stack,
            Final = [{Key, Value} | Acc2],
            continue(Rest, Input, Skip2, Stack2, StringDecode, maps:from_list(Final));
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip);
        <<_/bitstring>> ->
            empty_error(Input, Skip)
    end.

decode(Data, Options) when is_binary(Data) andalso is_map(Options) ->
    StringDecode = string_decode_function(Options),
    try
        {ok, value(Data, Data, 0, [?terminate], StringDecode)}
    catch
        throw:{position, Position}:_ ->
            case Position == byte_size(Data) of
                true ->
                    {error, unexpected_end_of_input};
                false ->
                    Byte = binary:at(Data, Position),
                    _hex@1 = integer_to_binary(Byte, 16),
                    {error, {unexpected_byte, <<"0x"/utf8,_hex@1/binary>>, Position}}
            end;
        throw:{token, Token, Position}:_ ->
            {error, {unexpected_sequence, Token, Position}}
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
        <<X/integer,_Rest/bitstring>> when X < 32 ->
            throw_error(Input, Skip);
        <<X/integer,Rest/bitstring>> when X < 34 ->
            string(Rest, Input, Skip, Stack, StringDecode, Acc, Len + 1);
        <<34/integer,Rest/bitstring>> ->
            Last = binary_part(Input, Skip, Len),
            String = iolist_to_binary([Acc | Last]),
            continue(Rest, Input, Skip + Len + 1, Stack, StringDecode, String);
        <<X/integer,Rest/bitstring>> when X < 92 ->
            string(Rest, Input, Skip, Stack, StringDecode, Acc, Len + 1);
        <<92/integer,Rest/bitstring>> ->
            Part = binary_part(Input, Skip, Len),
            escape(Rest, Input, Skip + Len, Stack, StringDecode, [Acc | Part]);
        <<X/integer,Rest/bitstring>> when X < 128 ->
            string(Rest, Input, Skip, Stack, StringDecode, Acc, Len + 1);
        <<Char/utf8,Rest/bitstring>> when Char =< 2047 ->
            string(Rest, Input, Skip, Stack, StringDecode, Acc, Len + 2);
        <<Char/utf8,Rest/bitstring>> when Char =< 65535 ->
            string(Rest, Input, Skip, Stack, StringDecode, Acc, Len + 3);
        <<_Char/utf8,Rest/bitstring>> ->
            string(Rest, Input, Skip, Stack, StringDecode, Acc, Len + 4);
        <<_/integer,_/bitstring>> ->
            throw_error(Input, Skip);
        <<_/bitstring>> ->
            empty_error(Input, Skip + Len)
    end.

string_decode_function(Options) ->
    case maps:get(strings, Options, reference) of
        reference -> fun(X) -> X end;
        copy -> fun binary:copy/1
    end.

terminate(<<32/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Value) ->
    terminate(Rest, Input, Skip + 1, Stack, StringDecode, Value);
terminate(<<13/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Value) ->
    terminate(Rest, Input, Skip + 1, Stack, StringDecode, Value);
terminate(<<10/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Value) ->
    terminate(Rest, Input, Skip + 1, Stack, StringDecode, Value);
terminate(<<9/integer,Rest/bitstring>>, Input, Skip, Stack, StringDecode, Value) ->
    terminate(Rest, Input, Skip + 1, Stack, StringDecode, Value);
terminate(<<>>, _Input, _Skip, _Stack, _StringDecode, Value) ->
    Value;
terminate(<<_Rest/bitstring>>, Input, Skip, _Stack, _StringDecode, _Value) ->
    throw_error(Input, Skip).

throw_error(_Input, Skip) ->
    throw({position, Skip}).

throw_error(<<_Rest/bitstring>>, _Input, Skip, _Stack, _StringDecode) ->
    throw({position, Skip - 1}).

token_error(Token, Position) ->
    throw({token, Token, Position}).

token_error(Token, Position, Len) ->
    throw({token, binary_part(Token, Position, Len), Position}).

try_parse_float(_string@1, Token, Skip) ->
    try
        binary_to_float(_string@1)
    catch
        error:badarg:_ ->
            token_error(Token, Skip)
    end.

value(Data, Input, Skip, Stack, StringDecode) ->
    case Data of
        <<X/integer,Rest/bitstring>> when X =:= 9 orelse X =:= 10 ->
            value(Rest, Input, Skip + 1, Stack, StringDecode);
        <<13/integer,Rest/bitstring>> ->
            value(Rest, Input, Skip + 1, Stack, StringDecode);
        <<32/integer,Rest/bitstring>> ->
            value(Rest, Input, Skip + 1, Stack, StringDecode);
        <<34/integer,Rest/bitstring>> ->
            string(Rest, Input, Skip + 1, Stack, StringDecode, 0);
        <<45/integer,Rest/bitstring>> ->
            number_minus(Rest, Input, Skip, Stack, StringDecode);
        <<48/integer,Rest/bitstring>> ->
            number_zero(Rest, Input, Skip, Stack, StringDecode, 1);
        <<X/integer,Rest/bitstring>> when X >= 49 andalso X =< 57 ->
            number(Rest, Input, Skip, Stack, StringDecode, 1);
        <<91/integer,Rest/bitstring>> ->
            array(Rest, Input, Skip + 1, Stack, StringDecode);
        <<93/integer,Rest/bitstring>> ->
            empty_array(Rest, Input, Skip + 1, Stack, StringDecode);
        <<102/integer,Rest/bitstring>> ->
            case Rest of
                <<"alse",Rest1/bitstring>> ->
                    continue(Rest1, Input, Skip + 5, Stack, StringDecode, false);
                <<_/bitstring>> ->
                    throw_error(Input, Skip)
            end;
        <<110/integer,Rest/bitstring>> ->
            case Rest of
                <<"ull",Rest1/bitstring>> ->
                    continue(Rest1, Input, Skip + 4, Stack, StringDecode, null);
                <<_/bitstring>> ->
                    throw_error(Input, Skip)
            end;
        <<116/integer,Rest/bitstring>> ->
            case Rest of
                <<"rue",Rest1/bitstring>> ->
                    continue(Rest1, Input, Skip + 4, Stack, StringDecode, true);
                <<_/bitstring>> ->
                    throw_error(Input, Skip)
            end;
        <<123/integer,Rest/bitstring>> ->
            object(Rest, Input, Skip + 1, Stack, StringDecode);
        <<_/integer,Rest/bitstring>> ->
            throw_error(Rest, Input, Skip + 1, Stack, StringDecode);
        <<_/bitstring>> ->
            throw_error(Input, Skip)
    end.

