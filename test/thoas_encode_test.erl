-module(thoas_encode_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{no_auto_import, [float/1]}]).

-import(thoas_encode, [
    true/0, false/0, null/0, boolean/1, integer/1, float/1, string/1,
    non_recursive_array/1, non_recursive_object/1, encode/2
]).

true_test() ->
    ?assertEqual(true(), <<"true">>).

false_test() ->
    ?assertEqual(false(), <<"false">>).

boolean_true_test() ->
    ?assertEqual(boolean('false'), <<"false">>).

boolean_false_test() ->
    ?assertEqual(boolean('true'), <<"true">>).

null_test() ->
    ?assertEqual(null(), <<"null">>).

integer_test_() ->
    Cases = [
        {0, "0"},
        {1, "1"},
        {-0, "0"},
        {-1, "-1"},
        {-100000, "-100000"},
        {100000, "100000"}
    ],
    [ 
        ?_assertEqual(Expected, integer(Input))
        || {Input, Expected} <- Cases
    ].

float_test_() ->
    Cases = [
        {0.0, "0.0"},
        {1.1, "1.1"},
        {-0.0, "-0.0"},
        {-1.3, "-1.3"},
        {-100000.4234, "-100000.4234"},
        {100000.214, "100000.214"}
    ],
    [ 
        ?_assertEqual(Expected, float(Input))
        || {Input, Expected} <- Cases
    ].

string_test_() ->
    Cases = [
        {<<"Hello, Joe">>, <<"\"Hello, Joe\"">>},
        {<<>>, <<"\"\"">>},
        {<<"\n">>, <<"\"\\n\"">>},
        {<<"🤔">>, <<"\"\\u0014\"">>},
        {<<"Goodbye, Joe">>, <<"\"Goodbye, Joe\"">>}
    ],
    [ 
        ?_assertEqual(Expected, iolist_to_binary(string(Input)))
        || {Input, Expected} <- Cases
    ].


non_recursive_array_test_() ->
    Cases = [
        {[true()], <<"[true]">>},
        {[true(), false(), null()], <<"[true,false,null]">>},
        {[true(), string(<<"\n">>)], <<"[true,\"\\n\"]">>},
        {[], <<"[]">>}
    ],
    [ 
        ?_assertEqual(Expected, iolist_to_binary(non_recursive_array(Input)))
        || {Input, Expected} <- Cases
    ].

non_recursive_object_test_() ->
    Cases = [
        {
            [
                {<<"name">>, string(<<"Gleam">>)},
                {<<"isCool">>, true()}
            ], 
            <<"{\"name\":\"Gleam\",\"isCool\":true}">>
        },
        {
            [
                {<<"\n">>, string(<<"That needed to be escaped">>)}
            ], 
            <<"{\"\\n\":\"That needed to be escaped\"}">>
        },
        {
            [
                {<<"isCool">>, true()}
            ], 
            <<"{\"isCool\":true}">>
        },
        {
            [
            ], 
            <<"{}">>
        }
    ],
    [ 
        ?_assertEqual(Expected, iolist_to_binary(non_recursive_object(Input)))
        || {Input, Expected} <- Cases
    ].

encode_test_() ->
    Cases = [
        {[{<<"foo">>, 1}], <<"{\"foo\":1}">>},
        {#{<<"bar">> => 2}, <<"{\"bar\":2}">>}
    ],
    [
        ?_assertEqual(Expected, iolist_to_binary(encode(Input, #{})))
        || {Input, Expected} <- Cases
    ].
