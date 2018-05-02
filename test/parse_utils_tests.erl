%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2009-2018
%%------------------------------------------------------------------------------
-module(parse_utils_tests).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit tests
%% =============================================================================

string_to_term_test() ->
    ?assertEqual(5, parse_utils:string_to_term("5")),
    ?assertEqual(atom, parse_utils:string_to_term("atom")),
    ?assertEqual({tuple, 3, {elem, 1}}, parse_utils:string_to_term("{tuple, 3, {elem, 1}}")),
    ?assertEqual([1,5,3], parse_utils:string_to_term("[1,5, 3]")),
    ?assertEqual(["string1", null, <<"bin1">>], parse_utils:string_to_term("[\"string1\",null,<<\"bin1\">>]")).

term_to_string_test() ->
    ?assertEqual("7.1", parse_utils:term_to_string(7.1)),
    ?assertEqual("{tuple,undefined,3}", parse_utils:term_to_string({tuple, undefined, 3})),
    ?assertEqual("[\"s1\",null,<<\"b1\">>]", parse_utils:term_to_string(["s1",null,<<"b1">>])).

eval_string_test() ->
    ?assertEqual(5, parse_utils:eval_string("2 + 3.")),
    ?assertEqual(["elem", 2, 7], parse_utils:eval_string("L1 = [\"elem\"], L2 = [2, 7], lists:append(L1, L2).")).

-endif.
