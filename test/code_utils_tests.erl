%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2009-2018
%%------------------------------------------------------------------------------
-module(code_utils_tests).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit tests
%% =============================================================================

string_to_term_test() ->
    ?assertEqual(5, code_utils:string_to_term("5")),
    ?assertEqual(atom, code_utils:string_to_term("atom")),
    ?assertEqual('', code_utils:string_to_term("''")),
    ?assertEqual("string", code_utils:string_to_term("\"string\"")),
    ?assertEqual({tuple, 3, {'$elem', 1}}, code_utils:string_to_term("{tuple, 3, {'$elem', 1}}")),
    ?assertEqual([1,5,3], code_utils:string_to_term("[1,5, 3]")),
    ?assertEqual(["string1", null, <<"bin1">>], code_utils:string_to_term("[\"string1\",null,<<\"bin1\">>]")),
    ?assertEqual({pids, [undefined, c:pid(0,285,3)]}, code_utils:string_to_term("{pids, [undefined, c:pid(0,285,3)]}")),
    ?assertEqual(#{}, code_utils:string_to_term("#{}")),
    ?assertEqual(#{id => 3, name => "test", state => nil}, code_utils:string_to_term("#{id => 3, name => \"test\", state => nil}")),
    ?assertEqual(#{"name" => <<"john">>, "address" => #{"city" => "chicago"}},
        code_utils:string_to_term("#{\"name\" => <<\"john\">>,\"address\" => #{\"city\" => \"chicago\"}}")),

    ?assertException(error, {unbound_var, 'Var'}, code_utils:string_to_term("{tuple, Var}")).

term_to_string_test() ->
    ?assertEqual("7.1", code_utils:term_to_string(7.1)),
    ?assertEqual("atom", code_utils:term_to_string(atom)),
    ?assertEqual("''", code_utils:term_to_string('')),
    ?assertEqual("\"string\"", code_utils:term_to_string("string")),
    ?assertEqual("{tuple,undefined,3}", code_utils:term_to_string({tuple, undefined, 3})),
    ?assertEqual("[\"s1\",null,<<\"b1\">>]", code_utils:term_to_string(["s1",null,<<"b1">>])),
    ?assertEqual("{pids,['my-test-srv',c:pid(0,285,3)]}", code_utils:term_to_string({pids, ['my-test-srv', c:pid(0,285,3)]})),
    ?assertEqual("#{}", code_utils:term_to_string(#{})),
    ?assertEqual("#{id=>3,name=>\"test\",state=>nil}", code_utils:term_to_string(#{id => 3, name => "test", state => nil})),
    ?assertEqual("#{\"address\"=>#{\"city\"=>\"chicago\"},\"name\"=><<\"john\">>}",
        code_utils:term_to_string(#{"name" => <<"john">>, "address" => #{"city" => "chicago"}})),

    ?assertException(error, function_clause, code_utils:term_to_string(fun() -> not_allowed end)).

eval_string_test() ->
    ?assertEqual(5, code_utils:eval_string("2 + 3.")),
    ?assertEqual(["elem", 2, 7], code_utils:eval_string("L1 = [\"elem\"], L2 = [2, 7], lists:append(L1, L2).")).

-endif.
