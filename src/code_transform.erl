%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2009-2018
%% @doc Functions for transforming of Erlang terms.
%% @end
%%------------------------------------------------------------------------------
-module(code_transform).

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
    traverse/2,

    simple_transform/2
]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type transform_value_type():: atom | number | pid.
-type transform_action()    :: list_start | list_stop | binary_start | binary_stop | tuple_start | tuple_stop |
                               map_start | map_stop | {value, transform_value_type()}.
-type transform_output()    :: {go, integer() | list(), list()} | {stop, list()} | integer() | list().

-type transform_function()  :: fun((Action :: transform_action(), Term :: term()) -> transform_output()).

-export_type([
    transform_function/0
]).

%% =============================================================================
%% Exported functions
%% =============================================================================

%%------------------------------------------------------------------------------
%% @spec traverse(S, F) -> Text
%% where
%%		S = any()
%%		F = Fun
%%		Text = list()
%% @doc Traverses structure, executes transformation F and returns list
%% @end
%%------------------------------------------------------------------------------
-spec traverse(T :: term(), F :: transform_function()) -> list().
traverse(T, F) ->
    lists:flatten(traverse_and_transform(T, F)).

%%------------------------------------------------------------------------------
%% @spec simple_transform(Action, Term) -> Ret
%% where
%%		Action = atom()
%%		Term = any()
%%		Ret = {atom(), any() } | { atom, any(), list() } | list() | integer()
%% @doc Converts Term to action command/output - produces Erlang string representation of term
%% @end
%%------------------------------------------------------------------------------

-spec simple_transform(Action :: transform_action(), Term :: term()) -> transform_output().
simple_transform(list_start,[]) ->
    {stop, "[]"};
simple_transform(list_start,L) ->
    case io_lib:printable_list(L) of
        true -> {stop, [io_lib:format("~p", [L])]};
        false ->{go, $[, L}
    end;
simple_transform(list_stop,_L) ->
    $];
simple_transform(binary_start, <<>>) ->
    {stop, "<<>>"};
simple_transform(binary_start, B) ->
    L = binary_to_list(B),
    case io_lib:printable_list(L) of
        true -> {stop, [io_lib:format("<<~p>>", [L])]};
        false ->{go, "<<", L}
    end;
simple_transform(binary_stop, _B) ->
    ">>";
simple_transform(tuple_start, {}) ->
    {stop, "{}"};
simple_transform(tuple_start, T) ->
    {go, ${, tuple_to_list(T)};
simple_transform(tuple_stop, _T) ->
    $};
simple_transform(map_start, M) when map_size(M) =:= 0 ->
    {stop, "#{}"};
simple_transform(map_start, M) ->
    {go, "#{", maps:to_list(M)};
simple_transform(map_stop, _M) ->
    $};
simple_transform({value,T},V) ->
    simple_value_transform(T,V).

-spec simple_value_transform(Type :: transform_value_type(), Value :: atom() | number() | pid()) -> list().
simple_value_transform(atom, A) ->
    io_lib:write(A);
simple_value_transform(number, N) ->
    io_lib:write(N);
simple_value_transform(pid, P) ->
    {match, [P1, P2, P3]} = re:run(pid_to_list(P), "<([0-9]+)\.([0-9]+)\.([0-9]+)>", [{capture, [1,2,3], list}]),
    io_lib:format("c:pid(~s,~s,~s)", [P1, P2, P3]).

%% =============================================================================
%% Local functions
%% =============================================================================

-spec traverse_and_transform(T :: term(), F :: transform_function()) -> list().
traverse_and_transform(L, F) when is_list(L) ->
    case F(list_start, L) of
        {go, Open, [H|T] } ->
            seq_process(traverse_and_transform(H,F), T, F, Open, F(list_stop, [H|T]));
        {stop, Text} -> Text
    end;
traverse_and_transform(T, F) when is_tuple(T) ->
    case F(tuple_start, T) of
        {go, Open, [H|Tail] } ->
            seq_process(traverse_and_transform(H,F), Tail, F, Open, F(tuple_stop, T));
        {stop, Text} -> Text
    end;
traverse_and_transform(B, F) when is_binary(B) ->
    case F(binary_start, B) of
        {go, Open, [H|T] } ->
            seq_process(traverse_and_transform(H,F), T, F, Open, F(binary_stop, B));
        {stop, Text} -> Text
    end;
traverse_and_transform(M, F) when is_map(M) ->
    case F(map_start, M) of
        {go, Open, [{K,V}|Tail] } ->
            seq_kv_process(
                traverse_and_transform(K,F) ++
                "=>" ++
                traverse_and_transform(V,F),
                Tail, F, Open, F(map_stop, M)
            );
        {stop, Text} -> Text
    end;
traverse_and_transform(A, F) when is_atom(A)->
    F({value, atom}, A);

traverse_and_transform(N, F) when is_number(N)->
    F({value, number}, N);

traverse_and_transform(P, F) when is_pid(P)->
    F({value, pid}, P).

seq_process( H, Tail, F, Open, Close ) ->
    [   Open ,
        [ H  | tail_traverse(Tail,F) ],
        Close
    ].

seq_kv_process( H, Tail, F, Open, Close) ->
    [
        Open,
        [ H  | tail_kv_traverse(Tail,F) ],
        Close
    ].

-spec tail_traverse(L :: list(), F :: transform_function()) -> list().
tail_traverse([], _F) ->
    "";
tail_traverse([H|T], F) ->
    [ $,
        ,
        traverse_and_transform(H,F) | tail_traverse(T, F)
    ].

-spec tail_kv_traverse(L :: list(), F :: transform_function()) -> list().
tail_kv_traverse([], _F) ->
    "";
tail_kv_traverse([{K,V}|T], F) ->
    [ $,
        ,
        traverse_and_transform(K,F),
        "=>",
        traverse_and_transform(V,F) | tail_kv_traverse(T, F)
    ].