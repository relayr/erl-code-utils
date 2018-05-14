%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2009-2018
%% @doc Functions for traversing of Erlang terms.
%% @end
%%------------------------------------------------------------------------------
-module(code_transform).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
    traverse/1,
    traverse/2
]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

%% =============================================================================
%% Exported functions
%% =============================================================================

traverse(T) ->
    traverse(T, fun simple_transform/2).

%%------------------------------------------------------------------------------
%% @spec traverse(S, F) -> Text
%% where
%%		S = any()
%%		F = Fun
%%		Text = list()
%% @doc Traverses structure, executes F and returns not flattened list
%% @end
%%------------------------------------------------------------------------------
traverse(L, F) when is_list(L) ->
    case F(list_start, L ) of
        {go, Open, [H|T] } ->
            seq_process(traverse(H,F), T, F, Open, F(list_stop, [H|T]));
        {stop,Text} -> Text
    end;
traverse(T, F) when is_tuple(T) ->
    case F(tuple_start, T) of
        {go, Open, [H|Tail] } ->
            seq_process(traverse(H,F), Tail, F, Open, F(tuple_stop, T));
        {stop, Text} -> Text

    end;
traverse(B, F) when is_binary(B) ->
    case F(binary_start, B ) of
        {go, Open, [H|T] } ->
            seq_process(traverse(H,F), T, F, Open, F(binary_stop, B));
        {stop, Text, _ } -> Text
    end;
traverse(A, F) when is_atom(A)->
    F(atom, A);

traverse(N, F) when is_number(N)->
    F(number, N);

traverse(P, F) when is_pid(P)->
    F(pid, P).

%% =============================================================================
%% Local functions
%% =============================================================================

tailtraverse([], _F) ->
    "";
tailtraverse([H|T], F) ->
    [ $,
        ,
        traverse(H,F) | tailtraverse(T, F)
    ].

seq_process( "", Tail, F, _Open, _Close) ->
    traverse(Tail,F);

seq_process( H, Tail, F, Open, Close ) ->
    [   Open ,
        [ H  | tailtraverse(Tail,F) ],
        Close
    ].

%%------------------------------------------------------------------------------
%% @spec simple_transform(Action, Term ) -> Ret
%% where
%%		Action = atom()
%%		Term = any()
%%		Ret = {atom(), any() } | { atom, any(), list() } | list() | integer()
%% @doc Converts Term to action command/output - produces Erlang string representation of term
%% @end
%%------------------------------------------------------------------------------

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
    {stop,"<<>>", []};
simple_transform(binary_start, B) ->
    L = binary_to_list(B),
    case io_lib:printable_list(L) of
        true -> {stop, [io_lib:format("<<~p>>", [L])], L };
        false ->{go, "<<", L }
    end;
simple_transform(binary_stop, _B) ->
    ">>";
simple_transform(tuple_start, {}) ->
    {stop, "{}"};
simple_transform(tuple_start, T) ->
    {go, ${,  tuple_to_list(T)};
simple_transform(tuple_stop, _T) ->
    $};
simple_transform(atom,A) ->
    io_lib:write(A);
simple_transform(number,N) ->
    io_lib:write(N);
simple_transform(pid,P) ->
    {match, [P1, P2, P3]} = re:run(pid_to_list(P), "<([0-9]+)\.([0-9]+)\.([0-9]+)>", [{capture, [1,2,3], list}]),
    io_lib:format("c:pid(~s,~s,~s)", [P1, P2, P3]).