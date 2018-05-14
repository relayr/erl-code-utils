%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2009-2018
%% @doc Miscellaneous functions for parsing of Erlang code.
%% @end
%%------------------------------------------------------------------------------
-module(code_utils).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
    string_to_exprs/1,
    string_to_term/1,
    term_to_string/1,
    eval_exprs/1,
    eval_string/1,

    transform/2
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

-spec string_to_exprs(String :: nonempty_string()) -> [erl_parse:abstract_expr()].
string_to_exprs(String) ->
    {ok, Scanned, _} = erl_scan:string(String),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    Parsed.

-spec string_to_term(String :: nonempty_string()) -> term().
string_to_term(String) ->
    eval_string(String ++ ".").

-spec term_to_string(Term :: term()) -> nonempty_string().
term_to_string(Term) ->
    lists:flatten(transform(Term, fun code_transform:simple_transform/2)).

-spec eval_exprs(Exprs :: [erl_parse:abstract_expr()]) -> Result :: any().
eval_exprs(Exprs) ->
    {value, Result, _Env} = erl_eval:exprs(Exprs, []),
    Result.

-spec eval_string(String :: string()) -> Result :: any().
eval_string(String) ->
    Exprs = string_to_exprs(String),
    eval_exprs(Exprs).

%%------------------------------------------------------------------------------
%% @spec transform(T, F) -> Text
%% where
%%		T = term()
%%		F = code_transform:transform_function()
%%		Text = list()
%% @doc Traverses Erlang term, executes transformation F and returns list.
%%      Example transform function can be found in code_transform:simple_transform/2.
%% @end
%%------------------------------------------------------------------------------
-spec transform(T :: term(), F :: code_transform:transform_function()) -> list().
transform(T, F) ->
    code_transform:traverse(T, F).

%% =============================================================================
%% Local functions
%% =============================================================================