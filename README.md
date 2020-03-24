# code_utils

![Build Status](https://github.com/relayr/erl-code-utils/workflows/Erlang%20CI/badge.svg) [![Hex.pm](https://img.shields.io/hexpm/v/code_utils.svg?style=flat)](https://hex.pm/packages/code_utils) [![Coverage Status](https://coveralls.io/repos/github/relayr/erl-code-utils/badge.svg?branch=master)](https://coveralls.io/github/relayr/erl-code-utils?branch=master)

Functions useful for evaluating Erlang code and manipulating of [Erlang abstract format](http://erlang.org/doc/apps/erts/absform.html).

## Examples

#### code_utils:eval_string/1
Evaluate input string as Erlang code. String needs to end with `.` character to properly evaluate.

```
1> code_utils:eval_string("
    N = 3,
    L = lists:seq(1,N),
    list_to_tuple([\"str\" | L]).
").
{"str",1,2,3}
```

#### code_utils:term_to_string/1
Convert Erlang term to string representation of this term. Functions and references can't be converted. PIDs are converted to `c:pid/3` functions.

```
2> code_utils:term_to_string({atom,0,"str",[pid(0,37,0)]}).
"{atom,0,\"str\",[c:pid(0,37,0)]}"
```

#### code_utils:string_to_term/1
Convert string containing an Erlang term to actual Erlang term. Functions and references can't be converted. PIDs are converted from `c:pid/3` functions.

```
3> code_utils:string_to_term("{atom,0,\"str\",[c:pid(0,37,0)]}").
{atom,0,"str",[<0.37.0>]}
```

#### code_utils:string_to_exprs/1
Convert string containing some Erlang code to abstract format expressions.
```
4> code_utils:string_to_exprs("
    N = 3,
    L = lists:seq(1,N),
    list_to_tuple([\"str\" | L]).
").

[{match,1,{var,1,'N'},{integer,1,3}},
 {match,2,
        {var,2,'L'},
        {call,2,
              {remote,2,{atom,2,lists},{atom,2,seq}},
              [{integer,2,1},{var,2,'N'}]}},
 {call,3,
       {atom,3,list_to_tuple},
       [{cons,3,{string,3,"str"},{var,3,'L'}}]}]
```

#### code_utils:eval_exprs/1
Evaluate abstract format expressions as code.
```
5> code_utils:eval_exprs([
    {call,1, {remote,1,{atom,1,lists},{atom,1,append}}, [
        {cons,1,{integer,1,2},{nil,1}},
        {cons,1,{integer,1,3},{nil,1}}
    ]}
]).

[2,3]
```
