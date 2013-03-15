% Copyright (c) 2013 Russell Branca <@chewbranca>
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
%
% Experimentations with a CouchDB Lisp for views
% This version is a fork of:
%   http://norvig.com/lispy.html
%


-module(lispenport_lang).
-include_lib("eunit/include/eunit.hrl").


-compile([export_all]).

%% TODO {{{
%% [x] fix begin
%% [x] switch eval return values from Val() to {State(), Val()}
%%    - every exp should return the updated process state and return value
%% [ ] switch this over to a gen_server
%%    - Logical way to handle state and func returns
%% [x] figure out how to get internal funcs to return the environment, ie ["*", 5, 7]
%% [x] fix lambda [Args] hack
%% [ ] fix length test
%% [ ] add if statements
%% [ ] finish rest of procs
%% [ ] do something with set!
%% [x] fix nested list read_from issue
%% }}}


-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).


wrap_fun(Fn) ->
    fun(Args, Env) ->
        {apply(Fn, Args), Env}
    end.


new_env() ->
    Env = dict:new(),
    Funs = [
        {"*", fun erlang:'*'/2},
        {"+", fun erlang:'+'/2},
        {"-", fun erlang:'-'/2},
        {"/", fun erlang:'/'/2},
        {">", fun erlang:'>'/2},
        {"<", fun erlang:'<'/2},
        {">=", fun erlang:'>='/2},
        {"<=", fun erlang:'=<'/2},
        {"=<", fun erlang:'=<'/2},
        {"=", fun erlang:'=='/2},
        {"eq?", fun erlang:'=:='/2},
        {"neq?", fun erlang:'=/='/2},
        {"length", fun erlang:length/1},
        %{"not", fun erlang:not/1},
        {"cons", fun(X,Y) -> [X | Y] end},
        {"car", fun erlang:hd/1},
        {"cdr", fun erlang:tl/1},
        {"append", fun erlang:'++'/2}
        %{"list?", fun erlang:'+'/2},
        %{"null?", fun erlang:'+'/2},
        %{"symbol?", fun erlang:'+'/2},
    ],
    lists:foldl(fun({Op, Fn}, EnvX) ->
            dict:store(Op, wrap_fun(Fn), EnvX)
        end, Env, Funs).


eval(X) ->
    eval(X, new_env()).


% constant literal
eval(X, Env) when not(is_list(X)) ->
    {X, Env};
% (quote exp)
eval(["quote", Rest], Env) ->
    {Rest, Env};
eval(["list" | Rest], Env) ->
    {Rest, Env};
% (if test conseq alt)
eval(["if", Predicate, Consequent, Alternative], Env) ->
    % Better way to do the fallthrough for false/undefined?
    % Also, these should be rethought, because, nil
    case eval(Predicate, Env) of
        {false, _EnvX} -> eval(Alternative, Env);
        {undefined, _EnvX} -> eval(Alternative, Env);
        {nil, _EnvX} -> eval(Alternative, Env);
        {_, _EnvX} -> eval(Consequent, Env)
    end;
% (set! var exp)
eval(["set!", Var, Exp], Env) ->
    dict:store(Var, eval(Exp, Env), Env);
% (define var exp)
eval(["define", Var, Exp], Env) ->
    {Val, Env2} = eval(Exp, Env),
    Env3 = dict:store(Var, Val, Env2),
    {Val, Env3};
% (lambda (vars*) exp)
eval(["lambda", Vars, Exp], Env) ->
    {fun(Args, _Env) ->
            EnvY = lists:foldl(fun({K,V}, EnvX) ->
                            dict:store(K, V, EnvX)
                    end, Env, lists:zip(Vars, Args)),
            eval(Exp, EnvY)
    end, Env};
% (begin exp*)
eval(["begin"|Exps], Env) ->
    lists:foldl(fun(Exp, {_Val, EnvX}) -> eval(Exp, EnvX) end, {nil, Env}, Exps);
eval(X, Env) ->
    case io_lib:printable_list(X) of
        % variable reference
        true ->
            case dict:find(X, Env) of
                {ok, Value} -> {Value, Env};
                error       -> {nil, Env}
            end;
        % (proc exp*)
        _ ->
            {Exps, _EnvZ} = lists:foldl(fun(Exp, {Vals, EnvX}) ->
                            {Val, EnvY} = eval(Exp, EnvX),
                            {[Val|Vals], EnvY}
                    end, {[], Env}, X),
            [Proc|Args] = lists:reverse(Exps),
            {Ret, _} = apply(Proc, [Args, Env]),
            {Ret, Env}
    end.


% Eval resp helpers
val({Val, _Env}) -> Val.
env({_Val, Env}) -> Env.


% Better way to do this? Compare to:
%def tokenize(s):
%    "Convert a string into a list of tokens."
%    return s.replace('(',' ( ').replace(')',' ) ').split()
tokenize(Sexps) ->
    re:split(
        re:replace(
            string:strip(
                re:replace(
                    re:replace(Sexps, "\\(", " ( ", [global, {return, list}]),
                    "\\)", " ) ", [global, {return, list}])),
            "[ ]+", " ", [global, {return, list}]),
        " ").


symbol(Token) ->
    Token.


atom(Token) ->
    case string:to_float(Token) of
        {F, []} ->
            F;
        {error, no_float} ->
            case string:to_integer(Token) of
                {I, []} -> I;
                _ -> symbol(Token)
            end;
        _ ->
            symbol(Token)
    end.


read(Tokens) ->
    read(Tokens, []).


read([], []) ->
    [];
read([], [Acc]) ->
    Acc;
read([<<"(">>|Tokens], Acc) ->
    case Tokens of
        [] ->
            erlang:error(cl_eof_read_error);
        _ ->
            [[<<")">>|Rest], Acc2] = read(Tokens, []),
            read(Rest, [lists:reverse(Acc2)|Acc])
    end;
read([<<")">>|_]=Tokens, Acc) ->
    [Tokens, Acc];
read([Token|Tokens], Acc) ->
    read(Tokens, [atom(?b2l(Token))|Acc]).


parse(Sexps) ->
    read(tokenize(Sexps)).


run(Sexp, Env) ->
    {Resp, EnvX} = eval(parse(re:replace(Sexp, "\\n", "", [global, {return, list}])), Env),
    {ok, Resp, EnvX}.


repl() ->
    repl(new_env()).


repl(Env) ->
    Sexp = io:get_line("lispenport> "),
    if (Sexp == "quit\n") -> io:format("bye~n");
        true ->
            {Resp, EnvX} = eval(parse(re:replace(Sexp, "\\n", "", [global, {return, list}])), Env),
            io:format("~p~n", [Resp]),
            repl(EnvX)
    end.


%% --------------------------------------------------------------------- 
%%                             EVAL TESTS
%% --------------------------------------------------------------------- 


eval_constant_literal_test() ->
    ?assertEqual(1234, val(eval(1234))).


eval_quote_exp_test() ->
    ?assertEqual(foo, val(eval(["quote", foo]))),
    ?assertEqual([foo, bar], val(eval(["quote", [foo, bar]]))).


eval_if_cond_test() ->
    ?assertEqual(
        "equal",
        val(eval(["if", ["=", 2, 2.0], ["quote", "equal"], ["quote", "notequal"]]))),
    ?assertEqual(
        "notequal",
        val(eval(["if", ["eq?", 2, 2.0], ["quote", "equal"], ["quote", "notequal"]]))).


eval_var_ref_test() ->
    ?assertEqual(nil, val(eval("asdf"))).


eval_define_test() ->
    ?assertEqual(nil, val(eval("foo"))),
    {1234, Env2} = eval(["define", "foo", 1234]),
    ?assertEqual(1234, val(eval("foo", Env2))),
    {"oofbar", Env3} = eval(["define", "bar", ["quote", "oofbar"]], Env2),
    ?assertEqual("oofbar", val(eval("bar", Env3))).


eval_begin_test() ->
    ?assertEqual(1234, val(eval(["begin", 1234]))),
    ?assertEqual(1234, val(eval(["begin", ["define", "fdsa", ["quote", "asdf"]], 1234]))),
    ?assertEqual(1234, val(eval(["begin", ["define", "x", 1234], "x"]))).


eval_proc_exp_test() ->
    ?assertEqual(35, val(eval(["*", 5, 7]))).


eval_lambda_test() ->
    %(define square (lambda (x) (* x x))),
    %F = val(eval(["begin", ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]]])),
    %io:format("RESULT: ~p~n", [F(6)]),
    ?assertEqual(16, val(eval(["begin", ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]], ["square", 4]]))).


% Proc tests
proc_plus_test()      -> ?assertEqual(12    , val(eval(["+"    , 5      , 7]))).
proc_mult_test()      -> ?assertEqual(35    , val(eval(["*"    , 5      , 7]))).
proc_sub_test()       -> ?assertEqual(-2    , val(eval(["-"    , 5      , 7]))).
proc_div_test()       -> ?assertEqual(2.0   , val(eval(["/"    , 14     , 7]))).
proc_gt_test()        -> ?assertEqual(true  , val(eval([">"    , 14     , 7]))).
proc_lt_test()        -> ?assertEqual(false , val(eval(["<"    , 14     , 7]))).
proc_lte_test()       -> ?assertEqual(false , val(eval(["=<"   , 14     , 7]))).
proc_gte_test()       -> ?assertEqual(true  , val(eval([">="   , 14     , 7]))).
proc_equal_test()     -> ?assertEqual(true  , val(eval(["="    , 2      , 2.0]))).
proc_un_equal_test()  -> ?assertEqual(false , val(eval(["="    , 3      , 2.0]))).
proc_eq_test()        -> ?assertEqual(false , val(eval(["eq?"  , 2      , 2.0]))).
proc_neq_test()       -> ?assertEqual(true  , val(eval(["neq?" , 2      , 2.0]))).
%proc_length_test() -> ?assertEqual(4     , val(eval(["length" , ["quote" , 1, 2, 3, 4]]))).
%proc_equal_str_test() -> ?assertEqual(false , val(eval(["="    , "asdf" , "fdsa"]))).


% Tokenize tests
%parse_multi_test() ->
%    ?assertEqual(
%        ["begin", ["define", ["square", "x"], ["*", "x", "x"]], ["square", 4]],
%        parse("(begin (define (square x) (* x x)) (square 4))")).


parse_combo_test() ->
    ?assertEqual(
        ["+", 4, ["*", 2, 2]],
        parse("(+ 4 (* 2 2))")).


parse_lambda_test() ->
    ?assertEqual(
        ["lambda", ["x"], ["*", "x", "x"]],
        parse("(lambda (x) (* x x))")).


parse_define_test() ->
    ?assertEqual(
        ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]],
        parse("(define square (lambda (x) (* x x)))")).


parse_square_test() ->
    ?assertEqual(
        ["begin", ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]], ["square", 4]],
        parse("(begin (define square (lambda (x) (* x x))) (square 4))")).


parse_if_test() ->
    ?assertEqual(
        ["if", ["=", 2, 2.0], ["quote", "equal"], ["quote", "notequal"]],
        parse("(if (= 2 2.0) (quote equal) (quote notequal))")).
