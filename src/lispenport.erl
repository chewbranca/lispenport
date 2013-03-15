-module(lispenport).


-export([start/0, stop/0]).


-export([
    eval/1,
    repl/0
]).


start() ->
    application:start(sasl),
    application:start(?MODULE).


stop() ->
    ok.


eval(Sexp) ->
    {ok, Resp, _Env} = gen_server:call(lispenport_runtime, {eval, Sexp}),
    Resp.


repl() ->
    Sexp = io:get_line("lispenport> "),
    if (Sexp == "quit\n") -> io:format("bye~n");
        true ->
            Resp = eval(Sexp),
            io:format("~p~n", [Resp]),
            repl()
    end.

