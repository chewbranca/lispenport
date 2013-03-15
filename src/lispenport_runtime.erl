-module(lispenport_runtime).
-behavior(gen_server).


-export([
    start_link/0,

    eval/1
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    env=lispenport_lang:new_env()
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    {ok, #st{}}.


terminate(_Reason, _St) ->
    ok.


handle_call({eval, Sexp}, _From, #st{env=Env}=St) ->
    {ok, Resp, NewEnv} = lispenport_lang:run(Sexp, Env),
    {reply, {ok, Resp, St}, St#st{env=NewEnv}};
handle_call(get_env, _From, #st{env=Env}=St) ->
    {reply, {ok, Env}, St};
handle_call(Msg, _From, St) ->
    {error, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(Msg, St) ->
    {error, {invalid_cast, Msg}, St}.


handle_info(Msg, St) ->
    {error, {invalid_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


eval(sexp) ->
    {Resp, Dict} = lispenport_lang:eval(sexp),
    {Resp, Dict}.
