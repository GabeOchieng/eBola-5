-module(ebola).
-export([start/0]).

start() ->
    {ok, P} = python:start(),
    python:call(P, ebola, run, []).