%%%-------------------------------------------------------------------
%%% @author robertruenes
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2014 3:10 PM
%%%-------------------------------------------------------------------
-module(chat_server).
-author("robertruenes").
%% API
-export([start/1]).

start(ChatName) -> spawn(fun() -> register(ChatName, self()), loop([], ChatName) end), true.

loop(State, ChatName) ->
  receive
    {subscribe, Pid, Node, UserName} -> loop([{Pid, Node, UserName} | State], ChatName);
    {send, Message} -> sendMessage(Message, State), loop(State, ChatName);
    {subscribers, Pid, Node} -> {Pid, Node} ! State, loop(State, ChatName);
    {stop} -> ok
  end.




sendMessage(Message, [{Pid, Node, _UserName} | Tail]) -> {Pid, Node} ! {message, Message}, sendMessage(Message, Tail);
sendMessage(_Message, _) -> ok.
