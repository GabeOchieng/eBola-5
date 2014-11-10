%%%-------------------------------------------------------------------
%%% @author robertruenes
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2014 3:10 PM
%%%-------------------------------------------------------------------
-module(chat_client).
-author("robertruenes").

%% API
-export([join/2, join/3]).

join(UserName, NodeName, ChatName) ->
  %%register(UserName, self()),
  spawn(fun() -> {ChatName, NodeName} ! {subscribe, self(), node(), UserName}, messageListener() end),
  %spawn(fun() -> messageSender(UserName, NodeName, ChatName) end).
  messageSender(UserName, NodeName, ChatName).

join(UserName, ChatName) -> join(UserName, node(), ChatName).

messageListener() ->
  receive
    {message, MessageText} -> io:format(MessageText), messageListener()
  end.

messageSender(UserName, NodeName, ChatName) ->
  UserInput = io:get_line("Enter a message: "),
  UserNameAndSpace = string:concat(UserName, ": "),
  Message = string:concat(UserNameAndSpace, UserInput),
  IntermediateMessage = string:concat("\"", Message),
  FinalMessage = string:concat(IntermediateMessage, "\""),
  {ChatName, NodeName} ! {send, FinalMessage},
  messageSender(UserName, NodeName, ChatName).

