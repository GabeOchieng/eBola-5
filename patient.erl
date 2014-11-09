-module(patient).
-export([start/1]).

start(Name) -> loop(Name).

loop(Name) ->
	receive
		print -> io:fwrite(string:concat(Name, ": Sup Bros!~n"))
	end,
	loop(Name).