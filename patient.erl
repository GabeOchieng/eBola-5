-module(patient).
-export([start/1, loop/1]).

% Start the loop
start({Name, Health}) -> 
	receive
		{server, Server} -> loop({Name, Health, Server})
	end.

% Continuously loop 
loop({Name, Health, Server}) ->
	receive
		print 		-> io:fwrite(string:concat(Name, ": Sup Bros!~n"))
	after 0      -> timeout
	end,

	NewHealth = sick(),

	Server ! {Name, Health},

	timer:apply_after(5000, patient, loop, [{Name, NewHealth, Server}]).

sick() -> 
	Rnd = random:uniform(),
	if 
		Rnd < 0.5 -> "sick";
		true -> "healthy"
	end.
