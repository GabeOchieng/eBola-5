-module(patient).
-export([start/1, loop/1, spread_disease/2]).

% Start the loop
start({Name, Health}) -> 
	receive
		{server, Server} ->
			{ok, Tref} = timer:apply_interval( (5 * 1000), patient, spread_disease, [ Health, Server ]), 
			loop({Name, Health, Server, Tref})
	end.

% Continuously loop 
loop({Name, Health, Server, Tref}) ->
	%receive
	%	print 		-> io:fwrite(string:concat(Name, ": Sup Bros!~n"))
	%after 0      -> timeout
	%end,

	%NewHealth = sick(),

	%Server ! {Name, Health},

	%timer:apply_after(5000, patient, loop, [{Name, NewHealth, Server}]).

	receive 
		infect   -> 
			case Health of
				clean ->
					Server ! {state_change, Name, dormant}, 
					% Cancel old Tref, generate new Tref with new state.
					loop({Name, dormant, Server, Tref});
				_     -> 
					loop({Name, Health, Server, Tref})
			end;
		sick 	 -> 
			New_Health = change_state(Health),
			Server ! {state_change, Name, New_Health},
			% Must cancel old Tref and start new Tref.
			loop({Name, New_Health, Server, Tref});
		healthy  -> 
			%Server ! {Name, "healthy"},
			loop({Name, Health, Server, Tref})
	end.

change_state(Health) -> 
	case Health of
		clean -> dormant;
		dormant -> sick;
		sick -> terminal;
		terminal -> dead
	end.

spread_disease(Health, Server) ->
	Server ! {spread, self(), Health}.
