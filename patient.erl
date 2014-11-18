-module(patient).
-export([start/4, loop/4, spread_disease/2]).

% Start the loop
start(Name, Health, Tick_time, Disease_Strength) -> 
	receive
		{server, Server} ->
			
			% Set time interval to repeatededly send spread message to server.
			{ok, Tref} = timer:apply_interval((Tick_time * 1000), patient, spread_disease, [Health, Server]), 

			%Set interval to send myself a sick message
			{ok, _Tref2} = timer:apply_interval(8000, patient, send_sick_message, [Disease_Strength]),

			% Go into main loop
			loop(Name, Health, Server, Tref)
	end.

% Continuously loop 
loop(Name, Health, Server, Tref) ->
	receive 

		% Send when a sick neighbor has infected me
		infect -> 
			case Health of

				% Only effects me if I wasn't sick already.
				clean ->

					%Send state change to server and start spreading disease
					New_Health = dormant,
					Server ! {state_change, Name, New_Health}, 

					% Cancel old Tref, generate new Tref with new state.
					timer:cancel(Tref),
					{ok, New_Tref} = timer:apply_interval(5000, patient, spread_disease, [New_Health, Server]),
					
					loop(Name, New_Health, Server, New_Tref);


				_ -> loop(Name, Health, Server, Tref)
			end;

		% Patient sent this message to itself, needs to check if it gets sicker	
		sick -> 

			case Health of 

				% Nothing happens
				clean -> loop(Name, Health, Server, Tref);

				% Potentially change your state, notify server.
				_ -> 
					New_Health = change_state(Health),
					Server ! {state_change, Name, New_Health},
					timer:cancel(Tref),
					{ok, New_Tref} = timer:apply_interval(5000, patient, spread_disease, [New_Health, Server]),
					loop(Name, New_Health, Server, New_Tref)
			end;

		%Shouldn't happen.
		_ -> loop(Name, Health, Server, Tref)
	end.

%Update your sickness
change_state(Health) -> 
	case Health of
		dormant -> sick;
		sick -> terminal;
		terminal -> dead
	end.

% Self explanatory, possibly sends a sick message
send_sick_message(Strength) -> 
	Rnd = random:uniform(),
	if 
		Rnd < Strength -> self() ! sick 
	end.

% Tell the server to infect others if you're sick.
spread_disease(Health, Server) ->
	case Health of
		clean -> ok;
		_ -> Server ! {spread, self(), Health}
	end.
