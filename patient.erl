-module(patient).
-export([start/4, loop/3, spread_disease/3, send_sick_message/2, send_spread_message/1]).

% Start the loop
start(Name, Health, Tick_time, Disease_Strength) -> 
	receive
		{server, Server} ->
			Pid = self(),
			% Set time interval to repeatededly send spread message to server.
			{ok, _Tref} = timer:apply_interval((Tick_time * 1000), patient, send_spread_message, [Pid]), 

			%Set interval to send myself a sick message
			{ok, _Tref2} = timer:apply_interval((Tick_time * 2000), patient, send_sick_message, [Pid, Disease_Strength]),

			% Go into main loop
			loop(Name, Health, Server)
	end.

% Continuously loop 
loop(Name, Health, Server) ->
	receive 

		spread -> spread_disease(self(), Health, Server), loop(Name, Health, Server);
		% Send when a sick neighbor has infected me
		infect -> 
			case Health of

				% Only effects me if I wasn't sick already.
				clean ->

					%Send state change to server and start spreading disease
					New_Health = dormant,
					Server ! {state_change, Name, New_Health}, 

					loop(Name, New_Health, Server);


				_ -> loop(Name, Health, Server)
			end;

		% Patient sent this message to itself, needs to check if it gets sicker	
		sick -> 

			case Health of 

				% Nothing happens
				clean -> loop(Name, Health, Server);
				dead -> loop(Name, Health, Server);

				% Potentially change your state, notify server.
				_ -> 
					New_Health = change_state(Health),
					Server ! {state_change, Name, New_Health},
					loop(Name, New_Health, Server)
			end;

		%Shouldn't happen.
		_ -> loop(Name, Health, Server)
	end.

% Self explanatory, possibly sends a sick message
send_sick_message(MyPid, Strength) -> 
	random:seed(erlang:now()),
	Rnd = random:uniform(),
	case Rnd < Strength of
		true -> MyPid ! sick;
		false -> false
	end.

send_spread_message(MyPid) -> MyPid ! spread.

%Update your sickness
change_state(Health) -> 
	random:seed(erlang:now()),
	Rnd = random:uniform(),
	case Health of
		dormant ->
			case Rnd < 0.3 of
				true -> sick;
				false -> dormant
			end;
		sick -> 
			case Rnd < 0.5 of
				true -> terminal;
				false -> sick
			end;
		terminal -> 
			case Rnd < 0.7 of
				true -> dead;
				false -> terminal
			end;
		dead -> dead
	end.

% Tell the server to infect others if you're sick.
spread_disease(MyPid, Health, Server) ->
	case Health of
		clean -> ok;
		dead -> ok;
		_ -> Server ! {spread, MyPid, Health}
	end.
