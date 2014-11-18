-module(disease).
-export([start/1, loop/1, infect/2]).

% Properties is a tuple of disease properties.
% Patients is list of PIDs of sick patient processes.
% Tick time is how often the disease should trigger
% Strength is between 0 and 1, likelihood of catching disease.
start([Name, Patients, {Tick_time, Strength}]) ->
	{ok, Tref} = timer:apply_interval( (Tick_time * 1000), disease, infect, [Patients, Strength]),
	loop([Name, Patients, Tref, {Tick_time, Strength}]).

% Tref will be used to stop the apply_interval when we need to update the list of
% infected patients!
loop([Name, Patients, Tref, {Tick_time, Strength}]) ->
	receive
		{new_infected, PID} -> 
			% Need to check for duplicates in the list.
			timer:cancel(Tref),
			{ok, New_Tref} = timer:apply_interval(Tick_time, disease, infect, [ [PID | Patients], Strength]),
			loop([Name, [PID | Patients], New_Tref, {Tick_time, Strength}])
	end.

%Attempts to infect a patient.
% BUG: can't reseed...
infect([], _) -> ok;
infect([{PID , _} | Tail], Strength) ->
	Rnd = random:uniform(),
	if 
		Rnd < Strength -> PID ! sick;
		true -> PID ! healthy
	end,
	infect(Tail, Strength).



