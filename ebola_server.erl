-module(ebola_server).
-export([s/0, start/5, loop/2]).

% Creates a list of Patient PIDs and spawns the server loop.
% Takes in number of patients, a list of names and a list of their current health status
% Coordinates is a tuple of {X, Y}.
start(PythonInstance, Names, Health, Coordinates, {Tick_time, Disease_Strength}) ->
	Patients = make_patients(Names, Health, Coordinates, Tick_time, Disease_Strength),
	Server = spawn(ebola_server, loop, [PythonInstance, Patients]),
	send_server_to_patients(Patients, Server).
	%test_infect_patients(Patients).

% Creates a list of tuple of {PIDs, Coordinate} of the patients.
make_patients([], [], [], _, _) -> [];
make_patients([Name | NTail], [Health | HTail], [Coord | CTail], Tick_time, Disease_Strength) -> 
		% Note: Patient himself doesn't need to know his location. Server deals with that.
		[ {spawn(patient, start, [Name, Health, Tick_time, Disease_Strength]), Coord} | make_patients(NTail, HTail, CTail, Tick_time, Disease_Strength)].

is_neighbor({X, Y}, {X2, Y2}) -> (abs(X2 - X) =< 1) and (abs(Y2 - Y) =< 1).	

test_infect_patients([{PID, _} | []]) -> PID ! infect;
test_infect_patients([{PID, _} | Tail]) -> PID ! infect, test_infect_patients(Tail).

% Server loop.
loop(PythonInstance, Patients) ->
	 %print_all_patients(Patients),
	 %timer:apply_after(5000, ebola_server, loop, [Patients]).

	receive
		{state_change, Name, Health} -> python:cast(PythonInstance, {state_change, Name, health_to_int(Health)}); % Produce new Patients list with changed state.
		{spread, PID, Health} 		 -> find_coord(Patients, PID, Health, Patients) % Need to find neighbors here.
		% true 			 	-> print_patient_state("Fuckface", "sick")
	after 0      			-> timeout
	end,

	loop(PythonInstance, Patients).

health_to_int(Atom) ->
	case Atom of
		clean -> 1;
		dormant -> 2;
		sick -> 3;
		terminal -> 4;
		dead -> 5
	end.

send_server_to_patients([{PID, _} | []], Server) -> PID ! {server, Server};
send_server_to_patients([{PID, _} | Tail], Server) -> PID ! {server, Server}, send_server_to_patients(Tail, Server).

find_coord( [], _, _, _) -> ok;

find_coord( [{PID, Coord} | _Tail], PID, Health, Patients) -> spread_to_neighbors(Coord, Health, Patients);

find_coord( [_Head | Tail], PID, Health, Patients) -> find_coord(Tail, PID, Health, Patients).

spread_to_neighbors(_, _, []) -> ok;
spread_to_neighbors(Coord1, Health, [{PID, Coord2} | Tail]) ->
	case is_neighbor(Coord1, Coord2) of
		true -> infect(PID, Health);
		_ -> ok
	end,
	spread_to_neighbors(Coord1, Health, Tail).


%%NEEDS RANDOMNESS
infect(PID, Health) -> PID ! infect. 
	%Threshold = case Health of 
	%				clean -> 0;
	%				dormant -> 0.2;
	%				sick -> 0.4;
	%				terminal -> 0.7;
	%				dead -> 0
	%			end,
	%Rnd = random:uniform(),
	%if 
	%	Rnd < Threshold -> 
	%		PID ! infect
	%end.	

s() ->
	{ok, P} = python:start(),
	python:call(P, frontend, main, [self()]),
	wait_for_settings(P).

wait_for_settings(PythonInstance) ->
	receive
		{initial_settings, Names, Health, Coordinates, {Tick_time, Disease_Strength}} -> io:fwrite("Got message"), start(PythonInstance, Names, Health, Coordinates, {Tick_time, Disease_Strength});
		_ -> io:fwrite("Got a message")

	end.