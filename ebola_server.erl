-module(ebola_server).
-export([start/5, loop/2, print_all_patients/1, run/0]).

% Creates a list of Patient PIDs and spawns the server loop.
% Takes in number of patients, a list of names and a list of their current health status
% Coordinates is a tuple of {X, Y}.
start(NumPatients, Names, Health, Coordinates, {DiseaseName, Tick_time, Strength}) ->
	Patients = make_patients(NumPatients, Names, Health, Coordinates),
	Disease = spawn(disease, start, [ [DiseaseName, Patients, {Tick_time, Strength}] ]),
	Server = spawn(ebola_server, loop, [Patients, Disease]),
	send_server_to_patients(Patients, Server).

% Creates a list of tuple of {PIDs, Coordinate} of the patients.
make_patients(0, _, _, _) -> [];
make_patients(NumPatients, [Name | NTail], [Health | HTail], [Coord | CTail]) -> 
		% Note: Patient himself doesn't need to know his location. Server deals with that.
		[ {spawn(patient, start, [{Name, Health}]), Coord} | make_patients(NumPatients - 1, NTail, HTail, CTail)].

is_neighbor({X, Y}, {X2, Y2}) -> (abs(X2 - X) =< 1) and (abs(Y2 - Y) =< 1).	

% Server loop.
loop(Patients, Disease) ->
	 %print_all_patients(Patients),
	 %timer:apply_after(5000, ebola_server, loop, [Patients]).

	receive
		{state_change, Name, Health} -> print_patient_state(Name, Health); % Produce new Patients list with changed state.
		{spread, PID, Health} 		 -> find_coord(Patients, PID, Health, Patients, Disease) % Need to find neighbors here.
		% true 			 	-> print_patient_state("Fuckface", "sick")
	after 0      			-> timeout
	end,

	loop(Patients, Disease).

print_patient_state(Name, Health) ->
	Msg = string:concat(string:concat(Name, " is "), Health),
	io:fwrite(string:concat(Msg, "~n")).

send_server_to_patients([{PID, _} | []], Server) -> PID ! {server, Server};
send_server_to_patients([{PID, _} | Tail], Server) -> PID ! {server, Server}, send_server_to_patients(Tail, Server).

% Send a 'print' message to all the patients.
print_all_patients([A | [] ]) -> A ! print;
print_all_patients([A | B]) -> 
						A ! print, 
						print_all_patients(B).

find_coord( [], _, _, _, _) -> ok;
find_coord( [{PID, Coord} | Tail], PID, Health, Patients, Disease) ->
	spread_to_neighbors(Coord, Health, Patients, Disease);
find_coord( [_Head | Tail], PID, Health, Patients, Disease) ->
	find_coord(Tail, PID, Health, Patients, Disease).

spread_to_neighbors(_, _, [], _) -> ok;
spread_to_neighbors(Coord1, Health, [{PID, Coord2} | Tail], Disease) ->
	case is_neighbor(Coord1, Coord2) of
		true -> infect(PID, Health, Disease);
		_ -> ok
	end,
	spread_to_neighbors(Coord1, Health, Tail, Disease).


infect(PID, Health, Disease) -> 
	Treshold = case Health of 
					clean -> 0;
					dormant -> 0.2;
					sick -> 0.4;
					terminal -> 0.7;
					dead -> 0
				end,
	Rnd = random:uniform(),
	if 
		Rnd < Treshold -> 
			Disease ! {new_infected, PID},
			PID ! sick
	end.	

run() ->
	start(4, 
		["Harry", "FuckFace", "ShitEater", "DumbFuckingFuck"],
		[clean, dormant, clean, clean],
		[{0, 0}, {1, 0}, {0, 1}, {1, 1}],
		{"E-Bola", 5, 0.5}
	).