-module(ebola_server).
-export([start/4, loop/1, print_all_patients/1]).

% Creates a list of Patient PIDs and spawns the server loop.
% Takes in number of patients, a list of names and a list of their current health status
start(NumPatients, Names, Health, Coordinates) ->
	Patients = make_patients(NumPatients, Names, Health, Coordinates),
	Server = spawn(ebola_server, loop, [Patients]),
	send_server_to_patients(Patients, Server).

% Creates a list of tuple of {PIDs, Coordinate} of the patients.
make_patients(0, _X, _Y, _Z) -> [];
make_patients(NumPatients, [A | B], [C | D], [E | F]) -> 
		% Note: Patient himself doesn't need to know his location. Server deals with that.
		[ {spawn(patient, start, [{A, C}]), E} | make_patients(NumPatients - 1, B, D, F)].

% Server loop.
loop(Patients) ->
	 %print_all_patients(Patients),
	 %timer:apply_after(5000, ebola_server, loop, [Patients]).

	receive
		{Name, Health} 		-> print_patient_state(Name, Health)
		% true 			 	-> print_patient_state("Fuckface", "sick")
	after 0      			-> timeout
	end,

	loop(Patients).

print_patient_state(Name, Health) ->
	Msg = string:concat(string:concat(Name, " is "), Health),
	io:fwrite(string:concat(Msg, "~n")).

send_server_to_patients([{PID, _} | []], Server) -> PID ! {server, Server};
send_server_to_patients([{PID, _} | B], Server) -> PID ! {server, Server}, send_server_to_patients(B, Server).

% Send a 'print' message to all the patients.
print_all_patients([A | [] ]) -> A ! print;
print_all_patients([A | B]) -> 
						A ! print, 
						print_all_patients(B).