-module(ebola_server).
-export([start/2, loop/1, print_all_patients/1]).

start(NumPatients, Names) ->
	Patients = make_patients(NumPatients, Names),
	spawn(ebola_server, loop, [Patients]).

make_patients(0, _X) -> [];
make_patients(NumPatients, [A | B]) -> [spawn(patient, start, [A]) | make_patients(NumPatients - 1, B)].

loop(Patients) ->
	 print_all_patients(Patients),
	 timer:apply_after(5000, ebola_server, loop, [Patients]).

print_all_patients([A | [] ]) -> A ! print;
print_all_patients([A | B]) -> 
						A ! print, 
						print_all_patients(B).