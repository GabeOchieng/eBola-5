% ebola_server.erl
% eBola
% 
% COMP 50 - Mark Sheldon
% Hyung-Seo Park, Robert Ruenes, and Paul Chang
% 
% This is the backend server process that boots off the python front end
% and also the independent patient processes. 
% Use start/0 in order to kick off the program.

-module(ebola_server).
-export([start/0, start/5, loop/2]).

% Creates a list of Patient PIDs and spawns the server loop.
% Takes in number of patients, a list of names, and their associated health
% and coordinates in a tuple {x,y}. Also takes in the disease parameters
start(PythonInstance, Names, Health, Coordinates, 
      {Tick_time, Disease_Strength}) ->
	Patients = make_patients(Names, Health, Coordinates, 
                             Tick_time, Disease_Strength),
	Server = spawn(ebola_server, loop, [PythonInstance, Patients]),
	send_server_to_patients(Patients, Server).

% Spawns a new process for each patient
% Returns a list of tuple of {PIDs, Coordinate} of the patients.
make_patients([], [], [], _, _) -> [];
make_patients([Name | NTail], [Health | HTail], [Coord | CTail], 
               Tick_time, Disease_Strength) -> 
	[{spawn(patient, start, [Name, Health, Tick_time, Disease_Strength]), Coord}
     | make_patients(NTail, HTail, CTail, Tick_time, Disease_Strength)].

% Returns whether not two coordinates are neighbors or not
is_neighbor({X, Y}, {X2, Y2}) -> (abs(X2 - X) =< 1) and (abs(Y2 - Y) =< 1).	

% Server loop.
loop(PythonInstance, Patients) ->
	receive
        % Send statechange to the front end
		{state_change, Name, Health} -> 
            python:cast(PythonInstance, 
                        {state_change, Name, health_to_int(Health)});
        % Infect neighboring patients
		{spread, PID, Health} 		 -> 
            find_coord(Patients, PID, Health, Patients)
	after 0      			         -> timeout
	end,
	loop(PythonInstance, Patients).

% Translate from the atom to the integer state
health_to_int(Atom) ->
	case Atom of
		clean -> 1;
		dormant -> 2;
		sick -> 3;
		terminal -> 4;
		dead -> 5
	end.

% Send the server to all of the patients.
send_server_to_patients([{PID, _} | []], Server) -> PID ! {server, Server};
send_server_to_patients([{PID, _} | Tail], Server) -> 
    PID ! {server, Server}, 
    send_server_to_patients(Tail, Server).

% Find the coordinates of a given PID and spread to their neighbors
find_coord( [], _, _, _) -> ok;
find_coord( [{PID, Coord} | _Tail], PID, Health, Patients) -> 
    spread_to_neighbors(Coord, Health, Patients);
find_coord( [_Head | Tail], PID, Health, Patients) -> 
    find_coord(Tail, PID, Health, Patients).

% Only spread the infect message to neighbors
spread_to_neighbors(_, _, []) -> ok;
spread_to_neighbors(Coord1, Health, [{PID, Coord2} | Tail]) ->
    % If its a neighbor, then infect
	case is_neighbor(Coord1, Coord2) of
		true -> infect(PID, Health);
		_ -> ok
	end,
	spread_to_neighbors(Coord1, Health, Tail).


% Infect a pid based on the health of the diseased.
infect(PID, Health) -> %PID ! infect. 
    % compute threshold based on health of the diseased
	Threshold = case Health of 
					clean -> 0;
					dormant -> 0.3;
					sick -> 0.5;
					terminal -> 0.7;
					dead -> 0
				end,
	random:seed(erlang:now()),
	Rnd = random:uniform(),
    % If random generated number is lower than threshold send infect
	case Rnd < Threshold of
		true -> PID ! infect;
		false -> false
	end.	

% Main function that boots the program!
start() ->
    % kick off the python process
	{ok, P} = python:start(),
	python:call(P, frontend, main, [self()]),
	wait_for_settings(P).

% Wait for the python process to pass back initial settings
wait_for_settings(PythonInstance) ->
	receive
		{initial_settings, Names, Health, Coordinates, 
        {Tick_time, Disease_Strength}} -> 
            start(PythonInstance, Names, Health, Coordinates, 
                  {Tick_time, Disease_Strength});
		_ -> io:fwrite("Got a message")
	end.