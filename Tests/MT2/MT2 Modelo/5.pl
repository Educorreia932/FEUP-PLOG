:-use_module(library(clpfd)).

corta(Pranchas, Prateleiras, PranchasSelecionadas):-
	% Variables
	length(Prateleiras, NPrateleiras),
	length(PranchasSelecionadas, NPrateleiras), 
	
	% Domain
	length(Pranchas, NPranchas),
	domain(PranchasSelecionadas, 1, NPranchas),
	
	% Constraints
	getTasks(Prateleiras, PranchasSelecionadas, Tasks),
	getMachines(Pranchas, Machines, 1),
	cumulatives(Tasks, Machines, [bound(upper)]),
	
	% Labeling
	labeling([], PranchasSelecionadas).
	
getTasks([], [], []).

getTasks([Prateleira|S], [PranchaSelecionada|B], [Task|T]):-
	Task = task(0, Prateleira, Prateleira, Prateleira, PranchaSelecionada),
	getTasks(S, B, T).
	
getMachines([], [], _).

getMachines([Prancha|B], [Machine|M], ID):-
	Machine = machine(ID, Prancha),
	NewID is ID + 1, 
	getMachines(B, M, NewID).

% corta([100,45,70], [12,50,14,8,10,90,24], S).
