inverter(Lista, InversoLista) :- 
    reverter(Lista, [], InversoLista). 
    
reverter([H|T], S, Resultado) :- 
    reverter(T, [H|S], Resultado). 
    
reverter([], Resultado, Resultado). 