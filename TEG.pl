continente(americaDelSur).
continente(americaDelNorte).
continente(asia).
continente(oceania).

estaEn(americaDelSur, argentina).
estaEn(americaDelSur, brasil).
estaEn(americaDelSur, chile).
estaEn(americaDelSur, uruguay).
estaEn(americaDelNorte, alaska).
estaEn(americaDelNorte, yukon).
estaEn(americaDelNorte, canada).
estaEn(americaDelNorte, oregon).
estaEn(asia, kamtchatka).
estaEn(asia, china).
estaEn(asia, siberia).
estaEn(asia, japon).
estaEn(oceania,australia).
estaEn(oceania,sumatra).
estaEn(oceania,java).
estaEn(oceania,borneo).

jugador(amarillo).
jugador(magenta).
jugador(negro).
jugador(blanco).

aliados(X,Y):- alianza(X,Y).
aliados(X,Y):- alianza(Y,X).
alianza(amarillo,magenta).

%el numero son los ejercitos
ocupa(argentina, magenta, 5).
ocupa(chile, negro, 3).
ocupa(brasil, amarillo, 8).
ocupa(uruguay, magenta, 5).
ocupa(alaska, amarillo, 7).
ocupa(yukon, amarillo, 1).
ocupa(canada, amarillo, 10).
ocupa(oregon, amarillo, 5).
ocupa(kamtchatka, negro, 6).
ocupa(china, amarillo, 2).
ocupa(siberia, amarillo, 5).
ocupa(japon, amarillo, 7).
ocupa(australia, negro, 8).
ocupa(sumatra, negro, 3).
ocupa(java, negro, 4).
ocupa(borneo, negro, 1).

% Usar este para saber si son limitrofes ya que es una relacion simetrica
sonLimitrofes(X, Y) :- limitrofes(X, Y).
sonLimitrofes(X, Y) :- limitrofes(Y, X).

limitrofes(argentina,brasil).
limitrofes(argentina,chile).
limitrofes(argentina,uruguay).
limitrofes(uruguay,brasil).
limitrofes(alaska,kamtchatka).
limitrofes(alaska,yukon).
limitrofes(canada,yukon).
limitrofes(alaska,oregon).
limitrofes(canada,oregon).
limitrofes(siberia,kamtchatka).
limitrofes(siberia,china).
limitrofes(china,kamtchatka).
limitrofes(japon,china).
limitrofes(japon,kamtchatka).
limitrofes(australia,sumatra).
limitrofes(australia,java).
limitrofes(australia,borneo).
limitrofes(australia,chile).

%puedenAtacarse/2 que relaciona dos jugadores si ocupan al menos un par de países que son limítrofes

puedenAtacarse(J1, J2):- 
			ocupa(P1, J1,_), 
			ocupa(P2, J2,_), 
			J1\=J2, 
			sonLimitrofes(P1, P2).

%estaTodoBien/2 que relaciona dos jugadores que, o bien no pueden atacarse, o son aliados.
%Entre el amarillo y el blanco está todo bien.

estaTodoBien(J1, J2):- 
			jugador(J1), jugador(J2), 
			J1\=J2, 
			not(puedenAtacarse(J1,J2)).
estaTodoBien(J1, J2):- 
			jugador(J1), 
			jugador(J2), 
			aliados(J1, J2).

%loLiquidaron/1 que se cumple para un jugador si no ocupa ningún país.
%Al blanco lo liquidaron

loLiquidaron(J):- 
			jugador(J), 
			not(ocupa(_,J,_)).

%ocupaContinente/2 que relaciona un jugador y un continente si el jugador ocupa todos los países del mismo.
%Por ejemplo, el amarillo ocupa todo americaDelNorte.

ocupaContinente(J,C):- 
			continente(C), 
			jugador(J), 
			forall(estaEn(C,Pais), ocupa(Pais,J,_)).

%estaPeleado/1 que se cumple para los continentes en los cuales cada jugador ocupa algún país.
%En este ejemplo no hay continentes peleados. Pero si sacamos el hecho jugador(blanco) entonces americaDelSur está peleado.

estaPeleado(C):- 
			continente(C), 
			forall(jugador(J), ocupaUnPaisDelContinente(J,C)).
ocupaUnPaisDelContinente(J,C):- 
			jugador(J), 
			continente(C), 
			estaEn(C,Pais), 
			ocupa(Pais,J,_).

%seAtrinchero/1 que se cumple para los jugadores que ocupan países en un único continente.
%Por ejemplo, el magenta se atrincheró en américa del sur.

seAtrinchero(J):- 
			jugador(J), 
			continente(C), 
			forall(ocupa(Pais,J,_), estaEn(C,Pais)).

%puedeConquistar/2 que relaciona un jugador y un continente si no ocupa dicho continente, pero todos los países del mismo que no
%tiene son limítrofes a alguno que ocupa y a su vez ese país no es de un aliado.
%Tanto el amarillo como el negro pueden conquistar asia, ninguno más está en condiciones de conquistar otros continentes.

puedeConquistar(J,C):- 
			jugador(J), 
			continente(C),
			not(ocupaContinente(J,C)),
			forall(paisesDeContinenteQueNoTiene(P,J,C), puedeConquistarpais(J,P2)).
puedeConquistarpais(J,P):-
			jugador(J),
			ocupa(P,J2,_),
			ocupa(P2,J2,_),
			not(aliados(J,J2)),
			sonLimitrofes(P,P2).

paisesDeContinenteQueNoTiene(P,J,C):- 
			jugador(J),
			continente(C),
			estaEn(C,P),
			not(ocupa(P,J,_)).

% elQueTieneMasEjercitos/2 que relaciona un jugador y un país si se cumple que es en ese país que hay más ejércitos que en los
%países del resto del mundo y a su vez ese país es ocupado por ese jugador.
%En la base de conocimiento provista, el que tiene más ejércitos es el amarillo, en canadá
elQueTieneMasEjercitos(J,P):- ocupa(P,J,E), forall(ocupa(_,_,Ej), E >= Ej).

% juntan/3 que relaciona dos países y una cantidad, cuando la cantidad representa la suma de los ejércitos en ambos países.
juntan(P1,P2,C):- ocupa(P1,_,E1), ocupa(P2,_,E2), C is E1+E2.

%10 seguroGanaContra/2 que relaciona dos países limítrofes de diferentes jugadores y es cierto cuando el primero tiene más del
%doble de ejércitos que el segundo.
seguroGanaContra(P1,P2):- sonLimitrofes(P1,P2),
			ocupa(P1,J1,E1),
			ocupa(P2,J2,E2),
			 J1 \= J2,
			E1 > (2*E2).
			
%cuantoAgregaParaGanarSeguro/3 que relaciona dos países limítrofes de diferentes jugadores y una cantidad, y es cierto cuando
%esa cantidad es la cantidad de ejércitos que tengo que ponerle al primer país para que le gane seguro al segundo.
%¡No repetir lógica!		

cuantoAgregaParaGanarSeguro(P1, P2, Cant):-
    ocupa(P1, _ , Ejercito1),
    juntan(P2, P2, Total),
    Cantidad = Total * 2 - Ejercito1.
						 
