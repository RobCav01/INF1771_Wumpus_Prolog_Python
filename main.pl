
:-dynamic posicao/3.
:-dynamic memory/3.
:-dynamic visitado/2.
:-dynamic certeza/2.
:-dynamic energia/1.
:-dynamic pontuacao/1.
:-dynamic ori_raccolti/1.
:-dynamic celle_pozioni/1.

:-consult('mapa.pl').

delete([], _, []).
delete([Elem|Tail], Del, Result) :-
    (   \+ Elem \= Del
    ->  delete(Tail, Del, Result)
    ;   Result = [Elem|Rest],
        delete(Tail, Del, Rest)
    ).
	


reset_game :- retractall(memory(_,_,_)), 
			retractall(visitado(_,_)), 
			retractall(certeza(_,_)),
			retractall(energia(_)),
			retractall(pontuacao(_)),
			retractall(posicao(_,_,_)),
			assert(energia(100)),
			assert(pontuacao(0)),
			assert(posicao(1,1, norte)),
    		abolish(celle_pozioni/1),
    		asserta(celle_pozioni([])).


:-reset_game.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Controle de Status
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%atualiza pontuacao
atualiza_pontuacao(X):- pontuacao(P), retract(pontuacao(P)), NP is P + X, assert(pontuacao(NP)),!.

%atualiza energia
atualiza_energia(N):- energia(E), retract(energia(E)), NE is E + N, 
					(
					 (NE =<0, assert(energia(0)),posicao(X,Y,_),retract(posicao(_,_,_)), assert(posicao(X,Y,morto)),!);
					 (NE >100, assert(energia(100)),!);
					  (NE >0,assert(energia(NE)),!)
					 ).

%verifica situacao da nova posicao e atualiza energia e pontos
verifica_player :- posicao(X,Y,_), tile(X,Y,'P'), atualiza_energia(-100), atualiza_pontuacao(-1000),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'D'), random_between(-80,-50,D), atualiza_energia(D),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'d'), random_between(-50,-25,D), atualiza_energia(D),!.
verifica_player :- posicao(X,Y,Z), tile(X,Y,'T'), 
					map_size(SX,SY), random_between(1,SX,NX), random_between(1,SY,NY),
				retract(posicao(X,Y,Z)), assert(posicao(NX,NY,Z)), atualiza_obs, verifica_player,!.
verifica_player :- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comandos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%virar direita
virar_direita :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.

%virar esquerda
virar_esquerda :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.

%andar
andar :- posicao(X,Y,P), P = norte, map_size(_,MAX_Y), Y < MAX_Y, YY is Y + 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.
		 
andar :- posicao(X,Y,P), P = sul,  Y > 1, YY is Y - 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = leste, map_size(MAX_X,_), X < MAX_X, XX is X + 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = oeste,  X > 1, XX is X - 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.
		 
%pegar	
pegar :- posicao(X,Y,_), tile(X,Y,'O'), retract(tile(X,Y,'O')), assert(tile(X,Y,'')), atualiza_pontuacao(-5), atualiza_pontuacao(500),set_real(X,Y),!. 
pegar :- posicao(X,Y,_), tile(X,Y,'U'), retract(tile(X,Y,'U')), assert(tile(X,Y,'')), atualiza_pontuacao(-5), atualiza_energia(50),set_real(X,Y),!. 
pegar :- atualiza_pontuacao(-5),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funcoes Auxiliares de navegação e observação
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		 
%Define as 4 adjacencias		 
adjacente(X, Y) :- posicao(PX, Y, _), map_size(MAX_X,_),PX < MAX_X, X is PX + 1.  
adjacente(X, Y) :- posicao(PX, Y, _), PX > 1, X is PX - 1.  
adjacente(X, Y) :- posicao(X, PY, _), map_size(_,MAX_Y),PY < MAX_Y, Y is PY + 1.  
adjacente(X, Y) :- posicao(X, PY, _), PY > 1, Y is PY - 1.  

%cria lista com a adjacencias
adjacentes(L) :- findall(Z,(adjacente(X,Y),tile(X,Y,Z)),L).

%define observacoes locais
observacao_loc(brilho,L) :- member('O',L).
observacao_loc(reflexo,L) :- member('U',L).

%define observacoes adjacentes
observacao_adj(brisa,L) :- member('P',L).
observacao_adj(palmas,L) :- member('T',L).
observacao_adj(passos,L) :- member('D',L).
observacao_adj(passos,L) :- member('d',L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tratamento de KB e observações
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%consulta e processa observações
atualiza_obs:-adj_cand_obs(LP), observacoes(LO), iter_pos_list(LP,LO), observacao_certeza, observacao_vazia.

%adjacencias candidatas p/ a observacao (aquelas não visitadas)
adj_cand_obs(L) :- findall((X,Y), (adjacente(X, Y), \+visitado(X,Y)), L).

%cria lista de observacoes
observacoes(X) :- adjacentes(L), findall(Y, observacao_adj(Y,L), X).

%itera posicoes da lista para adicionar observacoes
iter_pos_list([], _) :- !.
iter_pos_list([H|T], LO) :- H=(X,Y), 
							((corrige_observacoes_antigas(X, Y, LO),!);
							adiciona_observacoes(X, Y, LO)),
							iter_pos_list(T, LO).							 

%Corrige observacoes antigas na memoria que ficaram com apenas uma adjacencia
corrige_observacoes_antigas(X, Y, []):- \+certeza(X,Y), memory(X,Y,[]).
corrige_observacoes_antigas(X, Y, LO):-
	\+certeza(X,Y), \+ memory(X,Y,[]), memory(X, Y, LM), intersection(LO, LM, L), 
	retract(memory(X, Y, LM)), assert(memory(X, Y, L)).

%Adiciona observacoes na memoria
adiciona_observacoes(X, Y, _) :- certeza(X,Y),!.
adiciona_observacoes(X, Y, LO) :- \+certeza(X,Y), \+ memory(X,Y,_), assert(memory(X, Y, LO)).

%Quando há apenas uma observação e uma unica posição incerta, deduz que a observação está na casa incerta
%e marca como certeza
observacao_certeza:- findall((X,Y), (adjacente(X, Y), 
						((\+visitado(X,Y), \+certeza(X,Y));(certeza(X,Y),memory(X,Y,ZZ),ZZ\=[])),
						memory(X,Y,Z), Z\=[]), L), ((length(L,1),L=[(XX,YY)], assert(certeza(XX,YY)),!);true).

%Quando posição não tem observações
observacao_vazia:- adj_cand_obs(LP), observacao_vazia(LP).
observacao_vazia([]) :- !.
observacao_vazia([H|T]) :- H=(X,Y), ((memory(X,Y,[]), \+certeza(X,Y),assert(certeza(X,Y)),!);true), observacao_vazia(T).

%Quando posicao é visitada, atualiza memoria de posicao com a informação real do mapa 
set_real(X,Y):- ((retract(certeza(X,Y)), assert(certeza(X,Y)),!); assert(certeza(X,Y))), set_real2(X,Y),!.
set_real2(X,Y):- tile(X,Y,'P'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brisa])),!);assert(memory(X,Y,[brisa]))),!.
set_real2(X,Y):- tile(X,Y,'O'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brilho])),!);assert(memory(X,Y,[brilho]))),!.
set_real2(X,Y):- tile(X,Y,'T'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[palmas])),!);assert(memory(X,Y,[palmas]))),!.
set_real2(X,Y):- ((tile(X,Y,'D'),!); tile(X,Y,'d')), ((retract(memory(X,Y,_)),assert(memory(X,Y,[passos])),!);assert(memory(X,Y,[passos]))),!.
set_real2(X,Y):- tile(X,Y,'U'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[reflexo])),!);assert(memory(X,Y,[reflexo]))),!.
set_real2(X,Y):- tile(X,Y,''), ((retract(memory(X,Y,_)),assert(memory(X,Y,[])),!);assert(memory(X,Y,[]))),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa real
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_player(X,Y) :- posicao(X,Y, norte), write('^'),!.
show_player(X,Y) :- posicao(X,Y, oeste), write('<'),!.
show_player(X,Y) :- posicao(X,Y, leste), write('>'),!.
show_player(X,Y) :- posicao(X,Y, sul), write('v'),!.
show_player(X,Y) :- posicao(X,Y, morto), write('+'),!.

%show_position(X,Y) :- show_player(X,Y),!.
show_position(X,Y) :- (show_player(X,Y); write(' ')), tile(X,Y,Z), ((Z='', write(' '));write(Z)),!.

show_map :- map_size(_,MAX_Y), show_map(1,MAX_Y),!.
show_map(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_position(X,Y), write(' | '), XX is X + 1, show_map(XX, Y),!.
show_map(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_map(1, YY),!.
show_map(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa conhecido
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_mem_info(X,Y) :- memory(X,Y,Z), 
		((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
		((member(brisa, Z), write('P'));write(' ')),
		((member(palmas, Z), write('T'));write(' ')),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!.

show_mem_info(X,Y) :- \+memory(X,Y,[]), 
			((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
			write('     '),!.		
		
		

show_mem_position(X,Y) :- posicao(X,Y,_), 
		((visitado(X,Y), write('.'),!); (certeza(X,Y), write('!'),!); write(' ')),
		write(' '), show_player(X,Y),
		((memory(X,Y,Z),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!);
		(write('   '),!)).

		
show_mem_position(X,Y) :- show_mem_info(X,Y),!.


show_mem :- map_size(_,MAX_Y), show_mem(1,MAX_Y),!.
show_mem(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_mem_position(X,Y), write('|'), XX is X + 1, show_mem(XX, Y),!.
show_mem(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_mem(1, YY),!.
show_mem(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%apagar esta linha - apenas para demonstracao aleatoria
%executa_acao(X) :- L=['virar_esquerda','virar_direita','andar','pegar'],random_between(1,4,I), nth1(I, L, X),!.

%apagar linhas abaixo... sao exemplos de resposta
%executa_acao(andar) :- posicao(PX, _, oeste), PX > 1, X = andar,!.
%executa_acao(andar) :- posicao(PX, _, leste), PX < 3, X = andar,!.
%executa_acao(pegar) :- posicao(PX, PY,_), tem_ouro(PX, PY), !.
%executa_acao(voltar) :- peguei_todos_ouros,!.

%executa_acao(pegar) :- tem_ouro(posicao).	%prende oro
%executa_acao(tornare) :- peguei_todos_ouros, !.

ori_raccolti(0).
inizializza_celle_pozioni :-
    abolish(celle_pozioni/1),
    asserta(celle_pozioni([])).
inizializza_celle_pozioni.


aggiungi_cella_pozione(X, Y) :-
    retract(celle_pozioni(ListaAttuale)),
    NuovaLista = [(X, Y) | ListaAttuale],	% Aggiunge (X, Y) in cima
    assert(celle_pozioni(NuovaLista)),	% Aggiorna la lista
	celle_pozioni(Celle),
	write("Aggiunta pozione, celle_pozioni: "),
	write(Celle).

rimuovi_cella_pozione(X, Y) :-
    retract(celle_pozioni(ListaAttuale)),	
    delete(ListaAttuale, (X, Y), NuovaLista),	% Rimuove (X, Y) dalla lista
    assert(celle_pozioni(NuovaLista)),	% Aggiorna la lista
	celle_pozioni(Celle),
	write("Rimossa pozione, celle_pozioni: "),
	write(Celle).


presente_in_celle_pozioni(X, Y) :-
    celle_pozioni(Celle),	% false se celle_pozioni è vuoto (ciò va bene perchè se è vuoto non può contenere (X, Y))
    member((X, Y), Celle).

% Calcolo della distanza di Manhattan
manhattan_dist(X1, Y1, X2, Y2, Dist) :-
    Dist is abs(X2 - X1) + abs(Y2 - Y1).

explore_certeza(X, Y) :-
    posicao(X_pl, Y_pl, _),  % Posizione del player
    findall(
        (Dist, X, Y),
        (   certeza(X, Y),          % La cella deve essere certa,
            \+ (X = X_pl, Y = Y_pl), % diversa dalla posizione attuale del giocatore,
            \+ visitado(X, Y),       % non visitata,
            \+ tile(X, Y, 'P'),      % non un burrone,
            \+ tile(X, Y, 'D'),      % non un mostro grande,
            \+ tile(X, Y, 'd'),      % non un mostro piccolo,
            \+ tile(X, Y, 'T'),      % non un pipistrello
            manhattan_dist(X_pl, Y_pl, X, Y, Dist)  % Calcola la distanza tra cella e player
        ),
        CelleConDistanza
    ),
    sort(1, @=<, CelleConDistanza, [(_, X, Y) | _]).  % Ordina per distanza crescente e prende il primo elemento
    %min_member((_, X, Y), CelleConDistanza).	% Seleziona la cella con la distanza minima

explore_not_certeza(X, Y) :-
    findall(
        (AX, AY),
        (
            adjacente(AX, AY),		% Trova le celle adiacenti
            \+ visitado(AX, AY),	% La cella non è stata visitata
            (
				(
					certeza(AX, AY),        % Se la cella è certa
                	\+ tile(AX, AY, 'P'),	% non deve essere un burrone
                	\+ tile(AX, AY, 'D'),	% né un mostro grande
					\+ tile(AX, AY, 'd'),	% né un mostro piccolo
                	\+ tile(AX, AY, 'T')	% né un pipistrello
				)   
            	;   	
				\+ certeza(AX, AY)	% Oppure non è certa
            )
        ),
        CelleAdiacentiPossibili
    ),
    random_member((X, Y), CelleAdiacentiPossibili).	% Seleziona casualmente una cella

% Per sapere se in (X, Y) si ha in memoria uno tra '.   D' o '?   D'
presenza_osservaz_mostro(X, Y) :-
    memory(X, Y, Z),
    member(passos, Z).          % Presenza di passi (ovvero 'D') nella memoria

% Per sapere se in (X, Y) si ha in memoria '?   D'
presenza_incertezza_mostro(X, Y) :-
    presenza_osservaz_mostro(X, Y),
    \+ visitado(X, Y),          % La cella non è stata visitata
    \+ certeza(X, Y).           % La cella non è certa (ovvero '?')

presenza_incertezza_mostro_1_param((X, Y)) :-
    presenza_incertezza_mostro(X, Y).

% Trova la pozione nota più vicina al player
pozione_piu_vicina(X, Y) :-
    celle_pozioni(Celle),
    posicao(X_pl, Y_pl, _),	% Posizione del player
    
    findall(
        (Dist, X_curr, Y_curr),
        (
			member((X_curr, Y_curr), Celle),  % Itera su ogni posizione nella lista
			manhattan_dist(X_pl, Y_pl, X_curr, Y_curr, Dist)
		),
        DistanzeCelle
    ),
    min_member((_, X, Y), DistanzeCelle).	% Trova la cella con la distanza minima


executa_acao(prendere) :-
	posicao(X, Y, _), memory(X, Y, Z), Z = [brilho],
	% aggiornamento ori_raccolti
	ori_raccolti(N),
    N2 is N + 1,
    retract(ori_raccolti(N)),
    assert(ori_raccolti(N2)).

executa_acao(tornare) :- ori_raccolti(N), N = 3, !.

executa_acao(scappare) :- posicao(X, Y, _), memory(X, Y, Z), Z = [passos], !.


executa_acao(annota_pozione) :- posicao(X, Y, _), memory(X, Y, Z), Z = [reflexo],
	\+presente_in_celle_pozioni(X, Y),	% Controlla se la pozione è già stata annotata
	aggiungi_cella_pozione(X, Y), !.

executa_acao(prendere) :- energia(E), E =< 80, posicao(X, Y, _), memory(X, Y, Z), Z = [reflexo], 
	rimuovi_cella_pozione(X, Y), !.

executa_acao(Acao) :-
	energia(E), E =< 80,
	pozione_piu_vicina(X, Y),
	format(atom(Acao), 'esplorare_certeza(~w,~w)', [X, Y]), !.

executa_acao(Acao) :-
    findall((X, Y), adjacente(X, Y), Adjacentes),
    forall(member((X, Y), Adjacentes),
        (
            presenza_osservaz_mostro(X, Y)
        )
    ),
	include(
		presenza_incertezza_mostro_1_param,
        Adjacentes,
        AdjacentesPossibili
    ),
    random_member((X, Y), AdjacentesPossibili),
    format(atom(Acao), 'esplorare_incerteza(~w,~w)', [X, Y]), !.

executa_acao(Acao) :-
    (   
        (
			explore_certeza(X, Y),
			Modalita = "certeza"
			;
			explore_not_certeza(X, Y),
			Modalita = "incerteza"
		)
    	->  format(atom(Acao), 'esplorare_~w(~w,~w)', [Modalita, X, Y])
    ), !.

% vai verso la cella visitata più vicina al player che abbia almeno una cella adiacente incerta
% (questo perchè poi si forzerà il player ad esplorare quella cella incerta)
executa_acao(Acao) :-
    (   
        (
			cella_visitata_adj_a_incertezza_piu_vicina(X, Y)
		)
    	->  format(atom(Acao), 'esplorare_certeza(~w,~w)', [X, Y])
    ), !.


adiacenti_a(X, Y, AX, Y) :- map_size(MAX_X, _), X < MAX_X, AX is X + 1.
adiacenti_a(X, Y, AX, Y) :- X > 1, AX is X - 1.
adiacenti_a(X, Y, X, AY) :- map_size(_, MAX_Y), Y < MAX_Y, AY is Y + 1.
adiacenti_a(X, Y, X, AY) :- Y > 1, AY is Y - 1.

cella_visitata_adj_a_incertezza(X, Y) :-
	visitado(X, Y),
	findall(
		(AX, AY),
		(
			adiacenti_a(X, Y, AX, AY),	% Ottiene una cella adiacente
			\+ visitado(AX, AY),		% La cella adiacente non deve essere visitata
    		\+ certeza(AX, AY)			% La cella adiacente non deve essere certa
		),
		AdiacentiIncerte
	),
	AdiacentiIncerte \= [].
	
cella_visitata_adj_a_incertezza_piu_vicina(X, Y) :-
	posicao(X_pl, Y_pl, _),	% Posizione del player
    % Trova tutte le celle che rispettano la regola "cella_visitata_adj_a_incertezza(X,Y)"
    findall(
		(Dist, X_curr, Y_curr), 
		(
			cella_visitata_adj_a_incertezza(X_curr, Y_curr),
			manhattan_dist(X_pl, Y_pl, X_curr, Y_curr, Dist)
		),
		CelleDistanze
	),
    min_member((_, X, Y), CelleDistanze).	% Seleziona la cella con la distanza minima