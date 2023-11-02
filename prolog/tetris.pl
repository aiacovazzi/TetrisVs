/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
To do (29/10):
-assert next move
-implementazione minmax/maxmax per la ricerca della miglior mossa in multi e single player.

To do (5/11):
-modularizzare codice
-implementare il logger che spieghi le mosse
    prende la catena di valutazione delle mosse e ne siega la ratio

Bonus:
-implementazione delle sessioni
- algoritmo genetico per pesi euristica di valutazione??
*/

:- use_module(library(lists)).
:- use_module(planner).
:- use_module(minmax).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Web Server Interface Config%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_setting(http:cors, [*]).
:- http_server(http_dispatch, [port(7777)]).

%the root is for debugging printing the current GameBoard.
:- http_handler(root(home), writeGB, []).

writeGB(_Request) :-
    format('Content-type: text/plain~n~n'),
    writeGameBoard.

%ocell/R/C (assert an occupied cell)
:- http_handler(root(ocell/R/C), reqOccCell(R,C), []).

reqOccCell(R,C,_Request) :-
    cors_enable,
    atom_number(R, R1),
    atom_number(C, C1),
    assertz(occCell(R1,C1)),
    reply_json_dict('ok').

%retcell(retract all the occupied cell)
:- http_handler(root(retcell), retractCell, []).

retractCell(_Request) :-
    cors_enable,
    retractall(occCell(_,_)),
    reply_json_dict('ok').

%newgb(compute a new gb after row cleaning)
:- http_handler(root(newgb), newGb, []).

newGb(_Request) :-
    cors_enable,
    computeNewGameBoard,
    reply_json_dict('ok').

%start/T/R/C (assert the starting tetramin0)
:- http_handler(root(start/T/R/C), reqStart(T,R,C), []).

reqStart(T,R,C,_Request) :-
    cors_enable,
    retractall(start(_)),
    string_to_atom(T,T1),
    atom_number(R, R1),
    atom_number(C, C1),
    assertz(start((T1,R1,C1))),
    reply_json_dict('ok').

%path find the best path for the tetramino
:- http_handler(root(path/T), path(T), []).

path(T,_Request) :-
    cors_enable,
    getPathOfBestMove(T,Plan),
    reply_json_dict(Plan).
%
%//////////////////////////////////////////////////////////

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Defining the game board properties and the occupied cells%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gameBoardW(10).
gameBoardH(20).

:-dynamic(occCell/2).
%the starting position of the playing tetramino
%tetramino(T,R,C).
:-dynamic(start/1).

occCell(19,1).
occCell(19,2).
occCell(19,3).
occCell(19,4).
occCell(19,5).
occCell(19,6).
occCell(19,7).
occCell(19,8).
occCell(19,9).
occCell(18,1).
occCell(18,2).
occCell(16,1).
occCell(16,2).
occCell(17,1).
occCell(17,2).
occCell(17,3).
occCell(17,4).
occCell(17,5).
occCell(17,6).
occCell(17,7).
occCell(17,8).
occCell(17,9).

/*
occCell(R,C) :-
    http_session_data(occCell(R,C)),!.
*/

%%%%%%%%%%%%%%%%%
%Auxiliary rules%
%%%%%%%%%%%%%%%%%

%solve equations in the form
% X = Y + Z
%wrt the unknow value
%used by fitPiece
eq(X,Y,Z) :-
    var(X),
    X is Y + Z,
    !.

eq(X,Y,Z) :-
    var(Y),
    Y is X - Z,
    !.

eq(X,Y,Z) :-
    var(Z),
    Z is X - Y,
    !.

%logarithm in base 2
logBase2(0,0) :- !.

logBase2(X,R) :-
    N is log10(X),
    D is log10(2),
    R is N/D.

%auxiliary number generator used by freeCell1
gen(Cur, Top, Cur):- Cur < Top.

gen(Cur, Top, Next):-
  Cur < Top,
  Cur1 is Cur+1,
  gen(Cur1, Top, Next).


%auxiliary assert and retract for list of facts
assertList([]).

assertList([H|T]):-
    assertz(H),
    assertList(T).
	
retractList([]).

retractList([H|T]):-
    retract(H),
    retractList(T).

%    
%/////////////////////////////////

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Tetraminos' operators and checks%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Mapping each tetramino with its rotations.
%Can be used also to obtain the next rotation of a given tetramino.
rotation(o,o1,o1).
rotation(i, i1, i2).
rotation(i, i2, i1).
rotation(z, z1, z2).
rotation(z, z2, z1).
rotation(s, s1, s2).
rotation(s, s2, s1).
rotation(t, t1, t2).
rotation(t, t2, t3).
rotation(t, t3, t4).
rotation(t, t4, t1).
rotation(l, l1, l2).
rotation(l, l2, l3).
rotation(l, l3, l4).
rotation(l, l4, l1).
rotation(j, j1, j2).
rotation(j, j2, j3).
rotation(j, j3, j4).
rotation(j, j4, j1).

%Check if a cell is avalaible and if the indexes are not out of bound.
freeCell(R,C) :- 
    gameBoardH(H), 
    gameBoardW(W), 
    R >= 0, R<H, 
    C >=0, C<W, 
    \+occCell(R,C).

%Generate all possible free cell on the gameboard
freeCell1(R,C) :- 
    gameBoardH(H),
    gen(0, H, R),    
    gameBoardW(W),
    gen(0, W, C),    
    \+occCell(R,C).

%Check if there is space for an a tetromino on the gameboard given the reference point.
%R1;C1 is the reference point.
%Some of the fitPiece rules uses the "when" predicate available in the "Constraint Logic Programming"
%library of SwiProlog in order to delay the execution of eq/3 when the information are available.
%https://www.swi-prolog.org/pldoc/man?section=clp

%O Tetramino
%[ ][ ]
%[ ][x]
%[R4;C4][R2;C2]
%[R3;C3][R1;C1]

fitPiece(o1,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C2 = C1,    
    R3 = R1,    
    R4 = R2,
    C4 = C3,
    eq(C3,C1,-1),
    eq(R2,R1,-1),
    freeCell(R1,C1),
    freeCell(R2,C2),
    freeCell(R3,C3),
    freeCell(R4,C4).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%I Tetramino
%[ ][x][ ][ ]
%[R2;C2][R1;C1][R3;C3][R4;C4]
fitPiece(i1,R1,C1,R2,C2,R3,C3,R4,C4) :-
    R1 = R2,
    R2 = R3,
    R3 = R4,
    eq(C2,C1,-1),
    eq(C3,C1,+1),
    eq(C4,C1,+2),
    freeCell(R1,C1),
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).
    
%[ ]
%[x]
%[ ]
%[ ]
%[R2;C2]
%[R1;C1]
%[R3;C3]
%[R4;C4]
fitPiece(i2,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C2,
	C2 = C3,
	C3 = C4,
	eq(R2,R1,-1),
	eq(R3,R1,+1),
	eq(R4,R1,+2),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%S Tetramino
%   [ ][ ]
%[ ][x]
%       [R3;C3][R4;C4]
%[R2;C2][R1;C1]

fitPiece(s1,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C3,
    R1 = R2,
    R4 = R3,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C3,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).


%[ ]
%[ ][x]
%   [ ]
%[R4;C4]
%[R3;C3][R1;C1]
%       [R2;C2]

fitPiece(s2,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C2,
    R1 = R3,
    C3 = C4,
	eq(R2,R1,+1),
    eq(R4,R3,-1),
    eq(C3,C1,-1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Z Tetramino
%S Tetramino
%[ ][ ]
%   [x][ ]
%[R4;C4][R3;C3]
%       [R1;C1][R2;C2]
fitPiece(z1,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C3,
    R1 = R2,
    R4 = R3,
    eq(C2,C1,+1),
    eq(R3,R1,-1),
    eq(C4,C3,-1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%   [ ]
%[ ][x]
%[ ]
%       [R4;C4]
%[R3;C3][R1;C1]
%[R2;C2]

fitPiece(z2,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C4,
    R1 = R3,
    C3 = C2,
	eq(R2,R1,+1),
    eq(R4,R1,-1),
    eq(C3,C1,-1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%T Tetramino
%   [ ]
%[ ][x][ ]
%       [R3;C3]
%[R2;C2][R1;C1][R4;C4]

fitPiece(t1,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C3,
    R1 = R2,
    R1 = R4,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%   [ ]
%   [x][ ]
%   [ ]
%   [R3;C3]
%   [R1;C1][R4;C4]
%   [R2;C2]
fitPiece(t2,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C3,
    R1 = R4,
    C1 = C2,
	eq(R2,R1,+1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%   
%[ ][x][ ]
%   [ ]
%   
%[R3;C3][R1;C1][R4;C4]
%       [R2;C2]
fitPiece(t3,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    R1 = R3,
    R1 = R4,
    C1 = C2,
	eq(R2,R1,+1),
    eq(C3,C1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%   [ ]
%[ ][x]
%   [ ]
%       [R4;C4]
%[R3;C3][R1;C1]
%       [R2;C2]
fitPiece(t4,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    R1 = R3,
    C1 = C2,
    C1 = C4,
	eq(R2,R1,+1),
    eq(C3,C1,-1),
    eq(R4,R1,-1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%J Tetramino
%[ ]
%[ ][x][ ]
%[R3;C3]
%[R2;C2][R1;C1][R4;C4]

fitPiece(j1,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    R1 = R2,
    R1 = R4,
    C2 = C3,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%   [ ][ ]
%   [x]
%   [ ]
%   [R2;C2][R3;C3]
%   [R1;C1]
%   [R4;C4]

fitPiece(j2,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C1 = C2,
    C1 = C4,
    R3 = R2,
	eq(R2,R1,-1),
    eq(R4,R1,+1),
    eq(C3,C2,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%
%[ ][x][ ]
%      [ ]
%[R2;C2][R1;C1][R3;C3]
%              [R4;C4]

fitPiece(j3,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    R2 = R1,
    R3 = R1,
    C3 = C4,
	eq(C2,C1,-1),
    eq(C3,C1,+1),
    eq(R4,R3,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%   [ ]
%   [x]
%[ ][ ]
%       [R2;C2]
%       [R1;C1]
%[R4;C4][R3;C3]       

fitPiece(j4,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C2 = C1,
    C3 = C1,
    R4 = R3,
	eq(R2,R1,-1),
    eq(R3,R1,+1),
    eq(C4,C3,-1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%L Tetramino
%      [ ]
%[ ][x][ ]
%              [R3;C3]
%[R2;C2][R1;C1][R4;C4]

fitPiece(l1,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    R1 = R2,
    R1 = R4,
    C4 = C3,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%   [ ]
%   [x]
%   [ ][ ]
%   [R2;C2]
%   [R1;C1]
%   [R3;C3][R4;C4]       

fitPiece(l2,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C2 = C1,
    C3 = C1,
    R4 = R3,
	eq(R2,R1,-1),
    eq(R3,R1,+1),
    eq(C4,C3,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%
%[ ][x][ ]
%[ ]              
%[R2;C2][R1;C1][R4;C4]
%[R3;C3]

fitPiece(l3,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    R1 = R2,
    R1 = R4,
    C2 = C3,
	eq(C2,C1,-1),
    eq(R3,R2,+1),
    eq(C4,C1,+1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

%[ ][ ]
%   [x]
%   [ ]
%[R4;C4][R2;C2]
%       [R1;C1]
%       [R3;C3]       

fitPiece(l4,R1,C1,R2,C2,R3,C3,R4,C4) :- 
    C2 = C1,
    C3 = C1,
    R4 = R2,
	eq(R2,R1,-1),
    eq(R3,R1,+1),
    eq(C4,C2,-1),
    freeCell(R1,C1),    
    freeCell(R2,C2),
    freeCell(R3,C3),	
    freeCell(R4,C4).

retractRows([]).

retractRows([R|T]) :-
    retractall(occCell(R,_)),
    retractRows(T).

cellToShift(R,C,R1) :-
    occCell(R,C),
    R<R1.

shiftOccCell(N, RsL, NRL) :-
    shiftOccCell(N, RsL, NRL, []).

shiftOccCell(N, [], Sl, Sl).

shiftOccCell(N, [occCell(R,C)|T], NRL, Sl) :-
    R1 is R + N,
    append(Sl, [occCell(R1,C)], Sl1),
    shiftOccCell(N, T, NRL, Sl1).

shift([H|T]):-
    findall((occCell(R,C)), cellToShift(R, C, H), RsL),
    shiftOccCell(1, RsL, NRL),
    retractList(RsL),
    assertList(NRL),
    shift(T).

shift([]).

computeNewGameBoard() :-
    setof((R),clearedRow(R),ClearedRows),
    retractRows(ClearedRows),
    shift(ClearedRows),
    !.

computeNewGameBoard() :- !.

%Put the piece in the given reference point and then assert all the occupied cells.
%In -LGb there is the list of occupied cells before the execution of the move.
placePiece(T,R1,C1,LGbpre,LGbpost) :-
    fitPiece(T,R1,C1,R2,C2,R3,C3,R4,C4),
    findall((occCell(R,C)), occCell(R, C), LGbpre),
    L = [occCell(R1,C1),occCell(R2,C2),occCell(R3,C3),occCell(R4,C4)],
    assertList(L),
    computeNewGameBoard,
    findall((occCell(R,C)), occCell(R, C), LGbpost).
%

%Discover all the possible cell where a tetramino can be placed for all its rotations
findPossibleGoals(T,List) :- 
    findall((T1), rotation(T, T1, _), Lr),
    findPossibleGoals0([],Lr,List).

findPossibleGoals0(Found,[],Found).

findPossibleGoals0(Found,[Ht|T],L) :-
    findall((Ht,R,C), tetraminoGoal(Ht,R,C), List),
    append(Found,List,NewList),
    findPossibleGoals0(NewList,T,L).

%a tetramino can be placed on a free cell when the next row collide
tetraminoGoal(T,R,C) :-
    freeCell1(R,C),
    fitPiece(T,R,C,_,_,_,_,_,_),
    R1 is R + 1,
    \+fitPiece(T,R1,C,_,_,_,_,_,_).
%///////////////////

%%%%%%%%%%%%%%%%%%%%%
%GameBoard Evaluator%
%%%%%%%%%%%%%%%%%%%%%

%1) Compute the number of holes in the columns.
%This algorithm consider as holes all the empty cell below an occupied cell of a certain column.
%The holes are often the result of a bad move, so the goal is to choose a move that minimize this number.
holesInColumn(N) :- 
    setof((R,C),holesColumn(R,C),HolesColumn),
    length(HolesColumn,N),
    !.

holesInColumn(0).

holesColumn(R1,C) :-
    gameBoardW(W),
    gen(0, W, C),
    occCell(R,C),
    freeCell1(R1,C),
    R1 > R.

%

nonContinuosSpaceInRows(N) :-
    setof((R,C),nonContinuosSpaceInRows(R,C),NCSIR),
    length(NCSIR,N),
    !.

nonContinuosSpaceInRows(0).

nonContinuosSpaceInRows(R,C) :-
    gameBoardH(H),
    gen(0, H, R),
    freeCell1(R,C),
    occCell(R,C1),
    (C1 > C; C1 < C).



%2) Compute the number of holes in the rows.
%This algorithm consider as holes all the empty cell netx to an occupied cell of a certain row.
%The holes are often the result of a bad move, so the goal is to choose a move that minimize this number.
holesInRow(N) :- 
    setof((R,C),holesRow(R,C),HolesRow),
    length(HolesRow,N),
    !.

holesInRow(0).

holesRow(R,C1) :-
    gameBoardH(H),
    gen(0, H, R),
    occCell(R,C),
    freeCell1(R,C1),
    C1 > C.

%

%3) Compute the etropy of each row, then give average value as measure of the "entropy" of the gameboard.
%Count the occupied cell of a row.
countOccCelInRow(R,N) :- 
    setof((C),occCell(R,C),Occ),length(Occ,N).

%Compute the entropy of a row.
entropyOfRow(R,Ent) :-
    countOccCelInRow(R,Occ),
    gameBoardW(W),
    Free is W - Occ,
    OccRatio is Occ/W,
    FreeRatio is Free/W,
    logBase2(OccRatio,Log2OccRatio),
    logBase2(FreeRatio,Log2FreeRatio),
    Ent is -OccRatio*Log2OccRatio -FreeRatio*Log2FreeRatio.

entropyOfRow(_, 0).

avgEntropyOfGameBoard(AvgEnt) :-
    findall((Ent),entropyOfRow(_,Ent),EntropyXRow), 
    sum_list(EntropyXRow,Sum),
    AvgEnt is Sum.



%

%4) Return the number of the cleared row, this in order to give advantage to the move that actually clear some row.

clearedRow(R) :-
    countOccCelInRow(R,Occ),
    gameBoardW(W),
    Occ = W.

numberOfclearedRow(N) :- 
    setof((R),clearedRow(R),ClearedRow),
    length(ClearedRow,N),
    !.

numberOfclearedRow(0).
%

%Compute the whole GameBoard score
gameBoardScore(S) :-
    holesInColumn(HC),
    nonContinuosSpaceInRows(NCSIR),
    avgEntropyOfGameBoard(AvgEnt),
    S1 is AvgEnt*10+HC+NCSIR,
    S is -1*S1.  

scoreMoves([],M,M).

scoreMoves([(T,R,C)|Tail],M,ScoreList) :-
    placePiece(T,R,C,L,_),
    gameBoardScore(S),
    retractall(occCell(_,_)),
    assertList(L),
    append(M,[[S,(T,R,C)]],M1),
    scoreMoves(Tail,M1,ScoreList).  

%Compute the evalution for each available move and put them in an array
findMoves(T,ScoreList) :-
    findMoves(T,[],ScoreListUnsort),
    sort(1, @>=, ScoreListUnsort, ScoreList).
    
findMoves(T,M,ScoreList) :-
	findPossibleGoals(T,L),
    scoreMoves(L,M,ScoreList).
%////////////////////////////////////////    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Planner%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Define actions
%setof(A, action(A), Action)

action(rotate).
action(right).
action(left).
action(down).

%listing the actual transformation involved by the moves
rotate((T1,R1,C1),(T2,R1,C1)) :-
    rotation(_,T2,T1),
    fitPiece(T2,R1,C1,_,_,_,_,_,_).

left((T1,R1,C1),(T1,R1,C2)) :- 
    C2 is C1+1,
    fitPiece(T1,R1,C2,_,_,_,_,_,_).

right((T1,R1,C1),(T1,R1,C2)) :- 
    C2 is C1-1,
    fitPiece(T1,R1,C2,_,_,_,_,_,_).
    
down((T1,R1,C1),(T1,R2,C1)) :- 
    R2 is R1 - 1,
    fitPiece(T1,R2,C1,_,_,_,_,_,_).
%

%evaluateMovement compute a score for each move given the goal, the score is weighted by the priority of the move.
%the priority is: rotate, [left, right], down.
%the ratio is: 
%   a player first rotate a tetramino in the proper rotation
%   then align it to the point she wants to reach
%   then push down the tetramino in that point
%this sequence of operation is the most common, so priority are shaped around this scenario, however it can deal also with more complex situations.

evaluateMovement([rotate, (T1,_,_), (T2,_,_)], Score) :-
    diff(T1,T2,Diff),
	priority(rotate,Priority),
	Score is  Diff * Priority.

evaluateMovement([left, (_,_,C1), (_,_,C2)], Score) :-
	priority(left,Priority),
	Score is (C2 - C1) * Priority.
	
evaluateMovement([right, (_,_,C1), (_,_,C2)], Score) :-
	priority(right,Priority),
	Score is (C2 - C1) * Priority * -1.

evaluateMovement([down, (_,R1,_), (_,R2,_)], Score) :-
	priority(down,Priority),
	Score is (R2 - R1) * Priority * -1.

priority(left,10).
priority(right,10).
priority(down,1).
priority(rotate,1). %rotate has the minimum priority, it's executed as last move. 

diff(T1,T2,Diff) :-
    T1 == T2,
    Diff is 0,
    !.

diff(_,_,Diff) :-
    Diff is 1,
    !.

%getPathOfBestMove search for the best move and then call the planner for the tetris path problem.
%start and goal are inverted because I want to find the path starting from the goal and coming back to the start.
%This allow to deal easily with particulare cases like slide or t-spin.
%If a certain move is actually impossibile to reach (eg: trapped tetramino) the next move is considered.

getPathOfBestMove(T,Plan) :-
    findMoves(T,ScoreList), 
    member(GoalNode, ScoreList),
    nth1(2, GoalNode, Goal), %take the goal 
    start(Start), 
    serchPath(Goal, Start, RevPlan),
    reverse(RevPlan,Plan),
    !.

serchPath(Start, Goal, Plan) :-
    setof(A, action(A), Actions),
    Heuristic=evaluateMovement, 
    planner(Start, Goal, Actions, Heuristic, Plan).

%%%%%%%%%%%%%%%%%%%%%%%%
%Debugging Helper Rules%
%%%%%%%%%%%%%%%%%%%%%%%%
%write game board for debugging
%call it using: ?- writeGameBoard.

writeGameBoard :-
	writeGameBoard(0,0).

writeGameBoard(R,C) :- 
    gameBoardH(R),
    nl,
    write([--]),
    writeColNumbers(C).

writeGameBoard(R,C) :- 
    nl, 
    writeRowNumber(R),
    writeRow(R,C), 
    R1 is R + 1, 
    writeGameBoard(R1,C),
    !.

writeRowNumber(R):-
    R >= 10,
    write([R]),
    !.

writeRowNumber(R):-
    write('['),
    write(0),
    write(R),
    write(']'),
    !.

writeRow(_,C) :- 
    gameBoardW(C).

writeRow(R,C) :- 
    occCell(R,C), 
    write([■]), 
    C1 is C + 1, 
    writeRow(R,C1),
    !.

writeRow(R,C) :- 
    write([□]),
    C1 is C + 1,
    writeRow(R,C1),
    !.

writeColNumbers(C) :- 
    gameBoardW(C).

writeColNumbers(C) :- 
    write([C]), 
    C1 is C + 1, 
    writeColNumbers(C1),
    !.
%///////////////////////

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%MiniMax                                                                                                                                                                                                                                                                                                                                                     Max/MaxMax move selection%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tetraminos([t,t,t]).


callPlacePiece(Tetraminos,[H|Tl],NextNode):-
    callPlacePiece(Tetraminos,[H|Tl],[],NextNode).

callPlacePiece(Tetraminos,[H|Tl],NextNodesTemp,NextNode):-
    arg(1, H, T),
    arg(2, H, RC),
    arg(1, RC, R),
    arg(2, RC, C),
    placePiece(T,R,C,LGbpre,LGbpost),
    Node = [[Tetraminos,LGbpost,[T,R,C]]],
    retractall(occCell(_,_)),
    assertList(LGbpre),
    append(NextNodesTemp,Node,NextNodesTemp1),
    callPlacePiece(Tetraminos,Tl,NextNodesTemp1,NextNode).

callPlacePiece(_,[],NextNodesTemp,NextNodesTemp).

%Node: [Eval,Tetraminos,Gb,Move]
%Tetraminos: a list of tetraminos
%Gb: a list of the occCell for a certain gameboard configuration
%Move: the move [t,r,c] that allow to obatain the current node, added only when nextNodes is called
%Eval: the evaluation of the node, added only when the heuristc is called
nextNodes(Level,Node, NextNodes) :-
    nth1(1, Node, Tetraminos),
    nth1(2, Node, Gb),
    nth0(Level, Tetraminos, T),
    retractall(occCell(_,_)),
    assertList(Gb),
    findPossibleGoals(T,L),
    callPlacePiece(Tetraminos,L,NextNodes),
    retractall(occCell(_,_)).

evaluateNode(Node, EvaluatedNode) :-
    nth1(2, Node, Gb),
    assertList(Gb),
    gameBoardScore(S),
    retractall(occCell(_,_)),
    append([S],Node,EvaluatedNode).

callMinMax(Player,Move) :-
    tetraminos(T),
    length(T,Depth),
    findall(occCell(R,C),occCell(R,C),OccCell),
    retractList(OccCell),
    StartingNode = [T,OccCell],
    NextNodesGenerator = nextNodes,
    Heuristic = evaluateNode,
    minmax(StartingNode, NextNodesGenerator, Heuristic, 0, Depth, Alpha, Beta, NextMove, Player),
    nth1(4,NextMove,Move),
    assertList(OccCell). %restore starting Gb*/