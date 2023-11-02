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
/*
:- use_module(library(http/http_server)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Web Server Interface Config%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
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
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Defining the game board properties and the occupied cells%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gameBoardW(10).
gameBoardH(20).
:-dynamic(occCell/2).
%the starting position of the playing tetramino
%tetramino(T,R,C).
:-dynamic(start/1).

/*
occCell(R,C) :-
    http_session_data(occCell(R,C)),!.
*/

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
%

%logarithm in base 2
logBase2(0,0) :- !.

logBase2(X,R) :-
    N is log10(X),
    D is log10(2),
    R is N/D.
%

%auxiliary number generator used by freeCell1
gen(Cur, Top, Cur):- Cur < Top.

gen(Cur, Top, Next):-
  Cur < Top,
  Cur1 is Cur+1,
  gen(Cur1, Top, Next).
%

%Generate the list that contain all the occupied cells
createGameBoardList(GbList) :-
    setof(([R,C]),occCell(R,C),GbList).
	
%

%remove all the rows in the firt list indexed by rownumber
removeRows([],GbListNew,GbListNew).

removeRows([R|T],GbListOld,GbListNew) :-
    delete(GbListOld,[R,_],GbListOld1),
    removeRows(T,GbListOld1,GbListNew).

first_items([], []).
first_items([[H|_]|T], [H|T2]) :-
    first_items(T, T2).

second_items([], []).
second_items([[H|_]|T], [H|T2]) :-
    second_items(T, T2).
  
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

%Check if a cell is occupied in the list
occCellL(R,C,GbList) :-
    memberchk([R,C],GbList).

%Check/Generate cell  occupied in the list
occCellLG(R,C,GbList) :-
    member([R,C],GbList).

%Check if a cell is avalaible and if the indexes are not out of bound.
freeCell(R,C,GbList) :- 
    gameBoardH(H), 
    gameBoardW(W), 
    R >= 0, R<H, 
    C >=0, C<W, 
    \+occCellL(R,C,GbList).

%Generate all the free cell on the gameboard
freeCell1(R,C,GbList) :- 
    gameBoardH(H),
    gen(0, H, R),    
    gameBoardW(W),
    gen(0, W, C),    
    \+occCellL(R,C,GbList).

%Check if there is space for an a tetromino on the gameboard given the reference point.
%R1;C1 is the reference point.
%O Tetramino
%[ ][ ]
%[ ][x]
%[R4;C4][R2;C2]
%[R3;C3][R1;C1]

fitPiece(o1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C2 = C1,    
    R3 = R1,    
    R4 = R2,
    C4 = C3,
    eq(C3,C1,-1),
    eq(R2,R1,-1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),
    freeCell(R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%I Tetramino
%[ ][x][ ][ ]
%[R2;C2][R1;C1][R3;C3][R4;C4]
fitPiece(i1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :-
    R1 = R2,
    R2 = R3,
    R3 = R4,
    eq(C2,C1,-1),
    eq(C3,C1,+1),
    eq(C4,C1,+2),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).
    
%[ ]
%[x]
%[ ]
%[ ]
%[R2;C2]
%[R1;C1]
%[R3;C3]
%[R4;C4]
fitPiece(i2,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C2,
	C2 = C3,
	C3 = C4,
	eq(R2,R1,-1),
	eq(R3,R1,+1),
	eq(R4,R1,+2),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%S Tetramino
%   [ ][ ]
%[ ][x]
%       [R3;C3][R4;C4]
%[R2;C2][R1;C1]

fitPiece(s1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C3,
    R1 = R2,
    R4 = R3,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C3,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).


%[ ]
%[ ][x]
%   [ ]
%[R4;C4]
%[R3;C3][R1;C1]
%       [R2;C2]

fitPiece(s2,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C2,
    R1 = R3,
    C3 = C4,
	eq(R2,R1,+1),
    eq(R4,R3,-1),
    eq(C3,C1,-1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Z Tetramino
%S Tetramino
%[ ][ ]
%   [x][ ]
%[R4;C4][R3;C3]
%       [R1;C1][R2;C2]
fitPiece(z1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C3,
    R1 = R2,
    R4 = R3,
    eq(C2,C1,+1),
    eq(R3,R1,-1),
    eq(C4,C3,-1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%   [ ]
%[ ][x]
%[ ]
%       [R4;C4]
%[R3;C3][R1;C1]
%[R2;C2]

fitPiece(z2,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C4,
    R1 = R3,
    C3 = C2,
	eq(R2,R1,+1),
    eq(R4,R1,-1),
    eq(C3,C1,-1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%T Tetramino
%   [ ]
%[ ][x][ ]
%       [R3;C3]
%[R2;C2][R1;C1][R4;C4]

fitPiece(t1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C3,
    R1 = R2,
    R1 = R4,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%   [ ]
%   [x][ ]
%   [ ]
%   [R3;C3]
%   [R1;C1][R4;C4]
%   [R2;C2]
fitPiece(t2,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C3,
    R1 = R4,
    C1 = C2,
	eq(R2,R1,+1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%   
%[ ][x][ ]
%   [ ]
%   
%[R3;C3][R1;C1][R4;C4]
%       [R2;C2]
fitPiece(t3,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    R1 = R3,
    R1 = R4,
    C1 = C2,
	eq(R2,R1,+1),
    eq(C3,C1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%   [ ]
%[ ][x]
%   [ ]
%       [R4;C4]
%[R3;C3][R1;C1]
%       [R2;C2]
fitPiece(t4,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    R1 = R3,
    C1 = C2,
    C1 = C4,
	eq(R2,R1,+1),
    eq(C3,C1,-1),
    eq(R4,R1,-1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%J Tetramino
%[ ]
%[ ][x][ ]
%[R3;C3]
%[R2;C2][R1;C1][R4;C4]

fitPiece(j1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    R1 = R2,
    R1 = R4,
    C2 = C3,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%   [ ][ ]
%   [x]
%   [ ]
%   [R2;C2][R3;C3]
%   [R1;C1]
%   [R4;C4]

fitPiece(j2,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C1 = C2,
    C1 = C4,
    R3 = R2,
	eq(R2,R1,-1),
    eq(R4,R1,+1),
    eq(C3,C2,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%
%[ ][x][ ]
%      [ ]
%[R2;C2][R1;C1][R3;C3]
%              [R4;C4]

fitPiece(j3,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    R2 = R1,
    R3 = R1,
    C3 = C4,
	eq(C2,C1,-1),
    eq(C3,C1,+1),
    eq(R4,R3,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%   [ ]
%   [x]
%[ ][ ]
%       [R2;C2]
%       [R1;C1]
%[R4;C4][R3;C3]       

fitPiece(j4,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C2 = C1,
    C3 = C1,
    R4 = R3,
	eq(R2,R1,-1),
    eq(R3,R1,+1),
    eq(C4,C3,-1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%L Tetramino
%      [ ]
%[ ][x][ ]
%              [R3;C3]
%[R2;C2][R1;C1][R4;C4]

fitPiece(l1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    R1 = R2,
    R1 = R4,
    C4 = C3,
	eq(C2,C1,-1),
    eq(R3,R1,-1),
    eq(C4,C1,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%   [ ]
%   [x]
%   [ ][ ]
%   [R2;C2]
%   [R1;C1]
%   [R3;C3][R4;C4]       

fitPiece(l2,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C2 = C1,
    C3 = C1,
    R4 = R3,
	eq(R2,R1,-1),
    eq(R3,R1,+1),
    eq(C4,C3,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%
%[ ][x][ ]
%[ ]              
%[R2;C2][R1;C1][R4;C4]
%[R3;C3]

fitPiece(l3,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    R1 = R2,
    R1 = R4,
    C2 = C3,
	eq(C2,C1,-1),
    eq(R3,R2,+1),
    eq(C4,C1,+1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%[ ][ ]
%   [x]
%   [ ]
%[R4;C4][R2;C2]
%       [R1;C1]
%       [R3;C3]       

fitPiece(l4,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    C2 = C1,
    C3 = C1,
    R4 = R2,
	eq(R2,R1,-1),
    eq(R3,R1,+1),
    eq(C4,C2,-1),
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),	
    freeCell(R4,C4,GbList).

%Discover all the possible cell where a tetramino can be placed for all its rotations
findPossibleGoals(T,List,GbList) :- 
    findall((T1), rotation(T, T1, _), Lr),
    findPossibleGoals0([],Lr,List,GbList).

findPossibleGoals0(Found,[],Found,_).

findPossibleGoals0(Found,[Ht|T],L,GbList) :-
    bagof((Ht,R,C), tetraminoGoal(Ht,R,C,GbList), List),
    append(Found,List,NewList),
    findPossibleGoals0(NewList,T,L,GbList).

%a tetramino can be placed on a free cell when the next row collide
tetraminoGoal(T,R,C,GbList) :-
    freeCell1(R,C,GbList),
    fitPiece(T,R,C,_,_,_,_,_,_,GbList),
    R1 is R + 1,
    \+fitPiece(T,R1,C,_,_,_,_,_,_,GbList).

%row shifter for computing new gameBoard

cellToShift(R,C,R1,GbList) :-
    occCellLG(R,C,GbList),
    R<R1.

shiftOccCell(_, [], []).

shiftOccCell(N, [[R,C]|T], [[R1,C]|T2]) :-
    R1 is R + N,
    shiftOccCell(N, T, T2).

shift([H|T],GbListOld,GbListNew):-
    bagof(([R,C]), cellToShift(R, C, H, GbListOld), ElementToShift),
    shiftOccCell(1, ElementToShift, ShiftedElements),
    first_items(ElementToShift,RowsToShift),
    sort(RowsToShift,SortedRowsToShift), 
    removeRows(SortedRowsToShift,GbListOld,GbListOld1), 
    append(ShiftedElements,GbListOld1,GbListOld2),
    shift(T,GbListOld2,GbListNew).

shift([],GbListNew,GbListNew).

%Return the row number of the cleared row
countOccCelInRow(R,N, GbList) :- 
    setof((C),occCellLG(R,C,GbList),Occ),length(Occ,N).

clearedRow(R, GbList) :-
    countOccCelInRow(R,Occ, GbList),
    gameBoardW(W),
    Occ = W.
%
computeNewGameBoard(GbListOld,GbListNew) :-
    setof((R),clearedRow(R,GbListOld),ClearedRows),
    removeRows(ClearedRows,GbListOld,GbListMid),
    shift(ClearedRows,GbListMid,GbListNew),
    !.

computeNewGameBoard(GbListOld,GbListOld).

%Put the piece in the given reference point and then assert all the occupied cells.
placePiece(T,R1,C1,LGbpre,LGbpost) :-
    fitPiece(T,R1,C1,R2,C2,R3,C3,R4,C4,LGbpre),
    L = [[R1,C1],[R2,C2],[R3,C3],[R4,C4]],
    append(LGbpre,L,LGbpre1),
    computeNewGameBoard(LGbpre1,LGbpost).
%

%%%%%%%%%%%%%%%%%%  


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

%///////////////////

%%%%%%%%%%%%%%%%%%%%%
%GameBoard Evaluator%
%%%%%%%%%%%%%%%%%%%%%

%1) Compute the number of holes in the columns.
%This algorithm consider as holes all the empty cell below an occupied cell of a certain column.
%The holes are often the result of a bad move, so the goal is to choose a move that minimize this number.
holesInColumn(N,GbL) :- 
    setof((R,C,GbL),holesColumn(R,C,GbL),HolesColumn),
    length(HolesColumn,N),
    !.

holesInColumn(0,_).

holesColumn(R1,C,GbL) :-
    gameBoardW(W),
    gen(0, W, C),
    occCellL(R,C,GbL),
    freeCell1(R1,C,GbL),
    R1 > R.

%

nonContinuosSpaceInRows(N,GbL) :-
    setof((R,C,GbL),nonContinuosSpaceInRows(R,C,GbL),NCSIR),
    length(NCSIR,N),
    !.

nonContinuosSpaceInRows(0,_).

nonContinuosSpaceInRows(R,C,GbL) :-
    gameBoardH(H),
    gen(0, H, R),
    freeCell1(R,C,GbL),
    occCellL(R,C1,GbL),
    (C1 > C; C1 < C).

%

%3) Compute the etropy of each row, then give average value as measure of the "entropy" of the gameboard.
%Count the occupied cell of a row.

%Compute the entropy of a row.
entropyOfRow(R,Ent,GbL) :-
    countOccCelInRow(R,Occ,GbL),
    gameBoardW(W),
    Free is W - Occ,
    OccRatio is Occ/W,
    FreeRatio is Free/W,
    logBase2(OccRatio,Log2OccRatio),
    logBase2(FreeRatio,Log2FreeRatio),
    Ent is -OccRatio*Log2OccRatio -FreeRatio*Log2FreeRatio.

entropyOfRow(_,0,_).

avgEntropyOfGameBoard(AvgEnt,GbL) :-
    findall((Ent),entropyOfRow(_,Ent,GbL),EntropyXRow), 
    sum_list(EntropyXRow,Sum),
    AvgEnt is Sum.
%

%Compute the whole GameBoard score
gameBoardScore(S,GbL) :-
    holesInColumn(HC,GbL),
    nonContinuosSpaceInRows(NCSIR,GbL),
    avgEntropyOfGameBoard(AvgEnt,GbL),
    S1 is AvgEnt*10+HC+NCSIR,
    S is -1*S1.  

%Compute the evalution for each available move and put them in an array
findMoves(T,ScoreList,GbL) :-
    findPossibleGoals(T,L,GbL),
    write(L),
    scoreMoves(L,ScoreListUnsort,GbL),
    sort(1, @>=, ScoreListUnsort, ScoreList).

scoreMoves([],[],_).

scoreMoves([(T,R,C)|Tail],[[S,(T,R,C)]|Tail2],GbLPre) :-
    placePiece(T,R,C,GbLPre,GbLPost),
    gameBoardScore(S,GbLPost),
    scoreMoves(Tail,Tail2,GbLPre).  

/*
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


%///////////////////////
/*
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
    assertList(OccCell). %restore starting Gb
    */