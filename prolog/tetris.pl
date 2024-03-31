:- module(tetris, [startGbL/1,getPathOfBestMove/2,writeGameBoard/0,placePiece/3,start/1,tetrominoes/1,nextNodes/4,evaluateNode/3,takeMove/2,evaluateMovement/2,checkGoal/2,rotate/2,left/2,right/2,down/2,explanation/3,easyMode/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).
:- use_module(planner).
:- use_module(minmax).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Defining the game board properties and the occupied cells%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%gameboard dimensions
gameBoardW(10).
gameBoardH(20).

%the starting position of the playing tetromino
tetrominoSpawnX(5).
tetrominoSpawnY(1).

:-dynamic(tetrominoes/1).
:-dynamic(startGbL/1).
:-dynamic(explanation/3).
:-dynamic(easyMode/0).

start(T):-
    tetrominoes(TList),
    append(TList,[T],TList2),
    retract(tetrominoes(TList)),
    assert(tetrominoes(TList2)).

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


%Return the list that contain all the occupied cells
getStartGbL(GbL) :-
    startGbL(GbL),
    !.

getStartGbL([]).	

first_items([], []).
first_items([[H|_]|T], [H|T2]) :-
    first_items(T, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Tetrominoes' operators and checks%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%Generate all the free and reachable cell on the gameboard
freeCell1(R,C,GbList) :- 
    gameBoardH(H),
    gen(0, H, R),    
    gameBoardW(W),
    gen(0, W, C),    
    freeCell(R,C,GbList).

%check if there is space for a tetromino expressed in term of 4 cells.
fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),
    freeCell(R4,C4,GbList).

%%%%%%%%%%%%%%%%%%%%%%%
%Tetrominoes Definition%
%%%%%%%%%%%%%%%%%%%%%%%
%Check if there is space for an a tetromino on the gameboard given the reference point.
%R1;C1 is the reference point.

%O Tetromino
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%I Tetromino
%[ ][x][ ][ ]
%[R2;C2][R1;C1][R3;C3][R4;C4]
fitPiece(i1,R1,C1,R2,C2,R3,C3,R4,C4,GbList) :-
    R1 = R2,
    R2 = R3,
    R3 = R4,
    eq(C2,C1,-1),
    eq(C3,C1,+1),
    eq(C4,C1,+2),
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
    
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%S Tetromino
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).


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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Z Tetromino
%S Tetromino
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%T Tetromino
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%J Tetromino
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%L Tetromino
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).

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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Convert a generic tetromino in its starting shape
firstShape(o,o1).
firstShape(i, i1).
firstShape(s, s1).
firstShape(z, z1).
firstShape(l, l1).
firstShape(j, j1).
firstShape(t, t1).

%Mapping each tetromino with its rotations.
%Can be used also to obtain the next rotation of a given tetromino.
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

%Discover all the possible cell where a tetromino can be placed for all its rotations
findPossibleGoals(T,List,GbList) :- 
    findall((T1), rotation(T, T1, _), Lr),
    findPossibleGoals([],Lr,List,GbList).

findPossibleGoals(Found,[],Found,_).

findPossibleGoals(Found,[Ht|T],L,GbList) :-
    findall((Ht,R,C), tetrominoGoal(Ht,R,C,GbList), List),
    append(Found,List,NewList),
    findPossibleGoals(NewList,T,L,GbList).

%a tetromino can be placed on a free cell when the next row collide
tetrominoGoal(Tr,R,C,GbList) :-
    freeCell1(R,C,GbList),
    fitPiece(Tr,R,C,_,_,_,_,_,_,GbList),
    R1 is R + 1,
    \+fitPiece(Tr,R1,C,_,_,_,_,_,_,GbList).

%%%%%%%%%%%%%%%%%%%%%%%%
%GameBoard Manipulation%
%%%%%%%%%%%%%%%%%%%%%%%%
%Put the piece in the given reference point and then assert all the occupied cells.
placePiece(T,R1,C1,LGbpre,LGbpost,NumberOfClearedRows) :-
    fitPiece(T,R1,C1,R2,C2,R3,C3,R4,C4,LGbpre),
    L = [[R1,C1],[R2,C2],[R3,C3],[R4,C4]],
    append(LGbpre,L,LGbpre1),
    computeNewGameBoard(LGbpre1,LGbpost,NumberOfClearedRows). 

%remove all the rows in the list indexed by rownumber
removeRows([],GbListNew,GbListNew).

removeRows([R|T],GbListOld,GbListNew) :-
    delete(GbListOld,[R,_],GbListOld1),
    removeRows(T,GbListOld1,GbListNew).

%Compute the new gameboard after the tetromino placing
%If one or more rows are full perform removing and row shifting.
%Called by fronted is playert perform an action
placePiece(T,R1,C1) :-
    getStartGbL(GbListOld),
    placePiece(T,R1,C1,GbListOld,GbListNew,_),
    retractall(startGbL(_)),
    asserta(startGbL(GbListNew)).

computeNewGameBoard(GbListOld,GbListNew,NumberOfClearedRows) :-
    findall((R),clearedRow(R,GbListOld),ClearedRows),
    removeRows(ClearedRows,GbListOld,GbListMid),
    length(ClearedRows,NumberOfClearedRows),
    shift(ClearedRows,GbListMid,GbListNew),
    !.

computeNewGameBoard(GbListOld,GbListOld,0).

%row shifter for computing new gameBoard
shift([H|T],GbListOld,GbListNew):-
    findall(([R,C]), cellToShift(R, C, H, GbListOld), ElementToShift),
    shiftOccCell(1, ElementToShift, ShiftedElements),
    first_items(ElementToShift,RowsToShift),
    sort(RowsToShift,SortedRowsToShift), 
    removeRows(SortedRowsToShift,GbListOld,GbListOld1), 
    append(ShiftedElements,GbListOld1,GbListOld2),
    shift(T,GbListOld2,GbListNew).

shift([],GbListNew,GbListNew).

cellToShift(R,C,R1,GbList) :-
    occCellLG(R,C,GbList),
    R<R1.

shiftOccCell(_, [], []).

shiftOccCell(N, [[R,C]|T], [[R1,C]|T2]) :-
    R1 is R + N,
    shiftOccCell(N, T, T2).

%Return the row number of the cleared row
%here bagof is used because we want a result for each row
countOccCelInRow(R,N, GbList) :- 
    bagof(([C]),occCellLG(R,C,GbList),Occ),
    length(Occ,N).

clearedRow(R, GbList) :-
    countOccCelInRow(R,Occ, GbList),
    gameBoardW(W),
    Occ = W.

%%%%%%%%%%%%%%%%%%%%%
%GameBoard Evaluator%
%%%%%%%%%%%%%%%%%%%%%
%count the occupied cell for a certain column
countOccCelInColumn(C,N,GbList) :- 
    findall((R),occCellLG(R,C,GbList),Occ),length(Occ,N).

topOccCelInColumn(GbList,RT) :- 
    gameBoardW(W),
    gen(0, W, C), 
    findall((R),occCellLG(R,C,GbList),Rows),
    sort(Rows,[RT|_]).

occCellForColumn(GbList,O4C) :-
    gameBoardW(W),
    findall((N),countOccCelInColumn(_,N, GbList),O4C1),
    length(O4C1,N),
    W1 is W - N,
    build(0, W1, ListOfZeros),
    append(O4C1,ListOfZeros,O4C).


build(X, N, List)  :- 
    length(List, N), 
    maplist(=(X), List).

sum(X, Y, Z) :- 
    Z is X + Y.

heightForColumn(GbList,H4C) :-
    gameBoardH(H),
    gameBoardW(W),
    findall((HC),(topOccCelInColumn(GbList,TC),HC is H - TC),H4C1),
    length(H4C1,N),
    W1 is W - N,
    build(0, W1, ListOfZeros),
    append(H4C1,ListOfZeros,H4C).

subForHoles(X, Y, Z) :- 
    Z is X - Y.

totalHoles(L1, L2, R) :-
    maplist(subForHoles, L1, L2, R1),
    sumlist(R1,R).

deleteLast(X,Y):-
    reverse(X,[_|X1]), reverse(X1,Y).

deleteFirst([_|Y],Y).

subForBump(X, Y, Z) :-
    Z is abs(X-Y).

bumpiness(L1, L2, R) :-
    deleteFirst(L1, L12),
    deleteLast(L2,L22),
    maplist(subForBump, L12, L22, R1),
    sumlist(R1,R).

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

sumEntropyOfGameBoard(SumEnt,GbL) :-
    findall((Ent),entropyOfRow(_,Ent,GbL),EntropyXRow),
    sum_list(EntropyXRow,SumEnt).

gameBoardScore(GbList,AggregateHeight,RowsCleared,Holes,Bumpiness,SumEnt,Score) :-
    occCellForColumn(GbList,O4C),
    heightForColumn(GbList,H4C),
    %AggregateHeight
    sumlist(H4C,AggregateHeight),
    %Holes
    totalHoles(H4C, O4C, Holes),
    %Bumpiness
    bumpiness(H4C, H4C, Bumpiness),
    %Score
    E is -10,
    sumEntropyOfGameBoard(SumEnt,GbList),
    Score is E*SumEnt - AggregateHeight - Holes - Bumpiness + RowsCleared.

gameBoardScore('max',GbList,AggregateHeight,RowsCleared,Holes,Bumpiness,SumEnt,Score) :-
    RowsCleared > 0,
    gameBoardScore(GbList,AggregateHeight,0,Holes,Bumpiness,SumEnt,Score1),
    Score is -1*1000*RowsCleared+Score1,
    !.

gameBoardScore('min',GbList,AggregateHeight,RowsCleared,Holes,Bumpiness,SumEnt,Score) :-
    RowsCleared > 0,
    gameBoardScore(GbList,AggregateHeight,0,Holes,Bumpiness,SumEnt,Score1),
    Score is 1000*RowsCleared+Score1,
    !.

gameBoardScore(_,GbList,AggregateHeight,RowsCleared,Holes,Bumpiness,SumEnt,Score) :-
    gameBoardScore(GbList,AggregateHeight,RowsCleared,Holes,Bumpiness,SumEnt,Score).

%%%%%%%%%
%Planner%
%%%%%%%%%
%Define actions
action(rotate).
action(right).
action(left).
action(down).

%listing the actual transformation involved by the moves
rotate((T1,R1,C1,GbL),(T2,R1,C1,GbL)) :-
    rotation(_,T2,T1),
    fitPiece(T2,R1,C1,_,_,_,_,_,_,GbL).

left((T1,R1,C1,GbL),(T1,R1,C2,GbL)) :- 
    C2 is C1+1,
    fitPiece(T1,R1,C2,_,_,_,_,_,_,GbL).

right((T1,R1,C1,GbL),(T1,R1,C2,GbL)) :- 
    C2 is C1-1,
    fitPiece(T1,R1,C2,_,_,_,_,_,_,GbL).
    
down((T1,R1,C1,GbL),(T1,R2,C1,GbL)) :- 
    R2 is R1 - 1,
    fitPiece(T1,R2,C1,_,_,_,_,_,_,GbL).
%

%evaluateMovement compute a score for each move given the goal, the score is weighted by the priority of the move.
%the priority is: rotate, [left, right], down.
%the ratio is: 
%   align it to the point she wants to reach
%   rotate a tetromino in the proper rotation
%   then push down the tetromino in that point
%this sequence of operation is the most common, so priority are shaped around this scenario, however it can deal also with more complex situations.

evaluateMovement([rotate, (T1,_,_,_), (T2,_,_,_)], Score) :-
    diff(T1,T2,Diff),
	priority(rotate,Priority),
	Score is  Diff * Priority.

evaluateMovement([left, (_,_,C1,_), (_,_,C2,_)], Score) :-
	priority(left,Priority),
	Score is (C2 - C1) * Priority.
	
evaluateMovement([right, (_,_,C1,_), (_,_,C2,_)], Score) :-
	priority(right,Priority),
	Score is (C2 - C1) * Priority * -1.

evaluateMovement([down, (_,R1,_,_), (_,R2,_,_)], Score) :-
	priority(down,Priority),
	Score is (R2 - R1) * Priority * -1.

priority(left,10).
priority(right,10).
priority(down,1).
priority(rotate,1).

diff(T1,T2,Diff) :-
    T1 == T2,
    Diff is 0,
    !.

diff(_,_,Diff) :-
    Diff is 1,
    !.

checkGoal(Node,Node).

serchPath(Start, Goal, Plan, PlanStory) :-
    findall(A, action(A), Actions),
    Heuristic=evaluateMovement, 
    GoalChecker=checkGoal,
    planner(Start, Goal, Actions, Heuristic, Plan, PlanStory, GoalChecker).

%%%%%%%%%%%%%%%
%MaxMax/MinMax%                                                                                                                                                                                                                                                                                                                                                   Max/MaxMax move selection%
%%%%%%%%%%%%%%%
checkElegibility(Tn,R,C,GbList):-
    rotation(T, Tn, _),
    firstShape(T,T1),
    tetrominoSpawnX(X),
    tetrominoSpawnY(Y),
    Start = (T1,Y,X,GbList),  
    placePiece(T1,Y,X,GbList,_,_),  
    Goal = (Tn,R,C,GbList),
    serchPath(Goal, Start, _, _).

callPlacePiece(_,_,[],[]).

callPlacePiece(Tetrominoes,GbList,[(T,R,C)|Taill],[[Tetrominoes,GbListPost,ClRow,(T,R,C)]|Tail2]):-
    catch(call_with_time_limit(0.5, checkElegibility(T,R,C,GbList)),time_limit_exceeded,fail),
    placePiece(T,R,C,GbList,GbListPost,ClRow),
    callPlacePiece(Tetrominoes,GbList,Taill,Tail2),
    !.

callPlacePiece(Tetrominoes,GbList,[_|Taill],Tail2):-
    callPlacePiece(Tetrominoes,GbList,Taill,Tail2).

%Node: [Eval,Tetrominoes,GbL,(AggHeight,ClRow,Holes,Bump,Ent), Move]
%Tetrominoes: a list of tetrominoes
%Gb: a list of the occCell for a certain gameboard configuration
%Move: the move [t,r,c] that allow to obatain the current node, added only when nextNodes is called
%Eval: the evaluation of the node, added only when the heuristc is called, nextNodes will not see this one
%(AggHeight,ClRow,Holes,Bump,Ent): the compunded term containing the information from which the score derived

%do not generate any other move if checking a row-clear move in vs mode.
%it's like "win" from the AI perspective.

nextNodes1(_,Tetrominoes,GbL,T, _, NextNodes) :-
    findPossibleGoals(T,L,GbL),
    callPlacePiece(Tetrominoes,GbL,L,NextNodes).

%easy mode: every row-clearing move stops the branch exploration
nextNodes(Player,_,Node,[]) :-
    easyMode,
    Player \= 'maxmax',
    nth1(3, Node, ClRow),
    ClRow > 0.

%hard mode: only the opponents row-clearing move stops the branch exploration, 
%the AI player will continue the branch exploration until the end.
nextNodes(Player,_,Node,[]) :-
    \+easyMode,
    Player = 'max',
    nth1(3, Node, ClRow),
    ClRow > 0.

nextNodes(Player,Level,Node,NextNodes) :-
    nth1(1, Node, Tetrominoes),
    nth1(2, Node, GbL),
    nth0(Level, Tetrominoes, T),
    nth1(3, Node, ClRow),
    nextNodes1(Player,Tetrominoes,GbL,T,ClRow, NextNodes).

evaluateNode(Player,[Tetrominoes,GbL,ClRow,(T,R,C)], [S,Tetrominoes,GbL,(AggHeight,ClRow,Holes,Bump,Ent),(T,R,C)]) :-
    gameBoardScore(Player,GbL,AggHeight,ClRow,Holes,Bump,Ent,S),
    !.

%allows to remember the move chain
%base case: when we collect the value from a leaf
takeMove(Node,[Move,ScoreComponent]):-
    length(Node,5),
    nth1(4,Node,ScoreComponent),
    nth1(5,Node,Move).

%recursive case: when we collect the value from non-leaf node
takeMove(Node,[Move|CollectedMoves]):-
    nth1(2,Node,CollectedMoves),
    nth1(6,Node,Move).

callMinMax(GbL, Player, BestNode) :-
    tetrominoes(T),
    length(T,Depth),
    StartingNode = [T,GbL,0],
    NextNodesGenerator = nextNodes,
    Heuristic = evaluateNode,
    MoveTaker = takeMove,
    minmax(StartingNode, NextNodesGenerator, Heuristic, MoveTaker, 0, Depth, -inf, +inf, BestNode, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Main predicate: GetPathOfBestMove%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%getPathOfBestMove search for the best move and then call the planner for the tetris path problem.
%start and goal are inverted because I want to find the path starting from the goal and coming back to the start.
%This allow to deal easily with particular cases like slide or t-spin.

getPathOfBestMove(Player,Plan) :-
    getStartGbL(GbL),
    tetrominoes([T|_]),
    firstShape(T,T1),
    tetrominoSpawnX(X),
    tetrominoSpawnY(Y),!,
    placePiece(T1,Y,X,GbL,_,_),  %avoid to start the whole algorithm if the piece cannot be placed, gameover condition if failed
    callMinMax(GbL,Player,BestNode),
    Start = (T1,Y,X,GbL),
    last(BestNode,(Tg,Rg,Cg)),    
    Goal = (Tg,Rg,Cg,GbL),
    serchPath(Goal, Start, RevPlan, PathStory),
    reverse(RevPlan,Plan),
    assertGbL(BestNode),
    assertExplanation(BestNode,Tg,Rg,Cg,PathStory),
    !.

%we need two version of  assertGbL and assertExplanation because it is possible that the node story does not exist.
%this happens if the min max find a win move just after one step.
assertGbL(BestNode) :-
    tetrominoes(T),
    nth1(2,BestNode,NodeStory),
    T \= NodeStory,
    nth1(4,BestNode,NextGbL), 
    retractall(startGbL(_)),
    asserta(startGbL(NextGbL)).

assertGbL(BestNode) :-
    tetrominoes(T),
    nth1(2,BestNode,NotNodeStory),
    T == NotNodeStory,
    nth1(3,BestNode,NextGbL), 
    retractall(startGbL(_)),
    asserta(startGbL(NextGbL)).

assertExplanation(BestNode,Tg,Rg,Cg,PathStory) :-
    tetrominoes(T),
    nth1(2,BestNode,NodeStory), 
    T \= NodeStory,
    last(NodeStory,ScoreComponent),
    append([(Tg,Rg,Cg)],NodeStory,FullNodeStory),
    retractall(explanation),
    asserta(explanation(FullNodeStory,PathStory,ScoreComponent)),
    !.

assertExplanation(BestNode,Tg,Rg,Cg,PathStory) :-
    tetrominoes(T),
    nth1(2,BestNode,NotNodeStory), 
    T = NotNodeStory,
    nth1(4,BestNode,ScoreComponent),
    retractall(explanation),
    asserta(explanation([(Tg,Rg,Cg)],PathStory,ScoreComponent)),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%
%Debugging Helper Rules%
%%%%%%%%%%%%%%%%%%%%%%%%
%write game board for debugging
%call it using: ?- writeGameBoard.

getTetrominoes(Ts):-
    tetrominoes(Ts),
    !.

getTetrominoes([]).

writeGameBoard :-
    getStartGbL(GbL),
	writeGameBoard(0,0,GbL),
    nl,
    getTetrominoes(Ts),
    write('tetrominoes: '),
    write(Ts).

writeGameBoard(R,C,_) :- 
    gameBoardH(R),
    nl,
    write([--]),
    writeColNumbers(C).

writeGameBoard(R,C,GbL) :- 
    nl, 
    writeRowNumber(R),
    writeRow(R,C,GbL), 
    R1 is R + 1, 
    writeGameBoard(R1,C,GbL),
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

writeRow(_,C,_) :- 
    gameBoardW(C).

writeRow(R,C,GbL) :- 
    occCellL(R,C,GbL), 
    write([■]), 
    C1 is C + 1, 
    writeRow(R,C1,GbL),
    !.

writeRow(R,C,GbL) :- 
    write([□]),
    C1 is C + 1,
    writeRow(R,C1,GbL),
    !.

writeColNumbers(C) :- 
    gameBoardW(C).

writeColNumbers(C) :- 
    write([C]), 
    C1 is C + 1, 
    writeColNumbers(C1),
    !.

%%%%%%%%%%%%%%%%%%%
%Simulation Runner%
%%%%%%%%%%%%%%%%%%%
%allows to run a solo mode simulation to estimate the maximum number of cleared row
trmns([o,i,j,l,t,s,z]).

takeClearedRows(BestNode,ClearedRows) :-
    length(BestNode,6),
    nth1(5,BestNode,ClearedRows).

 takeClearedRows(BestNode,ClearedRows) :-
    length(BestNode,5),
    nth1(4,BestNode,HeuristicInfo),
    nth1(2,HeuristicInfo,ClearedRows).

getPathOfBestMove2(Player,ClearedRows) :-
    getStartGbL(GbL),
    tetrominoes([T|_]),
    firstShape(T,T1),
    tetrominoSpawnX(X),
    tetrominoSpawnY(Y),!,
    placePiece(T1,Y,X,GbL,_,_),  %avoid to start the whole algorithm if the piece cannot be placed, gameover condition if failed
    callMinMax(GbL,Player,BestNode),
    takeClearedRows(BestNode,ClearedRows),
    write(BestNode),nl,
    assertGbL(BestNode),
    !.

runSimulation(ClearedRows) :-
    retractall(startGbL(_)),
    asserta(startGbL([])),
    retractall(tetrominoes(_)),
    assertz(tetrominoes([])),
    random(0, 7, N),
    trmns(L),
    nth0(N, L, T),
    start(T),
    runSimulation1(0,ClearedRows),
    write(ClearedRows). 

runSimulation1(ClearedRows,TotClearedRows) :-
    write(ClearedRows),nl,
    random(0, 7, N),
    trmns(L),
    nth0(N, L, T2),
    start(T2),
    %writeGameBoard,
    getPathOfBestMove2('maxmax',TempClearedRows),
    %write('ok'),nl,
    tetrominoes([_,T2]),
    retractall(tetrominoes(_)),
    assertz(tetrominoes([T2])),
    %get_char(_X),
    ClearedRows1 is TempClearedRows + ClearedRows,
    runSimulation1(ClearedRows1, TotClearedRows).

runSimulation1(ClearedRows,ClearedRows).