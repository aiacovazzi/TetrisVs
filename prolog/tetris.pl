:- module(tetris, [getPathOfBestMove/2,writeGameBoard/0,computeNewGameBoard/0,occCell/2,callMinMax/2,start/1,tetraminos/1,nextNodes/3,evaluateNode/2,evaluateMovement/2,rotate/2,left/2,right/2,down/2]).
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
To do (5/11):

-test AI SOLO e VS + adeguamento tetrisJs
-debug computeNewGameBoard?
-ottimizzare (o cambiare) euristica + algoritmo genetico per pesi euristica di valutazione??

Problemi noti:
-il lookahead potrebbe generare mosse irragiungibili.
Soluzioni:
    1)prevalutazione mossa usando il pathfinder (lento).
    2)restituire in output non la mossa migliore ma la lista delle mosse di tutti i nodi successori alla radice ordinati.
      in questo modo eventuli mosse irragiungibili possono essere "segnalate" dal pathfinder e scartate, per passare alla successiva.
      mi aspetto non sia un caso frequente e di basso impatto sulle perfomance.

Bonus:
-impossible tetris
-implementare il logger che spieghi le mosse
-implementazione delle sessioni
*/
:- use_module(library(lists)).
:- use_module(planner).
:- use_module(minmax).
%
%//////////////////////////////////////////////////////////
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Defining the game board properties and the occupied cells%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gameBoardW(10).
gameBoardH(20).
:-dynamic(occCell/2).
%the starting position of the playing tetramino

:-dynamic(tetraminos/1).

start(T):-
tetraminos(TList),
append(TList,[T],TList2),
retract(tetraminos(TList)),
assert(tetraminos(TList2)).

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
    setof(([R,C]),occCell(R,C),GbList),!.

createGameBoardList([]).	
%

%remove all the rows in the list indexed by rownumber
removeRows([],GbListNew,GbListNew).

removeRows([R|T],GbListOld,GbListNew) :-
    delete(GbListOld,[R,_],GbListOld1),
    removeRows(T,GbListOld1,GbListNew).

first_items([], []).
first_items([[H|_]|T], [H|T2]) :-
    first_items(T, T2).
  
%/////////////////////////////////

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Tetraminos' operators and checks%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Convert a generic tetramino in its starting shape
firstShape(o,o1).
firstShape(i, i1).
firstShape(s, s1).
firstShape(z, z1).
firstShape(l, l1).
firstShape(j, j1).
firstShape(t, t1).

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

%check if there is space for a tetramin expressed in term of 4 cells.
fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList) :- 
    freeCell(R1,C1,GbList),
    freeCell(R2,C2,GbList),
    freeCell(R3,C3,GbList),
    freeCell(R4,C4,GbList).

%%%%%%%%%%%%%%%%%%%%%%%
%Tetraminos Definition%
%%%%%%%%%%%%%%%%%%%%%%%
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
    fitPiece(R1,C1,R2,C2,R3,C3,R4,C4,GbList).
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

%%%%%%%%%%%%%%%%%%%%%%%%
%GameBoard Manipulation%
%%%%%%%%%%%%%%%%%%%%%%%%
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

%Put the piece in the given reference point and then assert all the occupied cells.
placePiece(T,R1,C1,LGbpre,LGbpost) :-
    fitPiece(T,R1,C1,R2,C2,R3,C3,R4,C4,LGbpre),
    L = [[R1,C1],[R2,C2],[R3,C3],[R4,C4]],
    append(LGbpre,L,LGbpre1),
    computeNewGameBoard(LGbpre1,LGbpost).
%   

%Compute the new gameboard after the tetramino placing
%If one or more rows are full perform removing and row shifting.
computeNewGameBoard :-
    createGameBoardList(GbListOld),
    computeNewGameBoard(GbListOld,GbListNew),
    retractall(occCell(_,_)),
    assertList(GbListNew).

computeNewGameBoard. %avoid fail if called when Gb is empty

computeNewGameBoard(GbListOld,GbListNew) :-
    setof((R),clearedRow(R,GbListOld),ClearedRows),
    removeRows(ClearedRows,GbListOld,GbListMid),
    shift(ClearedRows,GbListMid,GbListNew),
    !.

computeNewGameBoard(GbListOld,GbListOld).

%row shifter for computing new gameBoard
shift([H|T],GbListOld,GbListNew):-
    bagof(([R,C]), cellToShift(R, C, H, GbListOld), ElementToShift),
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
countOccCelInRow(R,N, GbList) :- 
    setof((C),occCellLG(R,C,GbList),Occ),length(Occ,N).

clearedRow(R, GbList) :-
    countOccCelInRow(R,Occ, GbList),
    gameBoardW(W),
    Occ = W.
%

assertList([]).

assertList([[R,C]|T]) :-
    assertz(occCell(R,C)),
    assertList(T).
%%%%%%%%%%%%%%%%%%  

%%%%%%%%%%%%%%%%%%%%%
%GameBoard Evaluator%
%%%%%%%%%%%%%%%%%%%%%
%1) Compute the number of holes in the columns.
%This algorithm consider as holes all the empty cell below an occupied cell of a certain column.
%The holes are often the result of a bad move, so the goal is to choose a move that minimize this number.
holesInColumn(N,GbL) :- 
    setof((R,C),holesColumn(R,C,GbL),HolesColumn),
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
    setof((R,C),nonContinuosSpaceInRows(R,C,GbL),NCSIR),
    length(NCSIR,N),
    !.

nonContinuosSpaceInRows(0,_) :-!.

nonContinuosSpaceInRows(R,C,GbL) :-
    gameBoardW(W),
    gen(0, W, C1),
    gen(C1, W, C2),
    gen(C1, C2, C),
    freeCell1(R,C,GbL),
    occCellL(R,C1,GbL),
    occCellL(R,C2,GbL).
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

sumEntropyOfGameBoard(SumEnt,GbL) :-
    findall((Ent),entropyOfRow(_,Ent,GbL),EntropyXRow),
    sum_list(EntropyXRow,Sum),
    SumEnt is Sum.
%

%Compute the whole GameBoard score
gameBoardScore(S,GbL,HC,NCSIR,SumEnt) :-
    %holesInColumn(HC,GbL),
    HC is 0,
    %nonContinuosSpaceInRows(NCSIR,GbL),
    NCSIR is 0,
    sumEntropyOfGameBoard(SumEnt,GbL),
    S1 is SumEnt*10+HC+NCSIR,
    S is -1*S1.  

%Compute the evalution for each available move and put them in an array
findMoves(T,ScoreList,GbL) :-
    findPossibleGoals(T,L,GbL),
    scoreMoves(L,ScoreListUnsort,GbL),
    sort(1, @>=, ScoreListUnsort, ScoreList).

scoreMoves([],[],_).

scoreMoves([(T,R,C)|Tail],[[S,(T,R,C,GbLPre)]|Tail2],GbLPre) :-
    placePiece(T,R,C,GbLPre,GbLPost),
    gameBoardScore(S,GbLPost,_,_,_),
    scoreMoves(Tail,Tail2,GbLPre).  
%////////////////////////////////////////    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%MaxMax/MinMax                                                                                                                                                                                                                                                                                                                                                    Max/MaxMax move selection%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
callPlacePiece(_,_,[],[]).

callPlacePiece(Tetraminos,GbList,[(T,R,C)|Taill],[[Tetraminos,GbListPost,(T,R,C)]|Tail2]):-
    placePiece(T,R,C,GbList,GbListPost),
    callPlacePiece(Tetraminos,GbList,Taill,Tail2).

%Node: [Eval,Tetraminos,GbL,Move]
%Tetraminos: a list of tetraminos
%Gb: a list of the occCell for a certain gameboard configuration
%Move: the move [t,r,c] that allow to obatain the current node, added only when nextNodes is called
%Eval: the evaluation of the node, added only when the heuristc is called, nextNodes will not see this one
nextNodes(Level,Node, NextNodes) :-
    nth1(1, Node, Tetraminos),
    nth1(2, Node, GbL),
    nth0(Level, Tetraminos, T),
    findPossibleGoals(T,L,GbL),
    callPlacePiece(Tetraminos,GbL,L,NextNodes).

evaluateNode([Tetraminos,GbL,(T,R,C)], [S,Tetraminos,GbL,(T,R,C)]) :-
    gameBoardScore(S,GbL,_,_,_).

callMinMax(Player,Move) :-
    tetraminos(T),
    createGameBoardList(GbL),
    length(T,Depth),
    StartingNode = [T,GbL],
    NextNodesGenerator = nextNodes,
    Heuristic = evaluateNode,
    minmax(StartingNode, NextNodesGenerator, Heuristic, 0, Depth, -inf, +inf, NextMove, Player),
    nth1(4,NextMove,Move).

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
%   a player first rotate a tetramino in the proper rotation
%   then align it to the point she wants to reach
%   then push down the tetramino in that point
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
priority(rotate,1). %rotate has the minimum priority, it's executed as last move. 

diff(T1,T2,Diff) :-
    T1 == T2,
    Diff is 0,
    !.

diff(_,_,Diff) :-
    Diff is 1,
    !.

serchPath(Start, Goal, Plan) :-
    setof(A, action(A), Actions),
    Heuristic=evaluateMovement, 
    planner(Start, Goal, Actions, Heuristic, Plan).

%getPathOfBestMove search for the best move and then call the planner for the tetris path problem.
%start and goal are inverted because I want to find the path starting from the goal and coming back to the start.
%This allow to deal easily with particulare cases like slide or t-spin.
%If a certain move is actually impossibile to reach (eg: trapped tetramino) the next move is considered.

getPathOfBestMove(Player,Plan) :-
    createGameBoardList(GbL),
    callMinMax(Player,(Tg,Rg,Cg)),
    tetraminos([T|_]),
    firstShape(T,T1),
    Start = (T1,1,5,GbL),
    Goal = (Tg,Rg,Cg,GbL),
    serchPath(Goal, Start, RevPlan),
    reverse(RevPlan,Plan),
    !.
%///////////////////////

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