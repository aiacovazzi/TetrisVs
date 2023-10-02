:- use_module(library(lists)).
/*To do:
-finire di implementare i tetramini
-l'euristica di selezione della mossa è da migliorare.
-il planner usato come step successivo trovata la mossa migliore, se fallisce bisogna provare con la seconda migliore e così via...
    %non posso dare sempre per scontato che la mossa migliore trovata sdarà raggiungbile, ci saranno rari casi in cui non lo è, es:

    [7][x][x][x][x][x][x][x][o][o][o]
    [8][x][x][x][x][x][x][x][o][?][o]
    [9][x][x][x][x][x][x][?][?][?][o]
    [-][0][1][2][3][4][5][6][7][8][9]
    The L represent by ? cannot  fit there even if it is the best place.
-prima integrazione col gioco Js (TEST)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-modifica dell'algoritmo di ricerca della mossa per gestire intelligentemente il secondo pezzo 
    -%attualmente è strettamente collegata al primo, inoltre è specifica per tetris)
-evoluzione del'algoritmo allo step precedente per farlo diventare min/max
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Defining the game board properties and the occupied cells%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gameBoardW(10).
gameBoardH(10).

:-dynamic occCell/2.

%vertical S piece at the bottom right of the board
occCell(9,9).
occCell(8,9).
occCell(8,8).
occCell(8,7).
occCell(8,6).
occCell(8,5).
occCell(8,4).
occCell(7,8).

%//////////////////////////////////////////////////////////



%%%%%%%%%%%%%%%%%
%Auxiliary rules%
%%%%%%%%%%%%%%%%%
%

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
%


%Check if a cell is avalaible and if the indexes are not out of bound.
freeCell(R,C) :- 
    gameBoardH(H), 
    gameBoardW(W), 
    R >= 0, R<H, 
    C >=0, C<W, 
    \+occCell(R,C).

%Generate all possible free cell on the gameboard
freeCell1(R,C) :-     
    gameBoardH(W),
    gen(0, W, R),
    gameBoardH(H),
    gen(0, H, C),
    \+occCell(R,C).
    
%/////////////////

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Tetraminos' operators and checks%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Mapping each tetramino with its rotations.
%Can be used also to obtain the next rotation of a given tetramino.
rotation(o,o,o).
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

fitPiece(o,R1,C1,R2,C2,R3,C3,R4,C4) :- 
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
%

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
%

%Put the piece in the given reference point and then assert all the occupied cells.
placePiece(T,R1,C1,L) :-
    fitPiece(T,R1,C1,R2,C2,R3,C3,R4,C4),
    L = [occCell(R1,C1),occCell(R2,C2),occCell(R3,C3),occCell(R4,C4)],
    assertPiece(L).

assertPiece([]).

assertPiece([H|T]):-
    assert(H),
    assertPiece(T).
	
retractPiece([]).

retractPiece([H|T]):-
    retract(H),
    retractPiece(T).

%...

%Discover all the possible cell where a tetramino can be placed for all its rotations
findPossibleGoals(T,List) :- 
    findall((T1), rotation(T, T1, _), Lr),
    findPossibleGoals0([],Lr,List).

findPossibleGoals0(Found,[],Found).

findPossibleGoals0(Found,[Ht|T],L) :-
    findall((Ht,R,C), tetraminoGoal(Ht,R,C), List),
    append(Found,List,NewList),
    findPossibleGoals0(NewList,T,L).

%a tetramino cam be placed on a free cell when the next row it collide
tetraminoGoal(T,R,C) :-
    freeCell1(R,C),
    fitPiece(T,R,C,_,_,_,_,_,_),
    R1 is R + 1,
    \+fitPiece(T,R1,C,_,_,_,_,_,_).
%///////////////////


%%%%%%%%%%%%%%%%%%%%%
%GameBoard Evaluator%
%%%%%%%%%%%%%%%%%%%%%

%1) Compute the number of holes in the gameboard.
%This algorithm consider as holes all the empty cell below an occupied cell of a certain column.
%The holes are often the result of a bad move, so the goal is to choose a mve that minimize this number.
holesInGameBoard(N) :- 
    holesInGameBoard0(0,0,N).

holesInGameBoard0(C,N,N):-
    gameBoardW(C).

holesInGameBoard0(C,N,T):-
    holesInColumn(0,C,n,0,Nh),
    N1 is N + Nh,
    C1 is C + 1,
    holesInGameBoard0(C1,N1,T),
    !.

holesInColumn(R,_,_,N,N) :- 
    gameBoardH(R).

holesInColumn(R,C,n,N,NT) :- 
    freeCell(R,C),
    R1 is R + 1,
    holesInColumn(R1,C,n,N,NT),
    !.

holesInColumn(R,C,y,N,NT) :- 
    freeCell(R,C),
    R1 is R + 1,
    N1 is N + 1,
    holesInColumn(R1,C,y,N1,NT),
    !.

holesInColumn(R,C,_,N,NT) :- 
    occCell(R,C),
    R1 is R + 1,
    holesInColumn(R1,C,y,N,NT),
    !.
%

%2) Compute the etropy of each row, the give average value as measure of the "entropy" of the gameboard.

%Count the occupied cell of a row.
countOccCelInRow(R,T) :- 
    countOccCelInRow(R,0,0,T).

countOccCelInRow(_,C,N,N) :-
    gameBoardW(C).

countOccCelInRow(R,C,N,T) :-
    occCell(R,C),
    N1 is N+1,
    C1 is C+1,
    countOccCelInRow(R,C1,N1,T),
    !.

%if the previous rule fails then is a free cell, don't need to check it
countOccCelInRow(R,C,N,T) :-
    C1 is C+1,
    countOccCelInRow(R,C1,N,T),
    !.

%Compute the entropy of a row and give as output also the numbero of occupied cells.
entropyOfRow(R,Ent) :-
    countOccCelInRow(R,Occ),
    gameBoardW(W),
    Free is W - Occ,
    OccRatio is Occ/W,
    FreeRatio is Free/W,
    logBase2(OccRatio,Log2OccRatio),
    logBase2(FreeRatio,Log2FreeRatio),
    Ent is -OccRatio*Log2OccRatio -FreeRatio*Log2FreeRatio,
    !.

avgEntropyOfGameBoard(AvgEnt) :-
    gameBoardH(H),
    H1 is H-1,
    avgEntropyOfGameBoard(H1,0,0,AvgEnt).

%reached the top, stop
avgEntropyOfGameBoard(-1,Acc,Div,AvgEnt) :-
    AvgEnt is Acc/Div.

%empty gameboard stop
avgEntropyOfGameBoard(R,_,0,0):-
    countOccCelInRow(R,Occ),
    Occ = 0.

%reached an empty row, stop
avgEntropyOfGameBoard(R,Acc,Div,AvgEnt):-
    countOccCelInRow(R,Occ),
    Occ = 0,
    AvgEnt is Acc/Div.    

avgEntropyOfGameBoard(R,Acc,Div,AvgEnt):-
    entropyOfRow(R,Ent),
    Acc1 is Acc + Ent,
    R1 is R - 1,
    Div1 is Div + 1,
    avgEntropyOfGameBoard(R1,Acc1,Div1,AvgEnt),
    !.    

%3) Compute the number of non-consecutive empty columns.
%Each time the gamboard alternate an empty column to a full column (where full means at least one cell
%occupied) a counter is increased, the total number is given as output only if it is >= than 2.
nonConsecutiveEmptyCol(N) :-
    emptyColumn(0),
    nonConsecutiveEmptyCol0(1,e,0,N),
    !.
%if the preceeding rule does not match means that the column 0 was not empty...
nonConsecutiveEmptyCol(N) :-
    nonConsecutiveEmptyCol0(1,f,0,N),
    !.

nonConsecutiveEmptyCol0(C,_,N,N) :-
    gameBoardW(C), 
    N >= 2.

nonConsecutiveEmptyCol0(C,_,_,0) :-
    gameBoardW(C).

nonConsecutiveEmptyCol0(C,e,N,T) :-
    emptyColumn(C),
    C1 is C+1,
    nonConsecutiveEmptyCol0(C1,e,N,T),
    !.

%if the preceeding rule does not match means that the column was not empty...
nonConsecutiveEmptyCol0(C,e,N,T) :-
    C1 is C+1,
    N1 is N+1,
    nonConsecutiveEmptyCol0(C1,f,N1,T),
    !.

nonConsecutiveEmptyCol0(C,f,N,T) :-
    \+emptyColumn(C),
    C1 is C+1,
    nonConsecutiveEmptyCol0(C1,f,N,T),
    !.

%if the preceeding rule does not match means that the column was empty...
nonConsecutiveEmptyCol0(C,f,N,T) :-
    C1 is C+1,
    nonConsecutiveEmptyCol0(C1,e,N,T),
    !.    

emptyColumn(C) :-
    emptyColumn(0,C,y).    

emptyColumn(R,_,y) :-
    gameBoardH(R).

emptyColumn(R,C,E) :-
    freeCell(R,C),
    R1 is R + 1,
    emptyColumn(R1,C,E),
    !.

emptyColumn(R,C,n) :-
    occCell(R,C),
    !.
%

%return the Row number of the cleared row
clearedRow(C) :-
    gameBoardH(H),
    H1 is H - 1,
    clearedRow(H1,[],C).

%reached the top, stop.
clearedRow(-1,Ct,Ct).

%reached the first empty row, stop.
clearedRow(R,Ct,Ct) :-
    countOccCelInRow(R,T),
    T=0.

clearedRow(R,Ct,C) :-
    gameBoardW(W),
    countOccCelInRow(R,OccCell),
    W=OccCell,
    append(Ct,[R],Ct1),
    R1 is R -1,
    clearedRow(R1,Ct1,C),
    !.

%called if the previous rule fails
clearedRow(R,Ct,C) :-
    R1 is R -1,
    clearedRow(R1,Ct,C),
    !.
    

%Compute the whole GameBoard score
gameBoardScore(S) :-
    holesInGameBoard(N),
    avgEntropyOfGameBoard(AvgEnt),
    nonConsecutiveEmptyCol(EC),
    S is N + AvgEnt + EC.  

%Compute the evalution for each available move and put them in an array

findMoves(T,ScoreList) :-
    findMoves(T,[],ScoreListUnsort),
    sort(1, @=<, ScoreListUnsort, ScoreList).
    
findMoves(T,M,ScoreList) :-
	findPossibleGoals(T,L),
    scoreMoves(L,M,ScoreList).

scoreMoves([],M,M).

scoreMoves([(T,R,C)|Tail],M,ScoreList) :-
    placePiece(T,R,C,L),
    gameBoardScore(S),
    retractPiece(L),
    append(M,[(S,T,R,C)],M1),
    scoreMoves(Tail,M1,ScoreList).  
%////////////////////////////////////////    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Planner%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%

%the starting position of the playing tetramino
%tetramino(T1,R1,C1).
start((i1,0,6)).

%the position goal
%goal(T2,R2,C2).
goal((i1,9,6)).

%Define actions
%setof(A, action(A), Action)

action(rotate).
action(right).
action(left).
action(down).

%All the action replicate the input when can't be used.
rotate((T1,R1,C1),(T2,R1,C1)) :-
    rotation(_,T1,T2),
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

planner(Start, Goal, Actions, Heuristic, Plan) :-
    planner(Start, Goal, Actions, Heuristic, [Start], [], Plan).

planner(Goal, Goal, _, _, _,Plan, Plan) :- !.

%nodes must have the following shape:
%after the evaluation with the heuristic the node became:
%[score,action]
%all the evluated nodes are given in order from the most to the least promising
planner(Start, Goal, Actions, Heuristic, VisitedNodes, TempPlan, Plan) :-
	evaluateActions(Actions,Heuristic,Start,Goal,OrderedNodes),
    member(Node, OrderedNodes), %take the first element in the list [[score,action]...], it's also the bactracking point if call(Z) fail.
    nth1(2, Node, Action), %take the action 
    %nth1(1, Node, Score), %take the score
    Z =.. [Action,Start,Start1],
    call(Z), %it fails if the action is not elegible.
    \+member(Start1,VisitedNodes), %it fails if the goal has been already visited.
    append(VisitedNodes,[Start1],VisitedNodes1),
    append(TempPlan,[Action],TempPlan1),
	planner(Start1, Goal, Actions, Heuristic, VisitedNodes1, TempPlan1, Plan).

evaluateActions(Actions, Heuristic, Input, Goal, OrderedActions) :-
	evaluateActions(Actions, Heuristic, Input, Goal, [], OrderedActions).

evaluateActions([], _, _, _, [], []).

evaluateActions([], _, _, _, TempActions, OrderedActions) :-
	sort(1, @>=, TempActions, OrderedActions).
	
evaluateActions([Action|T], Heuristic, Input, Goal, TempActions, OrderedActions) :-
    %[action, input, output]
    Eval = [Action, Input, Goal],
    Z =.. [Heuristic,Eval,Score], 
    call(Z),
    %epurateZero(Score,Action,ScoreActionL), %score zero is given for useless moves given the goal
	append(TempActions, [[Score,Action]], TempActions1),
	evaluateActions(T, Heuristic, Input, Goal, TempActions1, OrderedActions).

epurateZero(0, _, []) :- !.
epurateZero(Score, Action, [[Score,Action]]).

%call the planner for the tetris path problem, start and goal are inverted because I want to find the path starting from the
%goal and coming back to the start.
%This allow to deal easily with particulare cases live slide or T-spin.
serchPath(Plan) :-
    setof(A, action(A), Actions),
    Heuristic=evaluateMovement, 
    start(Goal), 
    goal(Start),
    planner(Start, Goal, Actions, Heuristic, RevPlan),
    reverse(RevPlan,Plan).



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
    write([-]),
    writeColNumbers(C).

writeGameBoard(R,C) :- 
    nl, 
    write([R]),
    writeRow(R,C), 
    R1 is R + 1, 
    writeGameBoard(R1,C),
    !.

writeRow(_,C) :- 
    gameBoardW(C).

writeRow(R,C) :- 
    occCell(R,C), 
    write([o]), 
    C1 is C + 1, 
    writeRow(R,C1),
    !.

writeRow(R,C) :- 
    write([x]),
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
