:- use_module(minmax).
:-dynamic(gB/1).
:-dynamic(turn/1).
:-dynamic(winner/1).
:-dynamic(storyEnabled/0).

gameBoardW(3).
gameBoardH(3).
%storyEnabled.
%at the beginnig: empty board, x begin.
gB([e,e,e,e,e,e,e,e,e]).
turn(x).
aiTurn(o).

next(x,o).
next(o,x).

nextTurn :- 
    next(P1,P2),
    turn(P1),
    assert(turn(P2)),
    retract(turn(P1)).

winningGB(P, [P,P,P,_,_,_,_,_,_]).
winningGB(P, [_,_,_,P,P,P,_,_,_]).
winningGB(P, [_,_,_,_,_,_,P,P,P]).
winningGB(P, [P,_,_,P,_,_,P,_,_]).
winningGB(P, [_,P,_,_,P,_,_,P,_]).
winningGB(P, [_,_,P,_,_,P,_,_,P]).
winningGB(P, [P,_,_,_,P,_,_,_,P]).
winningGB(P, [_,_,P,_,P,_,P,_,_]).

%there is a winner
checKWin(P) :- 
    gB(G),
    winningGB(P,G),
    nl,
    write(P),write(' wins!'),
    assert(winner(P)),
    writeGameBoard,
    !.

%is a tie
checKWin(_) :- 
    gB(G),
    availableMoves(G,L),
    L==[],
    write('Is a tie!'),
    writeGameBoard,
    assert(winner(t)).

%game going on...
checKWin(P) :- 
    gB(G),
    \+ winningGB(P, G).

sublist(L, M, N, S) :-
    findall(E, (between(M, N, I), nth0(I, L, E)), S).

rcToIndex(R,C,I) :- 
    gameBoardW(W),
    gameBoardH(H),
    R<H,
    C<W,
    I is R*W+C.

getSymbol(I,S) :- 
    gB(G),
    nth0(I,G,S).

getSymbol(R,C,S) :- 
    rcToIndex(R,C,I),
    getSymbol(I,S).

putSymbol(I,S,G,Gp) :-
    I1 is I-1,
    nth0(I,G,SOld),
    SOld == 'e',
    sublist(G, 0, I1, G1),
    append(G1,[S],G2),
    I2 is I+1,
    length(G,Len),
    sublist(G, I2, Len, G3),
    append(G2,G3,Gp). 

putSymbol(I,S,Gp) :-
    gB(G),
    putSymbol(I,S,G,Gp).  

putSymbolRC(R,C,S,Gp) :- 
    rcToIndex(R,C,I),
    putSymbol(I,S,Gp).

writeGameBoard :-
    gB(GbL),
	writeGameBoard(0,0,GbL).

writeGameBoard(R,_,_) :- 
    gameBoardH(R).

writeGameBoard(R,C,GbL) :- 
    nl, 
    writeRow(R,C,GbL), 
    R1 is R + 1, 
    writeGameBoard(R1,C,GbL),
    !.

writeRow(_,C,_) :- 
    gameBoardW(C).

writeRow(R,C,GbL) :- 
    getSymbol(R,C,S),
    S \= 'e',
    write([S]), 
    C1 is C + 1, 
    writeRow(R,C1,GbL),
    !.

writeRow(R,C,GbL) :- 
    write(['_']),
    C1 is C + 1,
    writeRow(R,C1,GbL),
    !.

%minmax
%node: [G,P,[M]]; Gameboard, turn Player, move(index and player)
%%NextNodesGenerator
availableMoves(G,L) :-
    findall(I, (nth0(I, G, e)), L),
    !.

availableMoves(_,[]).

%generate next nodes
nextNodes(_, [G,P,_], [[Gp,P2,[Ly,P]]|NextNodes]) :-
    next(P,P2),
    \+winningGB(P2,G),
    availableMoves(G,[Ly|L]),
    !,
    putSymbol(Ly,P,G,Gp),
    nextNodes(_, [G,P,_], L, NextNodes).

nextNodes(_, [G,_,_], []) :-
    availableMoves(G,[]).

nextNodes(_, [G,P,_], [Ly|L], [[Gp,P2,[Ly,P]]|NextNodes]) :-
    next(P,P2),
    putSymbol(Ly,P,G,Gp),
    nextNodes(_, [G,P,_], L, NextNodes).

nextNodes(_, _, [], []).

%%Heuristic
evaluateNode([G,P,M],[S,G,P,M]) :-
    turn(X),
    winningGB(X,G),
    S is 1,
    !.

evaluateNode([G,P,M],[S,G,P,M]) :-
    turn(X),
    next(X,X2),
    winningGB(X2,G),
    S is -1,
    !.

evaluateNode([G,P,M],[S,G,P,M]) :-
    S is 0.

%allows to remember the move chain
%base case: when we collect the value from a leaf
takeMove(_,_,[_,_,_,Move],[Move]).

%recursive case: when we collect the value from non-leaf node
takeMove(_,_,[_,CollectedMoves,_,_,Move],[Move|CollectedMoves]).
%

callMinMax(BestNode) :-
    gB(G),
    turn(P),
    StartingNode = [G,P,[]],
    NextNodesGenerator = nextNodes,
    Heuristic = evaluateNode,
    MoveTaker = takeMove,
    minmax(StartingNode, NextNodesGenerator, Heuristic, MoveTaker, 0, 9, -inf, +inf, BestNode, max).

%min max with story
callMinMax(I,S,Story) :- callMinMax([_,Story,_,_,[I,S]]).

%min max without story(last move)
callMinMax(I,S,[]) :- callMinMax([_,_,_,[I,S]]).

%gameplay
reset :-
    winner(_),
    retract(winner(_)),
    retract(gB(_)),
    assert(gB([e,e,e,e,e,e,e,e,e])),
    retract(turn(_)),
    assert(turn(x)).

readInput(R,C) :-
    write('Write row and column without any space in between (both indexes are in the range [0,2]), then press enter:'),
    nl,
    get_char(R1),
    atom_number(R1,R),
    get_char(C1),
    atom_number(C1,C),
    get_char(_),
    nl.

readInput(R,C) :- 
    write('Wrong Input!'),
    nl,
    readInput(R,C).

readAndPut(S,GbP) :- 
    readInput(R,C),
    putSymbolRC(R,C,S,GbP).

readAndPut(S,GbP) :- 
    write('Cell occupied!'),
    nl,
    readAndPut(S,GbP).

playerVsPlayer :- reset.

playerVsPlayer :-
    \+winner(_),
    turn(P),
    writeGameBoard,
    nl,
    write(P),
    write(' turn, where do you want to place your symbol?'),
    nl,
    readAndPut(P,GbP),
    retract(gB(_)),
    assert(gB(GbP)),
    !,
    checKWin(P),
    nextTurn,
    playerVsPlayer.

writeStory(Story) :-
    storyEnabled,
    write(' given the following evolution of the game: '),write(Story),nl,!.

writeStory(_) :-
    nl,!.

playerVsAi :- reset.

playerVsAi :-
    \+winner(_),
    turn(P),
    aiTurn(P),
    write(P),
    write(', Ai turn: '),
    callMinMax(I,S,Story),
    write([I,S]),
    writeStory(Story),
    putSymbol(I,S,Gp),
    retract(gB(_)),
    assert(gB(Gp)),
    checKWin(P),
    nextTurn,
    playerVsAi.

playerVsAi :-
    \+winner(_),
    turn(P),
    writeGameBoard,
    nl,
    write(P),
    write(' turn, where do you want to place your symbol?'),
    nl,
    readAndPut(P,GbP),
    retract(gB(_)),
    assert(gB(GbP)),
    !,
    checKWin(P),
    nextTurn,
    playerVsAi.

aiVsAi :- reset.

aiVsAi :-
    \+winner(_),
    turn(P),
    writeGameBoard,
    nl,
    write(P),
    write(' turn, press enter to continue:'),
    nl,
    get_char(_),
    callMinMax(I,S,Story),
    write([I,S]),
    writeStory(Story),
    putSymbol(I,S,Gp),
    retract(gB(_)),
    assert(gB(Gp)),
    checKWin(P),
    nextTurn,
    aiVsAi.