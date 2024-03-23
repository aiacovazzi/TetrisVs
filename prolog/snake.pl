:- use_module(planner).
:-dynamic(snake/1).
:-dynamic(food/1).
%gameboard dimensions
gameBoardW(9).
gameBoardH(9).

snake([[5,4],[5,5],[5,6]]).
food([[]]).

generateFood(Snake) :-
gameBoardW(W),
gameBoardH(H),
random(0, W, C),
random(0, H, R),
\+checkSnake(R,C,Snake),
retract(food(_)),
assert(food([[R,C]])),
!.

%if an invalid food is generated and there is space...
generateFood(Snake) :- 
    gameBoardW(W),
    gameBoardH(H),
    length(Snake,L),
    MaxLength is W*H,
    L \= MaxLength,
    generateFood(Snake),
    !.

generateFood(_) :- 
    write('No more space availabe! You win!').

checkSnake(R,C,Snake) :-
    memberchk([R,C],Snake).

checkFood(R,C,Food) :-
    memberchk([R,C],Food).


eat(S,F,Sp) :- append(F,S,Sp). 

withoutLast([_], []).
withoutLast([X|Xs], [X|WithoutLast]) :- 
    withoutLast(Xs, WithoutLast).

moveDown(S,Sp) :-
    nth0(0,S,[R,C]),
    R1 is R+1,
    gameBoardH(H),
    R1<H,
    \+member([R1,C],S),
    append([[R1,C]],S,Sp1),
    withoutLast(Sp1,Sp).

moveUp(S,Sp) :-
    nth0(0,S,[R,C]),
    R1 is R-1,
    R1>=0,
    \+member([R1,C],S),
    append([[R1,C]],S,Sp1),
    withoutLast(Sp1,Sp).

moveLeft(S,Sp) :-
    nth0(0,S,[R,C]),
    C1 is C-1,
    C1>=0,
    \+member([R,C1],S),
    append([[R,C1]],S,Sp1),
    withoutLast(Sp1,Sp).

moveRight(S,Sp) :-
    nth0(0,S,[R,C]),
    C1 is C+1,
    gameBoardW(W),
    C1<W,
    \+member([R,C1],S),
    append([[R,C1]],S,Sp1),
    withoutLast(Sp1,Sp).

writeGameBoard :-
    snake(S),
    food(F),
	writeGameBoard(0,0,S,F).

writeGameBoard(R,_,_,_) :- 
    gameBoardH(R).

writeGameBoard(R,C,S,F) :- 
    nl,
    writeRow(R,C,S,F), 
    R1 is R + 1, 
    writeGameBoard(R1,C,S,F),
    !.

writeRow(_,C,_,_) :- 
    gameBoardW(C).

writeRow(R,C,S,F) :- 
    checkSnake(R,C,S), 
    write([■]), 
    C1 is C + 1, 
    writeRow(R,C1,S,F),
    !.

writeRow(R,C,S,F) :- 
    checkFood(R,C,F), 
    write([▣]), 
    C1 is C + 1, 
    writeRow(R,C1,S,F),
    !.

writeRow(R,C,S,F) :- 
    write([□]),
    C1 is C + 1,
    writeRow(R,C1,S,F),
    !.

%planner predicates

action(moveUp).
action(moveLeft).
action(moveDown).
action(moveRight).

evaluateMovement([moveUp, S, F], Score) :-
    nth0(0,S,[R1,_]),
    nth0(0,F,[R2,_]),
	Score is (R2 - R1) * -1.

evaluateMovement([moveDown, S, F], Score) :-
    nth0(0,S,[R1,_]),
    nth0(0,F,[R2,_]),
	Score is (R2 - R1).

evaluateMovement([moveRight, S, F], Score) :-
    nth0(0,S,[_,C1]),
    nth0(0,F,[_,C2]),
	Score is (C2 - C1).

evaluateMovement([moveLeft, S, F], Score) :-
    nth0(0,S,[_,C1]),
    nth0(0,F,[_,C2]),
	Score is (C2 - C1) * -1.

checkGoal([H|_],[F|_]) :-
    H == F.
	
serchPath(Start, Goal, Plan, PlanStory) :-
    findall(A, action(A), Actions),
    Heuristic=evaluateMovement, 
    GoalChecker = checkGoal,
    planner(Start, Goal, Actions, Heuristic, Plan, PlanStory, GoalChecker).

%all plan executed, commit new snake
executePlan(S,F,[_]) :-
    eat(S,F,Sp),
    retract(snake(_)),
    assert(snake(Sp)),!.

executePlan(S,F,[Action|T]) :-
    write('press enter to continue...'),nl,
    get_char(_),
    call(Action,S,S2),
    food(F),
    write('\33\[2J'),writeGameBoard(0,0,S2,F),nl,
    executePlan(S2,F,T).

gameLoop :-
    snake(Start),
    generateFood(Start),
    food(Goal),
    serchPath(Start, Goal, Plan, _),
    write('\33\[2J'),writeGameBoard,nl,
    executePlan(Start,Goal,Plan),
    gameLoop.

start :- gameLoop.