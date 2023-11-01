:- module(minmax, [minmax/9]).

nextplayer(max,min).
nextplayer(min,max).
nextplayer(maxmax,maxmax).

operator(max,'@>=').
operator(min,'@=<').
operator(maxmax,'@>=').

%minmax recursive case
minmax(CurrentNode, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, Player) :-
    call(NextNodesGenerator,Level,CurrentNode,NextNodes),
    minmax1(NextNodes,CurrentNode, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, Player),
    !.

%minmax, max depth reached
minmax(CurrentNode, _, Heuristic, Depth, Depth, _, _, CurrentNodeEvaluated, _) :-
    evaluate(Heuristic,CurrentNode,CurrentNodeEvaluated).

%no successor for the current node
minmax1([],CurrentNode, _, Heuristic, _, _, _, _, CurrentNodeEvaluated,_) :-
    evaluate(Heuristic,CurrentNode,CurrentNodeEvaluated).

%there are successor for the current node
minmax1(NextNodes, CurrentNode, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, Player) :-
    %call the minmax for each node in NextNodes and collect evaluation for each node for the next player and level+1
    Level1 is Level + 1,
    nextplayer(Player,NextPlayer),
    iterativeEval(NextNodes, NextNodesGenerator, Heuristic, Level1, Depth, Alpha, Beta, EvaluatedNodes, NextPlayer),
    %BestNode is the best one according to the operator related to the player
    operator(Player,Operator),
    sort(1, Operator, EvaluatedNodes, SortedEvaluatedNodes),
    nth1(1,SortedEvaluatedNodes,BestNode),
    getBest(Level,CurrentNode,BestNode,CurrentNodeEvaluated).

iterativeEval(NextNodes, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, EvaluatedNodes,Player):-
    iterativeEval1(NextNodes, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, [], EvaluatedNodes, Player).

iterativeEval1([CurrentNode|NextNodes], NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, TempEvaluatedNodes, EvaluatedNodes, Player):-
    minmax(CurrentNode, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, Player),
    append(TempEvaluatedNodes,[CurrentNodeEvaluated],TempEvaluatedNodes1),
    iterativeEval1(NextNodes, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, TempEvaluatedNodes1, EvaluatedNodes, Player).

iterativeEval1([], _, _, _, _, _, _, TempEvaluatedNodes, TempEvaluatedNodes, _).

%If the level is > 0 then I bind the best score to CurrentNode, elsewhere i give as result the best successor node.
getBest(0,_,BestNode,BestNode) :- !.

getBest(_,CurrentNode,BestNode,CurrentNodeEvaluated) :-
    nth1(1,BestNode,Evaluation),
    append([Evaluation],CurrentNode,CurrentNodeEvaluated),
    !. 

evaluate(Heuristic,CurrentNode,CurrentNodeEvaluated) :-
    call(Heuristic,CurrentNode,CurrentNodeEvaluated).