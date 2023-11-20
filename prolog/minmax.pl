:- module(minmax, [minmax/10]).

nextplayer(max,min).
nextplayer(min,max).
nextplayer(maxmax,maxmax).

operator(max,'@>=').
operator(min,'@=<').
operator(maxmax,'@>=').

%minmax recursive case
minmax(CurrentNode, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, NextNodesEvaluated, Player) :-
    call(NextNodesGenerator,Level,CurrentNode,NextNodes),
    minmax1(NextNodes,CurrentNode, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, NextNodesEvaluated, Player),
    !.

%minmax, max depth reached
minmax(CurrentNode, _, Heuristic, Depth, Depth, _, _, CurrentNodeEvaluated, _, _) :-
    evaluate(Heuristic,CurrentNode,CurrentNodeEvaluated).

%no successor for the current node
minmax1([],CurrentNode, _, Heuristic, _, _, _, _, CurrentNodeEvaluated, _, _) :-
    evaluate(Heuristic,CurrentNode,CurrentNodeEvaluated).

%there are successors for the current node
minmax1(NextNodes, CurrentNode, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, SortedEvaluatedNodes, Player) :-
    %call the minmax for each node in NextNodes and collect evaluation for each node for the next player and level+1
    iterativeEval(NextNodes, EvaluatedNodes, NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, Player),
    %BestNode is the best one according to the operator related to the player
    operator(Player,Operator),
    sort(1, Operator, EvaluatedNodes, SortedEvaluatedNodes),
    nth1(1,SortedEvaluatedNodes,BestNode),
    getBest(Level,CurrentNode,BestNode,CurrentNodeEvaluated).

iterativeEval([CurrentNode|NextNodes], [CurrentNodeEvaluated|NextNodes2], NextNodesGenerator, Heuristic, Level, Depth, Alpha, Beta, Player):-
    Level1 is Level + 1,
    nextplayer(Player,NextPlayer),
    minmax(CurrentNode, NextNodesGenerator, Heuristic, Level1, Depth, Alpha, Beta, CurrentNodeEvaluated, _, NextPlayer),
    updateAlphaBeta(Player,CurrentNodeEvaluated,Alpha,Beta,NextNodes,Alpha1,Beta1,NextNodes1),
    iterativeEval(NextNodes1, NextNodes2, NextNodesGenerator, Heuristic, Level, Depth, Alpha1, Beta1, Player).

iterativeEval([], [], _, _, _, _, _, _, _).

%If the level is > 0 then I bind the best score to CurrentNode, otherwise i give as result the best successor node.
getBest(0,_,BestNode,BestNode) :- !.

getBest(_,CurrentNode,BestNode,CurrentNodeEvaluated) :-
    nth1(1,BestNode,Evaluation),
    append([Evaluation],CurrentNode,CurrentNodeEvaluated),
    !. 

evaluate(Heuristic,CurrentNode,CurrentNodeEvaluated) :-
    call(Heuristic,CurrentNode,CurrentNodeEvaluated).

%AlphBeta pruning disabled for maxmax mode
updateAlphaBeta(maxmax,_,Alpha,Beta,NextNodes,Alpha,Beta,NextNodes).

updateAlphaBeta(max,[Eval,_,_,_],Alpha,Beta,NextNodes,Alpha1,Beta1,NextNodes1) :-
    Alpha1 is max(Eval,Alpha),
    Beta1 = Beta,
    pruneNextNodes(Alpha1,Beta1,NextNodes,NextNodes1).

updateAlphaBeta(min,[Eval,_,_,_],Alpha,Beta,NextNodes,Alpha1,Beta1,NextNodes1) :-
    Beta1 is min(Eval,Beta),
    Alpha1 = Alpha,
    pruneNextNodes(Alpha1,Beta1,NextNodes,NextNodes1).

pruneNextNodes(Alpha1,Beta1,_,NextNodes1) :-    
    Beta1 =< Alpha1,
    NextNodes1 = [],
    !.

pruneNextNodes(_,_,NextNodes,NextNodes).