:- module(minmax, [minmax/10]).
%generic MinMax MaxMax algorithm
%How to use it:
%+CurrentNode must be the start game state.

%+NextNodesGenerator must be a predicate capable of generating a lis of new game state given the current one.

%+Heuristic must be a predicate capable of returning an utility score associated to a terminal state.
%   An higher score imply a good move for the 'max' player.

%+MoveTaker must be a predicate capable taking information from a state in order to construct the node story related to the selected move.

%+Level is the start level, usually set to 0.

%+Depth is the maximum depth reachable my the algorithm before performing the static ealuation of the node.

%+Alpha and +Beta are the parameter used to prune the tree when a part of it doesn't nedd to be evaluated since a player at higher level has alredy a better option available.

%+Player is the starting player, when it is 'max' allows to usie the standard minmax algorithm, when it is 'maxmax' allows the algorithm to work as a lookahed algorithm

%-CurrentNodeEvaluated is the best successor node evaluated, it will contain also the move that lead to the best move detected at the end of the node.

nextplayer(max,min).
nextplayer(min,max).
nextplayer(maxmax,maxmax).

operator(max,'@>=').
operator(min,'@=<').
operator(maxmax,'@>=').

%minmax, max depth reached
minmax(CurrentNode, _, Heuristic, _, Depth, Depth, _, _, CurrentNodeEvaluated, Player) :-
    evaluate(Player,Heuristic,CurrentNode,CurrentNodeEvaluated),!.

%minmax recursive case
minmax(CurrentNode, NextNodesGenerator, Heuristic, MoveTaker, Level, Depth, Alpha, Beta, CurrentNodeEvaluated,  Player) :-
    %nl,write('Current Node: '),write(CurrentNode),
    %nl,write('Level: '),write(Level),
    %nl,write('Player: '),write(Player),
    call(NextNodesGenerator,Player,Level,CurrentNode,NextNodes),
    %nl,write('NextNodes: '),write(NextNodes),
    minmax1(NextNodes,CurrentNode, NextNodesGenerator, Heuristic, MoveTaker, Level, Depth, Alpha, Beta, CurrentNodeEvaluated,  Player).

%no successor for the current node
minmax1([],CurrentNode, _, Heuristic, _, _, _, _, _, CurrentNodeEvaluated, Player) :-
    %nl,write('Evaluate: '),write(CurrentNode),
    evaluate(Player,Heuristic,CurrentNode,CurrentNodeEvaluated),!.
    %nl,write('EvaluatedNode: '),write(CurrentNodeEvaluated).

%there are successors for the current node
minmax1(NextNodes, CurrentNode, NextNodesGenerator, Heuristic, MoveTaker, Level, Depth, Alpha, Beta, CurrentNodeEvaluated, Player) :-
    %call the minmax for each node in NextNodes and collect evaluation for each node for the next player and level+1
    iterativeEval(NextNodes, EvaluatedNodes, NextNodesGenerator, Heuristic, MoveTaker, Level, Depth, Alpha, Beta, Player),
    %BestNode is the best one according to the operator related to the player
    operator(Player,Operator),
    sort(1, Operator, EvaluatedNodes, SortedEvaluatedNodes),
    nth1(1,SortedEvaluatedNodes,BestNode),
    getBest(MoveTaker,Level,CurrentNode,BestNode,CurrentNodeEvaluated).  

iterativeEval([CurrentNode|NextNodes], [CurrentNodeEvaluated|NextNodes2], NextNodesGenerator, Heuristic, MoveTaker, Level, Depth, Alpha, Beta, Player):-
    Level1 is Level + 1,
    nextplayer(Player,NextPlayer),
    minmax(CurrentNode, NextNodesGenerator, Heuristic, MoveTaker, Level1, Depth, Alpha, Beta, CurrentNodeEvaluated, NextPlayer),
    updateAlphaBeta(Player,CurrentNodeEvaluated,Alpha,Beta,NextNodes,Alpha1,Beta1,NextNodes1),
    iterativeEval(NextNodes1, NextNodes2, NextNodesGenerator, Heuristic, MoveTaker, Level, Depth, Alpha1, Beta1, Player).

iterativeEval([], [], _, _, _, _, _, _, _, _).

%If the level is > 0 then I bind the best score to CurrentNode, otherwise i give as result the best successor node.
getBest(_,0,_,BestNode,BestNode) :- !.

getBest(MoveTaker,_,CurrentNode,BestNode,CurrentNodeEvaluated) :-
    nth1(1,BestNode,Evaluation),
    call(MoveTaker,BestNode,Moves),!,
    append([Evaluation,Moves],CurrentNode,CurrentNodeEvaluated),
    !. 

evaluate(Player,Heuristic,CurrentNode,CurrentNodeEvaluated) :-
    call(Heuristic,Player,CurrentNode,CurrentNodeEvaluated).
    

%AlphBeta pruning disabled for maxmax mode
updateAlphaBeta(maxmax,_,Alpha,Beta,NextNodes,Alpha,Beta,NextNodes).

updateAlphaBeta(max,CurrentNodeEvaluated,Alpha,Beta,NextNodes,Alpha1,Beta1,NextNodes1) :-
    nth1(1,CurrentNodeEvaluated,Eval),
    Alpha1 is max(Eval,Alpha),
    Beta1 = Beta,
    pruneNextNodes(Alpha1,Beta1,NextNodes,NextNodes1).

updateAlphaBeta(min,CurrentNodeEvaluated,Alpha,Beta,NextNodes,Alpha1,Beta1,NextNodes1) :-
    nth1(1,CurrentNodeEvaluated,Eval),
    Beta1 is min(Eval,Beta),
    Alpha1 = Alpha,
    pruneNextNodes(Alpha1,Beta1,NextNodes,NextNodes1).

pruneNextNodes(Alpha1,Beta1,_,NextNodes1) :-    
    Beta1 =< Alpha1,
    NextNodes1 = [],
    !.

pruneNextNodes(_,_,NextNodes,NextNodes).