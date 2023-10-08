:- module(planner, [planner/5]).

%generic A* planner
%How to use it:
%+Start and +Goal must be the start node and the goal node we want to reach.

%+Actions must be a list of predicates that take a node in input and give a node in output, this node must have the same structure of the start and goal node.

%+Heuristic must be the name of a predicate that given an Action, a Node and the goal tell us how much that action can contribute to reach that goal.
%   An higher score imply a most promising action.

%-Plan is the actual computed sequence of action that allow to reach the goal node starting from the start node.
planner(Start, Goal, Actions, Heuristic, Plan) :-
    planner(Start, Goal, Actions, Heuristic, [Start], [], Plan).

planner(Goal, Goal, _, _, _,Plan, Plan) :- !.

%nodes must have the following shape:
%after the evaluation with the heuristic the node became:
%[score,action]
%all the evluated nodes are given in order from the most to the least promising
planner(Start, Goal, Actions, Heuristic, VisitedNodes, TempPlan, Plan) :-
	evaluateActions(Actions,Heuristic,Start,Goal,OrderedNodes),
    member(Node, OrderedNodes), %take the first element in the list [[score,action]...], it's also the bactracking point if there is a fail taking the subsequent nodes.
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
	append(TempActions, [[Score,Action]], TempActions1),
	evaluateActions(T, Heuristic, Input, Goal, TempActions1, OrderedActions).