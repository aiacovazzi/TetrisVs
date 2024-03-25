:- module(planner, [planner/7]).
%generic Depth-First Heuristc planner
%How to use it:
%+Start and +Goal must be the start node and the goal node we want to reach.

%+Actions must be a list of predicates that take a node in input and give a node in output, this node must have the same structure of the start and goal node.

%+Heuristic must be the name of a predicate that given an Action, a Node and the goal tell us how much that action can contribute to reach that goal.
%   An higher score imply a most promising action.

%-Plan is the actual computed sequence of action that allow to reach the goal node starting from the start node.

%-PlanStory collect the whole searching story in order to use it as explanation.
planner(Start, Goal, Actions, Heuristic, Plan, PlanStory, GoalChecker) :-
    planner(Start, Goal, Actions, Heuristic, [Start], [], Plan, [], PlanStory, GoalChecker).

planner(CurrentNode, Goal, _, _, _,Plan, Plan, TempPlanStory, PlanStory, GoalChecker) :- 
    call(GoalChecker,CurrentNode,Goal),
    append(TempPlanStory,[Plan],PlanStory),
    !.

%nodes must have the following shape:
%after the evaluation with the heuristic the node became:
%[score,action]
%all the evluated nodes are given in order from the most to the least promising
planner(CurrentNode, Goal, Actions, Heuristic, VisitedNodes, TempPlan, Plan, TempPlanStory, PlanStory, GoalChecker) :-
	evaluateActions(Actions,Heuristic,CurrentNode,Goal,OrderedEvaluatedActions),
    append(TempPlanStory,[TempPlan],TempPlanStory1),
    member([_,Action], OrderedEvaluatedActions), %take the first element in the list [[score,action]...], it's also the bactracking point if there is a fail taking the subsequent nodes.
    Z =.. [Action,CurrentNode,SuccessorNode],
    call(Z), %it fails if the action is not elegible.
    \+member(SuccessorNode,VisitedNodes), %it fails if the goal has been already visited.
    append(VisitedNodes,[SuccessorNode],VisitedNodes1),
    append(TempPlan,[Action],TempPlan1),
	planner(SuccessorNode, Goal, Actions, Heuristic, VisitedNodes1, TempPlan1, Plan, TempPlanStory1, PlanStory, GoalChecker).

evaluateActions(Actions, Heuristic, Input, Goal, OrderedActions) :-
	evaluateActions(Actions, Heuristic, Input, Goal, [], OrderedActions).

evaluateActions([], _, _, _, [], []).

evaluateActions([], _, _, _, TempActions, OrderedActions) :-
	sort(1, @>=, TempActions, OrderedActions).
	
evaluateActions([Action|T], Heuristic, Input, Goal, TempActions, OrderedActions) :-
    Eval = [Action, Input, Goal],
    ExecuteEvaluation =.. [Heuristic,Eval,Score], 
    call(ExecuteEvaluation),
	append(TempActions, [[Score,Action]], TempActions1),
	evaluateActions(T, Heuristic, Input, Goal, TempActions1, OrderedActions).