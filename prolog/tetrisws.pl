%resources: https://github.com/Anniepoo/swiplwebtut/blob/master/web.adoc
:- use_module(tetris).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Web Server Interface Config%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_setting(http:cors, [*]).
:- http_server(http_dispatch, [port(7777)]).

%the root is for debugging printing the current GameBoard.
:- http_handler(root(home), writeGB, []).
writeGB(_Request) :-
    format('Content-type: text/plain~n~n'),
    writeGameBoard.

%retcell(retract all the occupied cell)
:- http_handler(root(retcell), retractCell, []).
retractCell(_Request) :-
    cors_enable,
    retractall(startGbL(_)),
    retractall(explanation(_,_,_)),
    reply_json_dict('ok').

%put/T/R/C (put a tetramino and compute the new gameboard)
:- http_handler(root(put/T/R/C), put(T,R,C), []).
put(T,R,C,_Request) :-
    cors_enable,
    string_to_atom(T,T1),
    atom_number(R,R1),
    atom_number(C,C1),
    placePiece(T1,R1,C1),
    reply_json_dict('ok').

%resetStart (retract all the starting tetraminos)
:- http_handler(root(resetstart), resetStart, []).
resetStart(_Request) :-
    cors_enable,
    retractall(tetraminos(_)),
    assertz(tetraminos([])),
    reply_json_dict('ok').

%start/T (assert the starting tetraminos)
:- http_handler(root(start/T), reqStart(T), []).
reqStart(T,_Request) :-
    cors_enable,
    string_to_atom(T,T1),
    start(T1),
    reply_json_dict('ok').

%path (find the best path for the tetramino)
:- http_handler(root(path/Player), path(Player), []).
path(Player,_Request) :-
    cors_enable,
    getPathOfBestMove(Player,Plan),
    reply_json_dict(Plan).

%explainMove (return the asserted explanation about move)
:- http_handler(root(explainMove), explainMove, []).
explainMove(_Request) :-
   cors_enable,
   explanation(FullNodeStory,_,_),
   nodeStoryInListForm(FullNodeStory,FullNodeStoryList),
   reply_json_dict(FullNodeStoryList).

nodeStoryInListForm([(_,_,_,_,_)], []).
nodeStoryInListForm([], []).
nodeStoryInListForm([(T,R,C)|FullNodeStoryTail], [[T,R,C]|FullNodeStoryListTail]) :-
    nodeStoryInListForm(FullNodeStoryTail,FullNodeStoryListTail).

%explainPath (return the asserted explanation about move)
:- http_handler(root(explainPath), explainPath, []).
explainPath(_Request) :-
    cors_enable,
    explanation(_,PathStory,_),
    format('Content-type: text/plain~n~n'),
    write('This shows how the path has been found. When a step is followed by a shorter step it indicates a backtraking point where a point of failure occurred.'),nl,
    writePath(PathStory).

writePath([]).
writePath([Path|PathStoryTail]) :-
    write(Path),nl,
    writePath(PathStoryTail).

%explainHeuristic
:- http_handler(root(explainHeuristic), explainHeuristic, []).
explainHeuristic(_Request) :-
    cors_enable,
    explanation(FullNodeStory,_,(AggHeight,ClRow,Holes,Bump,Ent)),
    nodeStoryInListForm(FullNodeStory,FullNodeStoryList),
    last(FullNodeStoryList,BestFutureMove),
    format('Content-type: text/plain~n~n'),
    write('This shows the relevance of each component of the heuristic that evaluate the best move to reach.'),nl,
    write('The move '),write(BestFutureMove),write(' is the best to reach because:'),nl,
    %
    writeClearedRows(ClRow),
    write('Minimize the sum of the entropy of each row having a value of: '),write(Ent),write('.'),nl,
    write('Minimize the sum of the aggregate height of all the columns having a value of: '),write(AggHeight),write('.'),nl,
    write('Minimize the holes under blocks having a value of: '),write(Holes),write('.'),nl,
    write('Minimize the difference of height between differernt column (bumpiness) having a value of: '),write(Bump),write('.'),nl,nl,
    write('Just remember that this is not the evalution of the current move, is the evaluation of the best reachable move starting from the one the AI has already executed, this because the minmax algorithm perform the static evaluation (how much a move is good or bad) looking at the last frontier of reachable moves for a given set of tetraminoes.').

writeClearedRows(0) :- !.

writeClearedRows(1) :- 
    write('Allows to clear 1 row.'),nl,!.

writeClearedRows(ClRow) :- 
    write('Allows to clear '),write(ClRow),write(' rows.'),nl,!.