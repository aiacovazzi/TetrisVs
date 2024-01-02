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
   explanation(FullNodeStory,_),
   nodeStoryInListForm(FullNodeStory,FullNodeStoryList),
   reply_json_dict(FullNodeStoryList).

nodeStoryInListForm([], []).
nodeStoryInListForm([(T,R,C)|FullNodeStoryTail], [[T,R,C]|FullNodeStoryListTail]) :-
    nodeStoryInListForm(FullNodeStoryTail,FullNodeStoryListTail).

%explainPath (return the asserted explanation about move)
:- http_handler(root(explainPath), explainPath, []).
explainPath(_Request) :-
    cors_enable,
    explanation(_,PathStory),
    format('Content-type: text/plain~n~n'),
    write('This shows how the path has been found. When a step is followed by a shorter step it indicates a backtraking point where a point of failure occurred.'),nl,
    writePath(PathStory).

writePath([]).
writePath([Path|PathStoryTail]) :-
    write(Path),nl,
    writePath(PathStoryTail).