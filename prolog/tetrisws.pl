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

%ocell/R/C (assert an occupied cell)
:- http_handler(root(ocell/R/C), reqOccCell(R,C), []).

reqOccCell(R,C,_Request) :-
    cors_enable,
    atom_number(R, R1),
    atom_number(C, C1),
    assertz(occCell(R1,C1)),
    reply_json_dict('ok').

%retcell(retract all the occupied cell)
:- http_handler(root(retcell), retractCell, []).

retractCell(_Request) :-
    cors_enable,
    retractall(occCell(_,_)),
    reply_json_dict('ok').

%newgb(compute a new gb after row cleaning)
:- http_handler(root(newgb), newGb, []).

newGb(_Request) :-
    cors_enable,
    computeNewGameBoard,
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

%path find the best path for the tetramino
:- http_handler(root(path/Player), path(Player), []).

path(Player,_Request) :-
    cors_enable,
    getPathOfBestMove(Player,Plan),
    reply_json_dict(Plan).
%