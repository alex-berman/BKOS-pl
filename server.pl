/*
Example:

curl -X POST http://localhost:8080/interact \
     -H "Content-Type: application/json" \
     -d '{"state_id": 1, "start_session":{}}'

returns

{"response":"There is a high probability that the patient will be satisfied with surgery."}


curl -X POST http://localhost:8080/interact \
     -H "Content-Type: application/json" \
     -d '{"state_id": 1, "move":"ask([]>>satisfied(pat_1))"}'

returns

{"response":"Yes, there is a high probability that the patient will be satisfied with surgery."}
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(yaml)).
:- use_module(isu_engine).
:- use_module(nlg).
:- ensure_loaded(bkos).

:- http_handler(root(interact), handle_interact, []).

:- initialization(main, main).

main :-
    http_server(http_dispatch, [port(8080)]),
    thread_get_message(stop).

initialize_state(StateID) :-
    yaml_read('initial_state.yml', InitialStateDict),
    assert_fact(StateID, InitialStateDict.facts).

assert_fact(StateID, Strs) :-
    is_list(Strs),
    !,
    reverse(Strs, StrsReversed),
    forall(member(Str, StrsReversed), assert_fact(StateID, Str)).

assert_fact(StateID, Str) :-
    term_string(Fact, Str),
    db_add(StateID, Fact).

handle_interact(Request) :-
    http_read_json_dict(Request, Input),
    ( with_output_to(string(Output), process_input(Input, SystemMove)) ->
        generate(SystemMove, Utterance),
        term_string(SystemMove, SystemMoveString),
        reply_json_dict(_{move:SystemMoveString, utterance:Utterance}),
        write(Output)
    ;
        reply_json_dict(_{move:none})
    ).

process_input(Input, SystemMove) :-
    get_dict(state_id, Input, StateID),
    ( get_dict(start_session, Input, _) ->
        initialize_state(StateID)
    ; true ),
    ( get_dict(unresolvable_phrase, Input, Phrase) ->
        db_add(StateID, recognized(unresolvable_phrase(Phrase)))
    ; true ),
    ( get_dict(move, Input, MoveString) ->
        term_string(Move, MoveString),
        db_add(StateID, recognized(move(Move)))
    ; true ),
    ( get_dict(presuppositions, Input, Presuppositions) ->
        forall(member(PresuppositionString, Presuppositions),
            (
                term_string(Presupposition, PresuppositionString),
                db_add(StateID, recognized(presupposition(Presupposition)))
            ))
    ; true ),
    apply_rules(StateID),
    db_remove(StateID, utter(SystemMove)).
