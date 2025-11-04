/*
Example:

curl -X POST http://localhost:8080/interact \
     -H "Content-Type: application/json" \
     -d '{"start_session":{}}'

returns

{"response":"assert(rel_prob(satisfied(pat_1),high))"}


curl -X POST http://localhost:8080/interact \
     -H "Content-Type: application/json" \
     -d '{"move":"ask([]>>satisfied(pat_1))"}'

returns

{"response":"confirm(rel_prob(satisfied(pat_1),high))"}
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(yaml)).
:- use_module(isu_engine).
:- ensure_loaded(bkos).

:- http_handler(root(interact), handle_interact, []).

:- initialization(main, main).

main :-
    http_server(http_dispatch, [port(8080)]),
    thread_get_message(stop).

load_initial_state :-
    yaml_read('initial_state.yml', InitialStateDict),
    forall(
        member(FactStr, InitialStateDict.facts),
        assert_fact(FactStr)).

assert_fact(Str) :-
    term_string(Fact, Str),
    assert(@Fact).

handle_interact(Request) :-
    http_read_json_dict(Request, Input),
    with_output_to(string(Output), process_input(Input, ResponseTerm)),
    term_string(ResponseTerm, Response),
    reply_json_dict(_{response:Response}),
    write(Output).

process_input(Input, Response) :-
    ( get_dict(start_session, Input, _) ->
        load_initial_state
    ; true ),
    ( get_dict(unresolvable_phrase, Input, Phrase) ->
        asserta(@recognized(unresolvable_phrase(Phrase)))
    ; true ),
    ( get_dict(move, Input, MoveString) ->
        term_string(Move, MoveString),
        asserta(@recognized(move(Move)))
    ; true ),
    ( get_dict(presuppositions, Input, Presuppositions) ->
        forall(member(PresuppositionString, Presuppositions),
            (
                term_string(Presupposition, PresuppositionString),
                asserta(@recognized(presupposition(Presupposition)))
            ))
    ; true ),
    apply_rules,
    ( retract(@utter(Response)) -> true ; Response = none ).
