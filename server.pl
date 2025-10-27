/*
Example:

curl -X POST http://localhost:8080/interact \
     -H "Content-Type: application/json" \
     -d '_{move:ask([]>>satisfied(pat_1))}'

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
    load_initial_state,
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
    http_read_data(Request, Data, [to(string)]),
    catch(read_term_from_atom(Data, Input, []), E,
          ( print_message(error, E),
            fail )),
    with_output_to(string(Output),
        process_input(Input, ResponseTerm)),
    term_string(ResponseTerm, Response),
    reply_json_dict(_{response:Response}),
    write(Output).

process_input(Input, Response) :-
    ( get_dict(unresolvable_phrase, Input, Phrase) ->
        asserta(@recognized(unresolvable_phrase(Phrase)))
    ; true ),
    ( get_dict(move, Input, Move) ->
        asserta(@recognized(move(Move)))
    ; true ),
    ( get_dict(presuppositions, Input, Presuppositions) ->
        forall(
            member(Presupposition, Presuppositions),
            asserta(@recognized(presupposition(Presupposition))))
    ; true ),
    apply_rules,
    ( retract(@utter(Response)) -> true ; Response = none ).
