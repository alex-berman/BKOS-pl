:- module(engine, [apply_rules/1, '@'/1]).
:- use_module(db).
:- ensure_loaded(isu_syntax).

:- dynamic user:(::)/2.
:- multifile user:(::)/2.


apply_rules(StateID) :-
    print_state(StateID),
    set_current_state(StateID),
    forall(
        user:(RuleName :: Antecedent -* Consequent),
        ( antecedent_holds(StateID, Antecedent) ->
            potentially_consume(StateID, Antecedent),
            establish(StateID, Consequent),
            write('Applied: '), write(RuleName), nl,
            print_state(StateID)
        ;
            true
        )).


print_state(StateID) :-
    write('State:\n'),
    forall(db_get(StateID, Fact), (
        write('  '),
        numbervars(Fact),
        write(Fact),
        nl
    )),
    nl.


antecedent_holds(_, []) :- !.

antecedent_holds(StateID, [Head|Tail]) :-
    !,
    antecedent_holds(StateID, Head),
    antecedent_holds(StateID, Tail).

antecedent_holds(StateID, ^Proposition) :- % premise is to be reproduced
    !,
    db_get(StateID, Proposition).

antecedent_holds(_, *_). % consume all matching premises (antecedent always true)

antecedent_holds(_, $Condition) :-
    !,
    Condition.

antecedent_holds(StateID, Proposition) :-
    db_get(StateID, Proposition).


potentially_consume(_, []) :- !.

potentially_consume(StateID, [Head|Tail]) :-
    !,
    potentially_consume(StateID, Head),
    potentially_consume(StateID, Tail).

potentially_consume(_, ^_) :- !. % don't consume premises that are reproduced

potentially_consume(_, $_) :- !. % don't consume Prolog-native conditions

potentially_consume(StateID, *Pattern) :- % consume all matching premises
    db_remove_all(StateID, Pattern).

potentially_consume(StateID, Proposition) :-
    db_remove(StateID, Proposition).


establish(_, []) :- !.

establish(StateID, [Head|Tail]) :-
    !,
    establish(StateID, Head),
    establish(StateID, Tail).

establish(StateID, Proposition) :-
    db_add(StateID, Proposition).
