:- module(engine, [apply_rules/0, '@'/1, clear_facts/0]).
:- use_module(db).
:- ensure_loaded(isu_syntax).

:- dynamic user:(::)/2.
:- multifile user:(::)/2.


apply_rules :-
    print_state,
    forall(
        user:(RuleName :: Antecedent -* Consequent),
        ( antecedent_holds(Antecedent) ->
            potentially_consume(Antecedent),
            establish(Consequent),
            write('Applied: '), write(RuleName), nl,
            print_state
        ;
            true
        )).


print_state :-
    write('State:\n'),
    forall(@Fact, (
        write('  '),
        numbervars(Fact),
        write(Fact),
        nl
    )),
    nl.


antecedent_holds([]) :- !.

antecedent_holds([Head|Tail]) :-
    !,
    antecedent_holds(Head),
    antecedent_holds(Tail).

antecedent_holds(^Proposition) :- % premise is to be reproduced
    !,
    @Proposition.

antecedent_holds(\+P) :- % check if proposition does NOT hold
    !,
    \+ @P.

antecedent_holds(*_). % consume all matching premises (antecedent always true)

antecedent_holds($Condition) :-
    !,
    Condition.

antecedent_holds(Proposition) :-
    @Proposition.


potentially_consume([]) :- !.

potentially_consume([Head|Tail]) :-
    !,
    potentially_consume(Head),
    potentially_consume(Tail).

potentially_consume(^_) :- !. % don't consume premises that are reproduced

potentially_consume($_) :- !. % don't consume Prolog-native conditions

potentially_consume(\+_) :- !. % don't consume negation test

potentially_consume(*Pattern) :- % consume all matching premises
    retractall(@Pattern).

potentially_consume(Proposition) :-
    retract(@Proposition).


establish([]) :- !.

establish([Head|Tail]) :-
    !,
    establish(Head),
    establish(Tail).

establish(Proposition) :-
    asserta(@Proposition).


clear_facts :-
    retractall(@_).
