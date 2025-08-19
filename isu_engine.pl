:- module(engine, [assert_initial_facts/0, apply_rules_exhaustively/0, '@'/1, clear_facts/0]).
:- use_module(db).
:- ensure_loaded(isu_syntax).

:- dynamic user:(::)/2.
:- multifile user:(::)/2.

assert_initial_facts :-
    forall((user:(_ :: Term), Term \= (_ -* _)),
	   assert(@Term)).

apply_rules_exhaustively :-
    repeat_apply_until_nothing_applied,
    !.

repeat_apply_until_nothing_applied :-
    print_state,
    bagof(
        user:(RuleName :: Antecedent -* Consequent),
        user:(RuleName :: Antecedent -* Consequent),
        Rules),
    repeat,
    apply_and_count(Rules, N),
    N == 0. % repeat if at least one rule applied

apply_and_count([], 0).
apply_and_count([user:(RuleName :: Antecedent -* Consequent)|Rules], N) :-
    user:(RuleName :: Antecedent -* Consequent),
    ( antecedent_holds(Antecedent) ->
      potentially_consume(Antecedent),
      establish(Consequent),
      write('Applied: '), write(RuleName), nl,
      print_state,
      ThisN = 1
    ; ThisN = 0),
    apply_and_count(Rules, RestN),
    N is ThisN + RestN.

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
antecedent_holds(^Proposition) :- % premise is to be reproduced (corresponds to -* in ProLin)
    !,
    @Proposition.
antecedent_holds(?Proposition) :- % check if proposition is non-unique (roughly corresponds to ?-* in ProLin)
    !,
    setof(Proposition, @Proposition, Solutions),
    length(Solutions, N),
    N >= 2.
antecedent_holds(!Proposition) :- % check if proposition is unique (roughly corresponds to !-* in ProLin)
    !,
    setof(Proposition, @Proposition, Solutions),
    length(Solutions, N),
    N == 1,
    Solutions = [Proposition]. % unify with the unique solution
antecedent_holds(\+P) :- % check if proposition does NOT hold
    !,
    \+ @P.
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
potentially_consume(?_) :- !. % don't consume content of non-uniqueness test
potentially_consume(!_) :- !. % don't consume content of uniqueness test
potentially_consume($_) :- !. % don't consume Prolog-native conditions
potentially_consume(\+_) :- !. % don't consume negation test
potentially_consume(Proposition) :-
    retract(@Proposition).

establish([]) :- !.
establish([Head|Tail]) :-
    !,
    establish(Head),
    establish(Tail).
establish(Proposition) :-
    assert(@Proposition).

clear_facts :-
    retractall(@_).
