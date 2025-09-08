:- module(engine, [assert_initial_facts/1, apply_rules/2, '@'/1, clear_facts/0]).
:- use_module(db).
:- ensure_loaded(isu_syntax).

:- dynamic user:(::)/2.
:- multifile user:(::)/2.


assert_initial_facts(State) :-
    forall((user:(_ :: Term), Term \= (_ -* _)),
	   db_add(State, Term)).


apply_rules(State, CandidateNextStates) :-
    print_state(State),
    bagof(
        user:(RuleName :: Antecedent -* Consequent),
        user:(RuleName :: Antecedent -* Consequent),
        Rules),
    apply_rules(State, Rules, CandidateNextStates).


apply_rules(State, [user:(RuleName :: Antecedent -* Consequent)|Rules], Result) :-
    %format('~w: testing ~w\n', [State, RuleName]),
    set_current_state(State),
    ( setof(Consequent, antecedent_holds(State, Antecedent), Solutions) ->
        %format('Solutions=~w\n', [Solutions]),
        ( Solutions = [Solution] ->
            potentially_consume(State, Antecedent),
            establish(State, Solution),
            write('Applied: '), write(RuleName), nl,
            print_state(State),
            apply_rules(State, Rules, Result)
        ;
            findall(
                SolutionStates,
                (
                    member(Solution, Solutions),
                    fork_state(State, CandidateNextState),
                    set_current_state(CandidateNextState),
                    potentially_consume(CandidateNextState, Antecedent),
                    establish(CandidateNextState, Solution),
                    write('Applied: '), write(RuleName), nl,
                    print_state(CandidateNextState),
                    apply_rules(CandidateNextState, Rules, SolutionStates)
                ),
                Results
            ),
            append(Results, Result)
        )
    ;
        apply_rules(State, Rules, Result)
    ).

apply_rules(State, [], [State]).


print_state(State) :-
    format('~w:\n', [State]),
    forall(db_get(State, Fact), (
        write('  '),
        numbervars(Fact),
        write(Fact),
        nl
    )),
    nl.


antecedent_holds(_, []) :- !.

antecedent_holds(State, [Head|Tail]) :-
    !,
    antecedent_holds(State, Head),
    antecedent_holds(State, Tail).

antecedent_holds(State, ^Proposition) :- % premise is to be reproduced (corresponds to -* in ProLin)
    !,
    db_get(State, Proposition).

antecedent_holds(State, ?Proposition) :- % check if proposition is non-unique (roughly corresponds to ?-* in ProLin)
    !,
    setof(Proposition, db_get(State, Proposition), Solutions),
    length(Solutions, N),
    N >= 2.

antecedent_holds(State, !Proposition) :- % check if proposition is unique (roughly corresponds to !-* in ProLin)
    !,
    setof(Proposition, db_get(State, Proposition), Solutions),
    length(Solutions, N),
    N == 1,
    Solutions = [Proposition]. % unify with the unique solution

antecedent_holds(State, \+P) :- % check if proposition does NOT hold
    !,
    \+ db_get(State, P).

antecedent_holds(_, *_). % consume all matching premises (antecedent always true)

antecedent_holds(_, $Condition) :-
    !,
    Condition.

antecedent_holds(State, Fact) :-
    db_get(State, Fact).


potentially_consume(_, []) :- !.

potentially_consume(State, [Head|Tail]) :-
    !,
    potentially_consume(State, Head),
    potentially_consume(State, Tail).

potentially_consume(_, ^_) :- !. % don't consume premises that are reproduced

potentially_consume(_, ?_) :- !. % don't consume content of non-uniqueness test

potentially_consume(_, !_) :- !. % don't consume content of uniqueness test

potentially_consume(_, $_) :- !. % don't consume Prolog-native conditions

potentially_consume(_, \+_) :- !. % don't consume negation test

potentially_consume(State, *Pattern) :- % consume all matching premises
    db_remove_all(State, Pattern).

potentially_consume(State, Proposition) :-
    db_remove(State, Proposition).


establish(_, []) :- !.

establish(State, [Head|Tail]) :-
    !,
    establish(State, Head),
    establish(State, Tail).

establish(State, Proposition) :-
    db_add(State, Proposition).


clear_facts :-
    db_remove_all(_).
