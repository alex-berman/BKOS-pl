:- module(dialog_testing, [get_test/2, run_test/1]).
:- use_module(db).
:- use_module(library(yaml)).
:- use_module(isu_engine).
:- ensure_loaded(isu_syntax).

get_test(Name:Test, TestsPath) :-
    yaml_read(TestsPath, TestsDict),
    get_dict(Name, TestsDict, Test).

run_test(Name:Test) :-
    write('\nRunning test '), write(Name), nl, nl,
    clear_facts,
    assert_initial_facts,
    ( get_dict(facts, Test, _) ->
        forall(
            member(FactStr, Test.facts),
            (
                term_string(Fact, FactStr),
                assert(@Fact)
            ))
        ; true ),
    test_turns(Test.turns).
    
test_turns([]) :- !.

test_turns([TurnStr|TurnsTail]) :-
    write('Turn='), write(TurnStr), nl,
    test_turn(TurnStr),
    test_turns(TurnsTail).

test_turn("S") :-
    !,
    apply_rules_exhaustively,
    assertion(\+ @utter(_)).

test_turn(TurnStr) :-
    atom_concat("S ", ExpectedSystemMoveAtom, TurnStr),
    !,
    test_system_turn(ExpectedSystemMoveAtom).

test_turn(TurnStr) :-
    atom_concat("U ", UserMoveAtom, TurnStr),
    !,
    test_user_turn(UserMoveAtom).

test_turn(TurnStr) :-
    throw(unsupported_turn_format(TurnStr)).

test_system_turn(ExpectedSystemMoveAtom) :-
    atom_to_term(ExpectedSystemMoveAtom, ExpectedSystemMove, _),
    apply_rules_exhaustively,
    assertion(@utter(_)),
    @utter(ActualSystemMove),
    assertion(ActualSystemMove = ExpectedSystemMove),
    retract(@utter(_)).

test_user_turn(UserMoveAtom) :-
    atom_to_term(UserMoveAtom, UserMove, _),
    assert(@heard(UserMove)).
