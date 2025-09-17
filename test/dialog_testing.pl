:- module(dialog_testing, [get_test/2, run_test_from_dict/1, run_test/2]).
:- use_module(db).
:- use_module(library(yaml)).
:- use_module(isu_engine).
:- ensure_loaded(isu_syntax).


get_test(Name:Test, TestsPath) :-
    yaml_read(TestsPath, TestsDict),
    get_dict(Name, TestsDict, Test),
    get_dict(turns, Test, _).


run_test(TestsPath, Name) :-
    yaml_read(TestsPath, TestsDict),
    get_dict(Name, TestsDict, Test),
    run_test_from_dict(Name:Test).


run_test_from_dict(Name:Test) :-
    write('\nRunning test '), write(Name), nl, nl,
    clear_facts,
    ( get_dict(facts, Test, _) ->
        forall(
            member(FactStr, Test.facts),
            assert_fact(FactStr))
        ; true
    ),
    test_turns(Test.turns, 1).


test_turns(Turns, N) :-
    length(Turns, NumTurns),
    ( N =< NumTurns ->
        nth1(N, Turns, TurnStr),
        format('Turn #~w: ~w\n', [N, TurnStr]),
        test_turn(TurnStr),
        NextN is N + 1,
        test_turns(Turns, NextN)
    ;
        true
    ).


assert_fact(Strs) :-
    is_list(Strs),
    !,
    forall(member(Str, Strs), assert_fact(Str)).

assert_fact(Str) :-
    term_string(Fact, Str),
    assert(@Fact).


test_turn("S") :-
    !,
    apply_rules_exhaustively,
    assertion(\+ @utter(_)).

test_turn(TurnStr) :-
    atom_concat("S ", ExpectedSystemMoveAtom, TurnStr),
    !,
    test_system_turn(ExpectedSystemMoveAtom).

test_turn(TurnStr) :-
    atom_concat("U ", InterpretationAtom, TurnStr),
    !,
    test_user_turn(InterpretationAtom).

test_turn(TurnStr) :-
    throw(unsupported_turn_format(TurnStr)).


test_system_turn(ExpectedSystemMoveAtom) :-
    atom_to_term(ExpectedSystemMoveAtom, ExpectedSystemMove, _),
    apply_rules_exhaustively,
    assertion(@utter(_)),
    @utter(ActualSystemMove),
    assertion(ActualSystemMove =@= ExpectedSystemMove),
    retract(@utter(_)).


test_user_turn(InterpretationAtom) :-
    atom_to_term(InterpretationAtom, Interpretation, _),
    interpretation_as_dict(Interpretation, InterpretationAsDict),
    assert(@heard(InterpretationAsDict)).


interpretation_as_dict(Dict, Dict) :-
    is_dict(Dict),
    !.

interpretation_as_dict(Move, _{move:Move}).
