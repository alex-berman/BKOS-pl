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
    create_empty_state(StateID),
    assert_initial_facts(StateID),
    ( get_dict(facts, Test, _) ->
        forall(
            member(FactStr, Test.facts),
            assert_fact(StateID, FactStr))
        ; true
    ),
    test_turns(StateID, Test.turns, 1),
    !.


test_turns(StateID, Turns, N) :-
    length(Turns, NumTurns),
    ( N =< NumTurns ->
        nth1(N, Turns, TurnStr),
        format('Turn #~w: ~w\n', [N, TurnStr]),
        test_turn(StateID, TurnStr, NextStateID),
        NextN is N + 1,
        test_turns(NextStateID, Turns, NextN)
    ;
        true
    ).


assert_fact(StateID, Strs) :-
    is_list(Strs),
    !,
    forall(member(Str, Strs), assert_fact(StateID, Str)).

assert_fact(StateID, Str) :-
    term_string(Fact, Str),
    db_add(StateID, Fact).


test_turn(StateID, "S", NextStateID) :-
    !,
    get_next_state(StateID, NextStateID),
    assertion(\+ db_get(NextStateID, utter(_))).

test_turn(StateID, TurnStr, NextStateID) :-
    atom_concat("S ", ExpectedSystemMoveAtom, TurnStr),
    !,
    test_system_turn(StateID, ExpectedSystemMoveAtom, NextStateID).

test_turn(StateID, TurnStr, StateID) :-
    atom_concat("U ", InterpretationAtom, TurnStr),
    !,
    test_user_turn(StateID, InterpretationAtom).

test_turn(_, TurnStr) :-
    throw(unsupported_turn_format(TurnStr)).


get_next_state(StateID, NextStateID) :-
    apply_rules(StateID, CandidateNextStateIDs),
    select_next_state(CandidateNextStateIDs, NextStateID).


select_next_state([], _) :-
    throw(no_next_state),
    !.

select_next_state([NextStateID], NextStateID) :- !.

select_next_state(CandidateNextStateIDs, _) :-
    throw(ambiguous_next_state(CandidateNextStateIDs)).


test_system_turn(StateID, ExpectedSystemMoveAtom, NextStateID) :-
    atom_to_term(ExpectedSystemMoveAtom, ExpectedSystemMove, _),
    get_next_state(StateID, NextStateID),
    assertion(db_get(NextStateID, utter(_))),
    db_get(NextStateID, utter(ActualSystemMove)),
    assertion(ActualSystemMove =@= ExpectedSystemMove),
    db_remove(NextStateID, utter(_)).


test_user_turn(StateID, InterpretationAtom) :-
    atom_to_term(InterpretationAtom, Interpretation, _),
    interpretation_as_dict(Interpretation, InterpretationAsDict),
    db_add(StateID, heard(InterpretationAsDict)).


interpretation_as_dict(Dict, Dict) :-
    is_dict(Dict),
    !.

interpretation_as_dict(Move, interpretation{move:Move}).
