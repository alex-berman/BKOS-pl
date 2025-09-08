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
        nth1(N, Turns, Turn),
        format('Turn #~w: ~w\n', [N, Turn]),
        test_turn(StateID, Turn, NextStateID),
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


test_turn(StateID, Turn, NextStateID) :-
    get_dict('S', Turn, Content),
    !,
    test_system_turn(StateID, Content, NextStateID).

test_turn(StateID, Turn, StateID) :-
    get_dict('U', Turn, Content),
    !,
    test_user_turn(StateID, Content).

test_turn(_, Turn, _) :-
    throw(unsupported_turn_format(Turn)).



test_system_turn(StateID, ExpectedOutputAsDict, NextStateID) :-
    is_dict(ExpectedOutputAsDict),
    !,
    get_dict(one_of, ExpectedOutputAsDict, ExpectedSystemMoveAtoms),
    setof(Move, (member(Atom, ExpectedSystemMoveAtoms), atom_to_term(Atom, Move, _)), ExpectedSystemMoves),
    apply_rules(StateID, CandidateNextStateIDs),
    setof(Move, (member(CandidateNextStateID, CandidateNextStateIDs), db_get(CandidateNextStateID, utter(Move))), ActualSystemMoves),
    assertion(sets_are_structurally_equal(ActualSystemMoves, ExpectedSystemMoves)),
    CandidateNextStateIDs = [NextStateID|_].

test_system_turn(StateID, ExpectedSystemMoveAtom, NextStateID) :-
    atom_to_term(ExpectedSystemMoveAtom, ExpectedSystemMove, _),
    apply_rules(StateID, CandidateNextStateIDs),
    GetState = (
        member(NextStateID, CandidateNextStateIDs),
        db_get(NextStateID, utter(ExpectedSystemMove))
    ),
    assertion(GetState),
    GetState,
    db_remove(NextStateID, utter(_)).


sets_are_structurally_equal(Xs, Ys) :-
    subset_structural(Xs, Ys),
    subset_structural(Ys, Xs).


subset_structural([], _).

subset_structural([E|Es], Ys) :-
    memberchk_structural(E, Ys),
    subset_structural(Es, Ys).


memberchk_structural(E, [Y|_]) :-
    E =@= Y, !.

memberchk_structural(E, [_|Ys]) :-
    memberchk_structural(E, Ys).


test_user_turn(StateID, InterpretationAtom) :-
    atom_to_term(InterpretationAtom, Interpretation, _),
    interpretation_as_dict(Interpretation, InterpretationAsDict),
    db_add(StateID, heard(InterpretationAsDict)).


interpretation_as_dict(Dict, Dict) :-
    is_dict(Dict),
    !.

interpretation_as_dict(Move, interpretation{move:Move}).
