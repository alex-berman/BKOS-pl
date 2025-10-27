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
    format(user_error, '\nRunning test ~w\n\n', [Name]),
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
        nth1(N, Turns, Turn),
        format('Turn #~w: ~w\n', [N, Turn]),
        test_turn(Turn),
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


test_turn(Turn) :-
    get_dict('S', Turn, Content),
    !,
    test_system_turn(Content).

test_turn(Turn) :-
    get_dict('U', Turn, Content),
    !,
    test_user_turn(Content).

test_turn(_, Turn, _) :-
    throw(unsupported_turn_format(Turn)).


test_system_turn(_{}) :-
    !,
    apply_rules,
    assertion(\+ @utter(_)).

test_system_turn(TurnDict) :-
    is_dict(TurnDict),
    get_dict(move, TurnDict, ExpectedSystemMoveAtoms),
    is_list(ExpectedSystemMoveAtoms),
    !,
    maplist({}/[A,T]>>atom_to_term(A,T,_), ExpectedSystemMoveAtoms, ExpectedSystemMove),
    expect_system_move(ExpectedSystemMove).
    
test_system_turn(ExpectedSystemMoveAtom) :-
    atom_to_term(ExpectedSystemMoveAtom, ExpectedSystemMove, _),
    expect_system_move(ExpectedSystemMove).

expect_system_move(ExpectedSystemMove) :-
    apply_rules,
    assertion(@utter(_)),
    @utter(ActualSystemMove),
    assertion(ActualSystemMove =@= ExpectedSystemMove),
    retract(@utter(_)).


test_user_turn(TurnDict) :-
    is_dict(TurnDict),
    !,
    ( get_dict(unresolvable_phrase, TurnDict, Phrase) ->
        asserta(@recognized(unresolvable_phrase(Phrase)))
    ; true ),
    ( get_dict(move, TurnDict, MoveAtom) ->
        atom_to_term(MoveAtom, Move, _),
        asserta(@recognized(move(Move)))
    ; true ),
    ( get_dict(presuppositions, TurnDict, PresuppositionAtoms) ->
        forall(
            member(PresuppositionAtom, PresuppositionAtoms),
            ( atom_to_term(PresuppositionAtom, Presupposition, _),
                asserta(@recognized(presupposition(Presupposition))) ))
    ; true ).

test_user_turn(MoveAtom) :-
    atom_to_term(MoveAtom, Move, _),
    asserta(@recognized(move(Move))).
