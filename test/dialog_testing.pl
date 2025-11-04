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
    StateID = Name,
    ( get_dict(facts, Test, _) ->
        assert_fact(StateID, Test.facts)
    ; true ),
    test_turns(StateID, Test.turns, 1).


test_turns(StateID, Turns, N) :-
    length(Turns, NumTurns),
    ( N =< NumTurns ->
        nth1(N, Turns, Turn),
        format('Turn #~w: ~w\n', [N, Turn]),
        test_turn(StateID, Turn),
        NextN is N + 1,
        test_turns(StateID, Turns, NextN)
    ;
        true
    ).


assert_fact(StateID, Strs) :-
    is_list(Strs),
    !,
    reverse(Strs, StrsReversed),
    forall(member(Str, StrsReversed), assert_fact(StateID, Str)).

assert_fact(StateID, Str) :-
    term_string(Fact, Str),
    db_add(StateID, Fact).


test_turn(StateID, Turn) :-
    get_dict('S', Turn, Content),
    !,
    test_system_turn(StateID, Content).

test_turn(StateID, Turn) :-
    get_dict('U', Turn, Content),
    !,
    test_user_turn(StateID, Content).

test_turn(_, Turn) :-
    throw(unsupported_turn_format(Turn)).


test_system_turn(StateID, _{}) :-
    !,
    apply_rules(StateID),
    assertion(\+ db_get(StateID, utter(_))).

test_system_turn(StateID, TurnDict) :-
    is_dict(TurnDict),
    get_dict(move, TurnDict, ExpectedSystemMoveAtoms),
    is_list(ExpectedSystemMoveAtoms),
    !,
    maplist({}/[A,T]>>atom_to_term(A,T,_), ExpectedSystemMoveAtoms, ExpectedSystemMove),
    expect_system_move(StateID, ExpectedSystemMove).
    
test_system_turn(StateID, ExpectedSystemMoveAtom) :-
    atom_to_term(ExpectedSystemMoveAtom, ExpectedSystemMove, _),
    expect_system_move(StateID, ExpectedSystemMove).

expect_system_move(StateID, ExpectedSystemMove) :-
    apply_rules(StateID),
    assertion(db_get(StateID, utter(_))),
    db_get(StateID, utter(ActualSystemMove)),
    assertion(ActualSystemMove =@= ExpectedSystemMove),
    db_remove(StateID, utter(_)), !.


test_user_turn(StateID, TurnDict) :-
    is_dict(TurnDict),
    !,
    ( get_dict(unresolvable_phrase, TurnDict, Phrase) ->
        db_add(StateID, recognized(unresolvable_phrase(Phrase)))
    ; true ),
    ( get_dict(move, TurnDict, MoveAtom) ->
        atom_to_term(MoveAtom, Move, _),
        db_add(StateID, recognized(move(Move)))
    ; true ),
    ( get_dict(presuppositions, TurnDict, PresuppositionAtoms) ->
        forall(
            member(PresuppositionAtom, PresuppositionAtoms),
            ( atom_to_term(PresuppositionAtom, Presupposition, _),
                db_add(StateID, recognized(presupposition(Presupposition))) ))
    ; true ).

test_user_turn(StateID, MoveAtom) :-
    atom_to_term(MoveAtom, Move, _),
    db_add(StateID, recognized(move(Move))).
