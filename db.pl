:- module(db, [create_empty_state/1, fork_state/2, db_add/2, db_remove/2, db_remove_all/2, db_get/2, '@'/1, set_current_state/1]).
:- ensure_loaded(isu_syntax).

:- dynamic fact/2, current_state/1.


create_empty_state(ID) :-
    gensym(s_, ID).


fork_state(ParentID, ChildID) :-
    atom_concat(ParentID, '_', Prefix),
    gensym(Prefix, ChildID),
    forall(fact(ParentID, Fact), assertz(fact(ChildID, Fact))).


db_add(StateID, Fact) :-
    assertz(fact(StateID, Fact)).


db_remove(StateID, Fact) :-
    retract(fact(StateID, Fact)).


db_remove_all(StateID, Fact) :-
    retractall(fact(StateID, Fact)).


db_get(StateID, Fact) :-
    fact(StateID, Fact).


'@'(Fact) :-
    current_state(StateID),
    fact(StateID, Fact).


set_current_state(ID) :-
    retractall(current_state(_)),
    assertz(current_state(ID)).
