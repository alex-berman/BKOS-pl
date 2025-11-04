:- module(db, [db_add/2, db_remove/2, db_remove_all/2, db_get/2, '@'/1, set_current_state/1]).
:- ensure_loaded(isu_syntax).

:- dynamic fact/2, current_state/1, state/1.


db_add(StateID, Fact) :-
    asserta(fact(StateID, Fact)).


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
