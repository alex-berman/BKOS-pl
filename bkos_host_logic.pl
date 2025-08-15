:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).

response(Q, Move) :-
	relevant_answer(Q, P),
	@P,
	answer_move(Q, P, Move).


relevant_answer(P, P).

relevant_answer(P, not(P)).

relevant_answer([_, _]^supports(_, X, _), E) :-
	@supports(E, X, _).

relevant_answer(_^P, P).


answer_move(P, P, confirm(P)).

answer_move(P, not(P), disconfirm(not(P))).

answer_move(_^_, P, assert(P)).
