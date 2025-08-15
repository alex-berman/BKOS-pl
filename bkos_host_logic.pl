:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).

response(Q, Move) :-
	relevant_answer(Q, P),
	@P,
	( evidence_strategy(Q, Strategy) ->
		satisfies_evidence_strategy(P, Strategy)
	; true ),
	answer_move(Q, P, Move).


relevant_answer(P, P).

relevant_answer(P, not(P)).

relevant_answer([_, _]^supports(_, X, _), E) :-
	@supports(E, X, _).

relevant_answer(_^P, P).


evidence_strategy(_^supports(_, P, _), Strategy) :-
	relevant_answer(XQ, P),
	( @evidence_strategy(XQ, Strategy) ->
		true
	; Strategy = datum ).


satisfies_evidence_strategy(P, datum) :-
	P \= supports(_, _, _).

satisfies_evidence_strategy(supports(_, _, _), warrant).


answer_move(P, P, confirm(P)).

answer_move(P, not(P), disconfirm(not(P))).

answer_move(_^_, P, assert(P)).
