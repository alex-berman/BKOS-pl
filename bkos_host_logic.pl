:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).


valid_answer([]>>P, P) :-
	@P.

valid_answer([]>>P, not(P)) :-
	@not(P).

valid_answer([]>>P, rel_prob(P, R)) :-
	@rel_prob(P, R).

valid_answer(Q, D) :-
	has_variable_and_body(Q, E, supports(E, C, _)),
	supports_directly_or_indirectly(D, C).

valid_answer(Q, W) :-
	has_variable_and_body(Q, E, supports(E, C, _)),
	W = supports(WA, WC, _),
	@W,
	unifiable(WC, C, _),
	copy_term(WA, WA1),
	@WA1.

valid_answer(Q, C) :-
    has_variable_and_body(Q, XC, supports(D, XC, _)),
	supports_directly_or_indirectly(D, C).

valid_answer([M]>>P, P) :-
	P = supports(_, _, M),
	@P.

valid_answer([_]>>P, P) :-
	P \= supports(_, _, _),
	@P.


has_variable_and_body(Vars>>Body, Var, Body) :-
	member(Var1, Vars),
	Var1 == Var.


supports_directly_or_indirectly(A, C) :-
	@supports(A, C, _),
	@A.

supports_directly_or_indirectly(A, C) :-
	@supports(A1, C, _),
	@A1,
	supports_directly_or_indirectly(A, A1).

supports_directly_or_indirectly(rel_value(FX, moderate), rel_prob(PX, moderate)) :-
	@rel_value(FX, moderate),
	FX =.. [_, X],
	PX =.. [_, X].

supports_directly_or_indirectly(A, C) :-
	C = rel_prob(Event, moderate),
	@C,
	findall(E, supports_directly_or_indirectly(E, rel_prob(Event, high)), PosEvidences),
	findall(E, supports_directly_or_indirectly(E, rel_prob(Event, low)), NegEvidences),
	PosEvidences \== [],
	NegEvidences \== [],
	(member(A, PosEvidences) ; member(A, NegEvidences)).


answer_move(Q, [P], M) :-
	!,
	answer_move(Q, P, M).

answer_move([]>>P, P, confirm(P)).

answer_move([]>>P, not(P), disconfirm(not(P))).

answer_move([]>>P, rel_prob(P, high), confirm(rel_prob(P, high))).

answer_move([]>>P, rel_prob(P, low), disconfirm(rel_prob(P, low))).

answer_move(_, P, assert(P)).



select_answers(Q, Candidates, Result) :-
	(@answer_selection_policy(Q, Candidates, Selected, Condition) ->
		Condition,
		intersection(Candidates, Selected, Result)
	;
		Result = Candidates
	).


has_responded(Q, P) :-
	@responded(Q, Ps),
	member(P, Ps).


contradicts(rel_prob(Event, X), rel_prob(Event, Y)) :-
	X \== Y.

contradicts(rel_value(Property, X), rel_value(Property, Y)) :-
	X \== Y.
	  