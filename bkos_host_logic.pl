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

valid_answer(Vars>>supports(E, prob(Event, _), M), A) :-
	member(R, [low, high]),
	valid_answer(Vars>>supports(E, rel_prob(Event, R), M), A).

valid_answer([M]>>P, P) :-
	P = supports(_, _, M),
	@P.

valid_answer([_]>>P, P) :-
	P \= supports(_, _, _),
	@P.


has_variable_and_body(Vars>>Body, Var, Body) :-
	member(Var1, Vars),
	Var1 == Var.


supports_directly_or_indirectly(D, C) :-
	@supports(A, C, _),
	@A,
	(
		D = A
	;
		A = rel_value(P, _),
		D = value(P, _),
		@D
	).

supports_directly_or_indirectly(A, C) :-
	@supports(A1, C, _),
	@A1,
	supports_directly_or_indirectly(A, A1).

supports_directly_or_indirectly(A, C) :-
	C = rel_prob(Event, moderate),
	@C,
	findall(E, (
		member(R, [high, low]),
		supports_directly_or_indirectly(E, rel_prob(Event, R))
		), Evidence),
	member(A, Evidence).


answer_move(Vars>>supports(E, C, M), As, Move) :-
	member(C, [rel_prob(P, moderate), prob(P, _)]),
	findall(
		infer(NormalizedEvidence, rel_prob(P, R)),
		(
			member(R, [high, low]),
			findall(A,
				(member(A, As), valid_answer(Vars>>supports(E, rel_prob(P, R), M), A)),
				Evidence),
			Evidence \== [],
			normalize(Evidence, NormalizedEvidence)
		),
		Inferences),
	Inferences \== [],
	normalize(Inferences, Move).

answer_move([]>>P, [P], confirm(P)).

answer_move([]>>P, [not(P)], disconfirm(not(P))).

answer_move([]>>P, [rel_prob(P, high)], confirm(rel_prob(P, high))).

answer_move([]>>P, [rel_prob(P, low)], disconfirm(rel_prob(P, low))).

answer_move(_, Ps, assert(P)) :-
	normalize(Ps, P).


normalize([X], X) :- !.

normalize(X, X).


select_answers(Q, Candidates, Result) :-
	(@answer_selection_policy(Q, Candidates, Selected, Condition) ->
		Condition,
		intersection(Candidates, Selected, Result)
	;
		Result = Candidates
	).


contradicts(rel_prob(Event, X), rel_prob(Event, Y)) :-
	X \== Y.

contradicts(rel_value(Property, X), rel_value(Property, Y)) :-
	X \== Y.
	  