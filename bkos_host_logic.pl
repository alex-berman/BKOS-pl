:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).


valid_answer([]>>P, P) :-
	@P.

valid_answer([]>>P, not(P)) :-
	@not(P).

valid_answer([]>>P, rel_prob(P, R)) :-
	@rel_prob(P, R).

valid_answer(Vars>>supports(E, C, _), D) :-
    contains_variable(Vars, E),
	supports_directly_or_indirectly(D, C).

valid_answer(Vars>>supports(E, C, _), W) :-
    contains_variable(Vars, E),
	W = supports(WA, WC, _),
	@W,
	unifiable(WC, C, _),
	copy_term(WA, WA1),
	@WA1.

valid_answer(Vars>>supports(D, C, _), AC) :-
    contains_variable(Vars, C),
	supports_directly_or_indirectly(D, AC).

valid_answer([M]>>P, P) :-
	P = supports(_, _, M),
	@P.

valid_answer([_]>>P, P) :-
	P \= supports(_, _, _),
	@P.


contains_variable(Vars, Var) :-
	member(Var1, Vars),
	Var1 == Var.


supports_directly_or_indirectly(A, C) :-
	@supports(A, C, _),
	@A.

supports_directly_or_indirectly(A, C) :-
	@supports(A1, C, _),
	@A1,
	supports_directly_or_indirectly(A, A1).

supports_directly_or_indirectly(A, C) :-
	C = rel_prob(Event, moderate),
	@C,
	findall(E, supports_directly_or_indirectly(E, rel_prob(Event, high)), PosEvidences),
	findall(E, supports_directly_or_indirectly(E, rel_prob(Event, low)), NegEvidences),
	PosEvidences \== [],
	NegEvidences \== [],
	(member(A, PosEvidences) ; member(A, NegEvidences)).


answer_move(R, Ps, signal_continuation(M)) :-
	is_dict(R),
	get_dict(continuation, R, true),
	get_dict(q, R, Q),
	answer_move(Q, Ps, M).

answer_move(R, Ps, M) :-
	is_dict(R),
	get_dict(q, R, Q),
	answer_move(Q, Ps, M).

answer_move(Q, [P], M) :-
	!,
	answer_move(Q, P, M).

answer_move([]>>P, P, confirm(P)).

answer_move([]>>P, not(P), disconfirm(not(P))).

answer_move([]>>P, rel_prob(P, high), confirm(rel_prob(P, high))).

answer_move([]>>P, rel_prob(P, low), disconfirm(rel_prob(P, low))).

answer_move(_, P, assert(P)).



get_dict(Key, Dict, Value, Default) :-
	( get_dict(Key, Dict, Value) ->
		true
	;
		Value = Default
	).


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


compatible_with_facts(P) :-
	\+ (@Q, contradicts(Q, P)).


contradicts(not(P), P).

contradicts(P, not(P)).

contradicts(supports(P, Q1, How), supports(P, Q2, How)) :-
	ground(P),
	contradicts(Q1, Q2).

contradicts(rel_value(Feature, Value1), rel_value(Feature, Value2)) :-
	Value1 \== Value2.
