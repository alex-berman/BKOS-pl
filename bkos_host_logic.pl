:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).


valid_answer([]>>P, A) :-
	( A = P ; A = not(P) ),
	@A.

valid_answer([]>>P, relative_prob(P, R)) :-
	@relative_prob(P, R).

valid_answer(Vars>>Body, A) :-
	Body = supports(E, Consequent, _),
    contains_variable(Vars, E),
	(
		supports_directly_or_indirectly(Evidence, Consequent),
		@Evidence,
		A = Evidence
	;
		SupportsFact = supports(SupportingAntecedent, _, _),
		@SupportsFact,
		matches_fact(SupportingAntecedent),
		unifiable(SupportsFact, supports(_, Consequent, _), _),
		A = SupportsFact
	).

valid_answer(Vars>>Body, Consequent) :-
	Body = supports(Evidence, C, _),
    contains_variable(Vars, C),
	supports_directly_or_indirectly(Evidence, Consequent).

valid_answer([M]>>Body, SupportsFact) :-
	Body = supports(_, _, M),
	SupportsFact = supports(_, _, M),
	@SupportsFact,
	unifiable(SupportsFact, Body, _).

valid_answer([_]>>P, A) :-
	P \= supports(_, _, _),
	copy_term(P, A),
	@A.


contains_variable(Vars, Var) :-
	member(Var1, Vars),
	Var1 == Var.


supports_directly_or_indirectly(A, C) :-
	@supports(A, C, _).

supports_directly_or_indirectly(A, C) :-
	@supports(A1, C, _),
	supports_directly_or_indirectly(A, A1).

supports_directly_or_indirectly(A, C) :-
	C = relative_prob(Event, moderate),
	@C,
	findall(E, supports_directly_or_indirectly(E, relative_prob(Event, high)), PosEvidences),
	findall(E, supports_directly_or_indirectly(E, relative_prob(Event, low)), NegEvidences),
	PosEvidences \== [],
	NegEvidences \== [],
	(member(A, PosEvidences) ; member(A, NegEvidences)).


matches_fact(P) :-
	@P1,
	unifiable(P, P1, _).


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

answer_move(_>>_, P, assert(P)).


select_answers(Q, Candidates, Result) :-
	(@(select_answers(Q, Candidates, Selected) :- Condition) ->
		Condition,
		findall(A, (member(A, Selected), member(A, Candidates)), Result)
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

contradicts(relative_value(Feature, Value1), relative_value(Feature, Value2)) :-
	Value1 \== Value2.
