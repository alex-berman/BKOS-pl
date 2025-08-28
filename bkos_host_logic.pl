:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).


valid_answer([]>>P, A) :-
	( A = P ; A = not(P) ),
	belief(A).

valid_answer(Vars>>Body, A) :-
	Body = supports(E, Consequent, _),
    contains_variable(Vars, E),
	(
		supports_directly_or_indirectly(Evidence, Consequent),
		belief(Evidence),
		A = Evidence
	;
		SupportsFact = supports(_, _, _),
		belief(SupportsFact),
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
	belief(SupportsFact),
	unifiable(SupportsFact, Body, _).

valid_answer([_]>>P, A) :-
	P \= supports(_, _, _),
	copy_term(P, A),
	belief(A).


belief(P) :-
	(@P ; @confidence(P, _)).


contains_variable(Vars, Var) :-
	member(Var1, Vars),
	Var1 == Var.


supports_directly_or_indirectly(P, Q) :-
	belief(supports(P, Q, _)).

supports_directly_or_indirectly(P, Q) :-
	belief(supports(R, Q, _)),
	supports_directly_or_indirectly(P, R).


answer_move(R, Ps, signal_continuation(M)) :-
	is_dict(R),
	get_dict(continuation, R, true),
	get_dict(q, R, Q),
	answer_move(Q, Ps, M).

answer_move(R, Ps, M) :-
	is_dict(R),
	get_dict(q, R, Q),
	answer_move(Q, Ps, M).

answer_move(Q, P, hedge(M, Confidence)) :-
	@confidence(P, Confidence),
	!,
	unhedged_answer_move(Q, P, M).

answer_move(Q, [P], M) :-
	!,
	answer_move(Q, P, M).

answer_move(Q, P, M) :-
	unhedged_answer_move(Q, P, M).


unhedged_answer_move([]>>P, P, confirm(P)).

unhedged_answer_move([]>>P, not(P), disconfirm(not(P))).

unhedged_answer_move(_>>_, P, assert(P)).


remove_pragmatical_redundance(Q, IsContinuation, L, L2) :-
    remove_pragmatical_redundance(Q, IsContinuation, L, [], L2).

remove_pragmatical_redundance(_, _, [], _, []).

remove_pragmatical_redundance(Q, IsContinuation, [X|Xs], Prev, Ys) :-
	( member(Y, Prev) ; (IsContinuation == true, has_responded(Q, Y)) ),
    implicates(Q, Y, X),
    !,
    remove_pragmatical_redundance(Q, IsContinuation, Xs, [X|Prev], Ys).

remove_pragmatical_redundance(Q, IsContinuation, [X|Xs], Prev, [X|Ys]) :-
    remove_pragmatical_redundance(Q, IsContinuation, Xs, [X|Prev], Ys).


implicates(Q, A, B) :-
	copy_term((Q, A, B), (Q1, A1, B1)),
	implicates_with_unification(Q1, A1, B1).

implicates_with_unification(_, P, P).

implicates_with_unification(_>>supports(_, X, _), E, supports(E, X, M)) :-
	belief(supports(E, X, M)).

implicates_with_unification(_>>supports(_, X, _), supports(E, X, M), E) :-
	belief(supports(E, X, M)).


satisfy_tcu(Ps, TCU) :-
	( @tcu(TCU) ->
		append(TCU, _, Ps)
	;
		TCU = Ps
	).


has_responded(Q, P) :-
	@responded(Q, Ps),
	member(P, Ps).


compatible_with_facts(P) :-
	\+ (belief(Q), contradicts(Q, P)).


contradicts(not(P), P).

contradicts(P, not(P)).

contradicts(supports(P, Q1, How), supports(P, Q2, How)) :-
	ground(P),
	contradicts(Q1, Q2).

contradicts(relative_value(Feature, Individual, Value1), relative_value(Feature, Individual, Value2)) :-
	Value1 \== Value2.
