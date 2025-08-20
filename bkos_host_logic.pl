:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).


valid_answer(P, A) :-
	polar_question(P),
	( A = P ; A = not(P) ).

valid_answer(Vars^Body, A) :-
	Body = supports(E, Consequent, _),
    memberchk(E, Vars),
	(
		supports_directly_or_indirectly(Evidence, Consequent),
		A = Evidence
	;
		@SupportsFact,
		unifiable(SupportsFact, supports(_, Consequent, _), _),
		A = SupportsFact
	).

valid_answer([C, M]^supports(Evidence, C, M), Consequent) :-
	\+ ground(C),
	\+ ground(M),
	ground(Evidence),
	supports_directly_or_indirectly(Evidence, Consequent).

valid_answer(_^P, A) :-
	P \= supports(_, _, _),
	copy_term(P, A),
	@A.


polar_question(P) :-
	P \= _^_.


supports_directly_or_indirectly(P, Q) :-
	@supports(P, Q, _).

supports_directly_or_indirectly(P, Q) :-
	@supports(R, Q, _),
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

answer_move(Q, [P], M) :-
	!,
	answer_move(Q, P, M).

answer_move(P, P, confirm(P)).

answer_move(P, not(P), disconfirm(not(P))).

answer_move(_^_, P, assert(P)).


remove_pragmatical_redundance(R, L, L2) :-
    remove_pragmatical_redundance(R, L, [], L2).

remove_pragmatical_redundance(_, [], _, []).

remove_pragmatical_redundance(R, [X|Xs], Prev, Ys) :-
	( member(Y, Prev) ; (get_dict(continuation, R, true), has_asserted(Y)) ),
	get_dict(q, R, Q),
    implicates(Q, Y, X),
    !,
    remove_pragmatical_redundance(R, Xs, [X|Prev], Ys).

remove_pragmatical_redundance(R, [X|Xs], Prev, [X|Ys]) :-
    remove_pragmatical_redundance(R, Xs, [X|Prev], Ys).


implicates(Q, A, B) :-
	copy_term((Q, A, B), (Q1, A1, B1)),
	implicates_with_unification(Q1, A1, B1).

implicates_with_unification(_, P, P).

implicates_with_unification(_^supports(_, X, _), E, supports(E, X, M)) :-
	@supports(E, X, M).

implicates_with_unification(_^supports(_, X, _), supports(E, X, M), E) :-
	@supports(E, X, M).


satisfy_tcu(Ps, TCU) :-
	( @tcu(TCU) ->
		append(TCU, _, Ps)
	;
		TCU = Ps
	).


has_asserted(P) :-
	@uttered(Move),
	constative_content(Move, P).


constative_content(confirm(P), P).

constative_content(disconfirm(P), P).

constative_content(assert(P), P).

constative_content(signal_continuation(M), P) :-
	constative_content(M, P).

constative_content(Moves, P) :-
	is_list(Moves),
	member(Move, Moves),
	constative_content(Move, P).
