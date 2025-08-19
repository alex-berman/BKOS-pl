:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).


relevant_answer(P, P) :-
	polar_question(P).

relevant_answer(P, not(P)) :-
	polar_question(P).

relevant_answer(_^supports(QueriedEvidence, QueriedConsequent, _), Evidence) :-
	\+ ground(QueriedEvidence),
	ground(QueriedConsequent),
	supports_directly_or_indirectly(Evidence, QueriedConsequent).

relevant_answer(_^supports(QueriedEvidence, QueriedConsequent, _), Consequent) :-
	ground(QueriedEvidence),
	\+ ground(QueriedConsequent),
	supports_directly_or_indirectly(QueriedEvidence, Consequent).

relevant_answer(_^P, P) :-
	@P.


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


remove_pragmatical_redundance(Q, L, L2) :-
    remove_pragmatical_redundance(Q, L, [], L2).

remove_pragmatical_redundance(_, [], _, []).

remove_pragmatical_redundance(Q, [X|Xs], Prev, Ys) :-
	( member(Y, Prev) ; has_asserted(Y) ),
    implicates(Q, Y, X),
    !,
    remove_pragmatical_redundance(Q, Xs, [X|Prev], Ys).

remove_pragmatical_redundance(Q, [X|Xs], Prev, [X|Ys]) :-
    remove_pragmatical_redundance(Q, Xs, [X|Prev], Ys).


implicates(_, P, P).

implicates(_^supports(_, X, _), E, supports(E, X, M)) :-
	@supports(E, X, M).

implicates(_^supports(_, X, _), supports(E, X, M), E) :-
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
