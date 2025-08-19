:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).


relevant_answer(P, P) :-
	polar_question(P).

relevant_answer(P, not(P)) :-
	polar_question(P).

relevant_answer([_, _]^supports(_, X, _), E) :-
	@supports(E, X, _).

relevant_answer(_^P, P) :-
	@P.


polar_question(P) :-
	P \= _^_.


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
    member(Y, Prev),
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
