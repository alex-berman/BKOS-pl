response(Q, Move) :-
	relevant_answer(Q, P),
	@P,
	answer_move(Q, P, Move).


relevant_answer(P, P).

relevant_answer(P, not(P)).


answer_move(P, P, confirm(P)).

answer_move(P, not(P), disconfirm(not(P))).
