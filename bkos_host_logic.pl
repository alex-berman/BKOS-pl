response_move(Q, Move) :-
  relevant_answer(Q, P),
  @P,
	answer_move(Q, P, Move).

relevant_answer(P, P).

relevant_answer(P, not(P)).


answer_move(Q, P, confirm(P)) :-
	P = Q.

answer_move(Q, not(P), disconfirm(not(P))) :-
	P = Q.
