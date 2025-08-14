response_move(Q, Move) :-
  relevant_answer(Q, P),
  @P,
	answer_move(Q, P, Move).

relevant_answer(P, P) :-
	is_polar_question(P).

relevant_answer(P, not(P)) :-
	is_polar_question(P).

is_polar_question(Q) :-
	\+ is_wh_question(Q).


is_wh_question(_^_).


answer_move(Q, P, confirm(P)) :-
	is_polar_question(P),
	P = Q.

answer_move(Q, not(P), disconfirm(not(P))) :-
	is_polar_question(P),
	P = Q.
