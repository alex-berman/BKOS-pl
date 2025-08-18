:- ensure_loaded(isu_engine).

get_move :: ([
	heard(Interpretation),
	$get_dict(move, Interpretation, Move)
	] -> non_integrated_move(Move)).

integrate_user_question :: (
	non_integrated_move(ask(Q)) -> agenda(respond(Q))
	).

add_response :: ([
	^agenda(respond(Q)),
	^P,
	$relevant_answer(Q, P),
	$(\+ (
		@response(Q, P2),
		implicates(Q, P2, P)
	))
	] ->
	response(Q, P)).

respond :: ([
	agenda(respond(Q)),
	$(\+ (
		@P,
		relevant_answer(Q, P),
		\+ (
			@response(Q, P2),
			implicates(Q, P2, P)
		)
	)),
	$findall(P, @response(Q, P), Ps),
	$answer_move(Q, Ps, Move)
	] ->
	utter(Move)).
