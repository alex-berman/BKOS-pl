:- ensure_loaded(isu_engine).

get_move :: ([
	heard(Interpretation),
	$get_dict(move, Interpretation, Move)
	] -> non_integrated_move(Move)).

integrate_user_question :: (
	non_integrated_move(ask(Q)) -> agenda(respond(Q))
	).

respond :: ([
	agenda(respond(Q)),
	$response_move(Q, Move)
	] ->
	utter(Move)).
