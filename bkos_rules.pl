:- ensure_loaded(isu_syntax).

get_move :: ([
	heard(Interpretation),
	$get_dict(move, Interpretation, Move)
	] -> non_integrated_move(Move)).

integrate_user_question :: (
	non_integrated_move(ask(Q)) -> agenda(respond(Q))
	).

respond :: ([
	agenda(respond(Q)),
	$findall(P, (
		@P,
		relevant_answer(Q, P)
	), RelevantResponses),
	$remove_pragmatical_redundance(Q, RelevantResponses, RelevantInformativeResponses),
	$satisfy_tcu(RelevantInformativeResponses, SelectedResponses),
	$answer_move(Q, SelectedResponses, Move)
	] ->
	utter(Move)).
