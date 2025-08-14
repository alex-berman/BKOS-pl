:- ensure_loaded(isu_engine).

get_move_content :: ([
	heard(Move),
	$get_dict(content, Move, Content)
	] -> non_integrated_move(Content)).

integrate_user_question :: ([
	non_integrated_move(ask(Q)),
	qud(Qs)
	] -> [
		qud([Q|Qs]),
		agenda(respond(Q))
	]).

respond :: ([
	agenda(respond(Q)),
	$response_move(Q, Move)
	] ->
	utter(Move)).
