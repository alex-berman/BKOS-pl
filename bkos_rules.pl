:- ensure_loaded(isu_syntax).

_ :: qud([]).

get_move :: [
	heard(Interpretation),
	$get_dict(move, Interpretation, Move)
	] -* non_integrated_move(Move).

integrate_user_question :: [
	qud(Qs),
	non_integrated_move(ask(Q))
	] -* [
		qud([Q|Qs]),
		agenda(respond(_{q:Q}))
	].

integrate_acknowledgement :: [
	non_integrated_move(icm(acceptance, positive)),
	^qud([Q|_])
	] -* agenda(respond(_{q:Q, continuation:true})).

respond :: [
	agenda(respond(R)),
	$get_dict(q, R, Q),
	$findall(P, (
		@P,
		\+ (get_dict(continuation, R, true), has_asserted(P)),
		valid_answer(Q, P)
	), ValidNonUtteredResponses),
	$remove_pragmatical_redundance(R, ValidNonUtteredResponses, ValidInformativeResponses),
	$satisfy_tcu(ValidInformativeResponses, SelectedResponses),
	$answer_move(R, SelectedResponses, Move)
	] -*
	[
		utter(Move),
		uttered(Move)
	].
