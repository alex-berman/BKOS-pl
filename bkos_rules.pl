:- ensure_loaded(isu_syntax).

_ :: qud([]).

reject_move_with_presupposition_violation :: [
	heard(Move),
	$get_dict(presuppositions, Move, Presuppositions),
	$member(Presupposition, Presuppositions),
	$(\+ compatible_with_facts(Presupposition))
	] -*
	utter(icm(acceptance, negative, Presupposition)).

get_move_and_clear_agenda :: [
	heard(Interpretation),
	$get_dict(move, Interpretation, Move),
	*agenda(_)
	] -* non_integrated_move(Move).

provide_negative_understanding_when_no_semantic_interpretation ::
	non_integrated_move(none) -*
	utter(icm(understanding, negative)).

reject_unanswerable_question :: [
	non_integrated_move(ask(Q)),
	$(\+ valid_answer(Q, _))
	] -*
	utter(icm(acceptance, negative, lack_knowledge(Q))).

integrate_user_question :: [
	qud(Qs),
	non_integrated_move(ask(Q)),
	*responded(Q, _)
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
	$(get_dict(continuation, R, IsContinuation) -> true ; IsContinuation = false),
	$findall(P, (
		belief(P),
		\+ (IsContinuation == true, has_responded(Q, P)),
		valid_answer(Q, P)
	), ValidNonUtteredResponses),
	$remove_pragmatical_redundance(Q, IsContinuation, ValidNonUtteredResponses, ValidInformativeResponses),
	$satisfy_tcu(ValidInformativeResponses, SelectedResponses),
	$answer_move(R, SelectedResponses, Move)
	] -* [
		utter(Move),
		responded(Q, SelectedResponses)
	].

deliver_claim_and_supporting_evidence_as_inference :: [
	agenda(argue(C)),
	$(Q = [E, M]>>supports(E, C, M)),
	$findall(P, (
		belief(P),
		valid_answer(Q, P)
	), ValidSupportingPropositions),
	$remove_pragmatical_redundance(Q, false, ValidSupportingPropositions, SelectedSupportingPropositions)
	] -* utter(infer(SelectedSupportingPropositions, C)).
