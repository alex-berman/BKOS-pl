:- ensure_loaded(isu_syntax).

provide_negative_understanding_when_no_semantic_interpretation :: [
	heard(Interpretation),
	$get_dict(move, Interpretation, none)
	] -* utter(icm(understanding, negative)).

reject_move_with_presupposition_violation :: [
	heard(Move),
	$get_dict(presuppositions, Move, Presuppositions),
	$member(Presupposition, Presuppositions),
	^Belief,
	$contradicts(Belief, Presupposition)
	] -*
	utter(icm(acceptance, negative, Presupposition)).

reject_unanswerable_question :: [
	heard(Interpretation),
	$get_dict(move, Interpretation, ask(Q)),
	$(\+ valid_answer(Q, _))
	] -*
	utter(icm(acceptance, negative, lack_knowledge(Q))).

mark_move_as_accepted :: [
	heard(Interpretation),
	$get_dict(move, Interpretation, Move),
	*agenda(_)
	] -* accepted(Move).

integrate_user_question :: [
	accepted(ask(Q)),
	*responded(Q, _)
	] -* [
		qud(Q),
		agenda(respond(Q))
	].

integrate_acknowledgement :: [
	accepted(icm(acceptance, positive)),
	^qud(Q)
	] -* agenda(resume(respond(Q))).

respond :: [
	agenda(respond(Q)),
	$findall(P, valid_answer(Q, P), ValidAnswers),
	$select_answers(Q, ValidAnswers, SelectedAnswers),
	$answer_move(Q, SelectedAnswers, Move)
	] -* [
		utter(Move),
		responded(Q, SelectedAnswers)
	].

resume_responding :: [
	agenda(resume(respond(Q))),
	$findall(P, (
		valid_answer(Q, P),
		\+ has_responded(Q, P)
	), ValidAnswers),
	$select_answers(Q, ValidAnswers, SelectedAnswers),
	$answer_move(Q, SelectedAnswers, Move)
	] -* [
		utter(signal_resumption(Move)),
		responded(Q, SelectedAnswers)
	].

deliver_claim_and_supporting_evidence_as_inference :: [
	agenda(argue(C)),
	$(Q = [E, M]>>supports(E, C, M)),
	$findall(P, valid_answer(Q, P), ValidAnswers),
	$select_answers(Q, ValidAnswers, SelectedAnswers)
	] -* utter(infer(SelectedAnswers, C)).
