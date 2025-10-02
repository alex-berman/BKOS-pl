:- ensure_loaded(isu_syntax).

signal_negative_understanding ::
	recognized(unresolvable_phrase(Phrase)) -*
	utter(icm(understanding, negative, unresolvable_phrase(Phrase))).

reject_move_with_presupposition_violation :: [
	recognized(presupposition(Presupposition)),
	^Belief,
	$contradicts(Belief, Presupposition),
	recognized(move(Move))
	] -* utter(icm(acceptance, negative, Move, false(Presupposition))).

reject_unanswerable_question :: [
	recognized(move(ask(Q))),
	$(\+ valid_answer(Q, _))
	] -* utter(icm(acceptance, negative, ask(Q), lack_knowledge)).

mark_move_as_accepted :: [
	recognized(move(Move)),
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
		utter(resumption(Move)),
		responded(Q, SelectedAnswers)
	].

argue :: [
	agenda(argue(C)),
	$(Q = [E, M]>>supports(E, C, M)),
	$findall(P, valid_answer(Q, P), ValidAnswers),
	$select_answers(Q, ValidAnswers, SelectedAnswers),
	$normalize(SelectedAnswers, A)
	] -* utter(infer(A, C)).
