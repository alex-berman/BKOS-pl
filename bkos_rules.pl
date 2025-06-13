:- ensure_loaded(isu_engine).

clear_requested_continuation :: (requested_continuation(_, _) -> []).

reject_move_with_presupposition_violation :: ([
	heard(Move),
	$get_dict(presuppositions, Move, Presuppositions),
	$member(Presupposition, Presuppositions),
	$(\+ compatible_with_facts(Presupposition))
	] ->
	next_system_move(icm(acceptance, negative, Presupposition))).

get_move_content :: ([
	heard(Move),
	$get_dict(content, Move, Content)
	] -> non_integrated_move(Content)).

emit_move_on_agenda :: (
	agenda(emit_move(Move)) ->
	next_system_move(Move)
).

select_negative_understanding_when_no_semantic_interpretation :: (
	non_integrated_move(none) ->
	next_system_move(icm(understanding, negative))).

integrate_user_ask :: (
	non_integrated_move(ask(Q)) ->
	non_integrated_goal(question(Q))).

treat_user_positive_acceptance_as_continuation_request :: ([
	non_integrated_move(icm(acceptance, positive)),
	^qud([Q | _]),
	$response_delivery_strategy(Q, incrementally),
	$relevant_answer(Q, P),
	$question_and_answer_match_wrt_evidence_strategy(Q, P),
	^P,
	$(\+ asserted(P))
 	] -> [
		non_integrated_goal(question(Q)),
		requested_continuation(question(Q), implicit)
	]).

integrate_continuation_request :: ([
	non_integrated_move(request_continuation(PotentiallyUnderspecifiedMove)),
	$resolve_underspecified_move(PotentiallyUnderspecifiedMove, ResolvedGoal)
	] -> [
		non_integrated_goal(ResolvedGoal),
		requested_continuation(ResolvedGoal, explicit)
	]).

integrate_user_question :: ([
	non_integrated_goal(question(PotentiallyUnderspecifiedQuestion)),
	$resolve_underspecified_question(PotentiallyUnderspecifiedQuestion, ResolvedQuestion),
	qud(Qs)
	] -> [
		qud([ResolvedQuestion|Qs]),
		agenda(respond(question(ResolvedQuestion)))
	]).

integrate_user_negative_understanding_concerning_claim :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(M),
	qud([Q|Qs]),
	$(Q \= [E, H]^supports(E, _, H)),
	$answer_move(Q, P, M)
	] ->
	[
		qud([[E, H]^supports(E, P, H), Q | Qs]),
		agenda(respond(question([E, H]^supports(E, P, H))))
	]).

integrate_user_negative_understanding_concerning_datum_with_direct_response_strategy :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(PreviouslyAssertedDatum)),
	qud([[E, H]^supports(E, P, H), Q | Qs]),
	$response_strategy(Q, direct),
	$evidence_strategy([E, H]^supports(E, P, H), datum)
	] ->
	[
		qud([[H]^supports(PreviouslyAssertedDatum, P, H), [E, H]^supports(E, P, H), Q | Qs]),
		agenda(respond(question([H]^supports(PreviouslyAssertedDatum, P, H))))
	]).

integrate_user_negative_understanding_concerning_datum_with_datum_response_strategy :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(PreviouslyAssertedDatum)),
	qud([[E, H]^supports(E, P, H), Q | _]),
	$response_strategy(Q, datum),
	^supports(PreviouslyAssertedDatum, P, _)
	] ->
	next_system_move(assert(P))).

integrate_user_negative_understanding_concerning_warrant :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(supports(Datum, P, _))),
	qud([[E, H]^supports(E, P, H), Q | _]),
	$response_strategy(Q, direct),
	^E
	] ->
		next_system_move(assert(Datum))).

integrate_user_negative_understanding_concerning_inference :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(infer(Antecedent, P)),
	qud([[E, H]^supports(E, P, H) | Qs])
	] ->
	[
		qud([[H]^supports(Antecedent, P, How), P, Qs]),
		agenda(respond(question([H]^supports(Antecedent, P, How))))
	]).

ignore_user_positive_acceptance :: (
	non_integrated_move(icm(acceptance, positive)) ->
	[]).

deduce_negation :: ([
	^agenda(respond(question(P))),
	$is_polar_question(P),
	$(\+ (belief(F, _), relevant_answer(P, F)))
	] ->
	not(P)).

reject_unanswerable_question :: ([
	agenda(respond(question(Q))),
	$(\+ (belief(P, _), relevant_answer(Q, P)))
	] ->
	next_system_move(icm(acceptance, negative, lack_knowledge(Q)))).

acknowledge_user_assertion :: (
	non_integrated_move(assert(_)) ->
	next_system_move(icm(acceptance, positive))).

emit_icm_for_no_additional_answer :: ([
	agenda(respond(question(Q))),
	requested_continuation(question(Q), _),
	$(\+ (
		relevant_answer(Q, P),
		question_and_answer_match_wrt_evidence_strategy(Q, P),
		@P,
		\+ asserted(P)
	))
	]
	-> next_system_move(icm(acceptance, negative, no_additional_answers(Q)))).

respond_with_inference :: ([
	agenda(respond(question(Q))),
	^response_strategy(Q, inference),
	qud(QUD),
	$inference_answer(Q, QUD, AnswerMove, NewQUD)
	] ->
	[
		qud(NewQUD),
		next_system_move(AnswerMove)
	]).

respond_with_datum :: ([
	agenda(respond(question(Q))),
	^response_strategy(Q, datum),
	$relevant_answer(Q, P),
	$supports_directly_or_indirectly(Datum, P),
	$belief(Datum, Confidence),
	$hedge_level(Confidence, Hedge),
	$answer_move([E, H]^supports(E, P, H), Datum, Hedge, Move),
	qud(Qs)
	] -> [
		qud([[E, H]^supports(E, P, H) | Qs]),
		next_system_move(Move)
	]).

respond_directly :: ([
	agenda(respond(question(Q))),
	$response_strategy(Q, direct),
	$direct_response_move(Q, Move)
	] ->
	next_system_move(Move)).

select_negative_understanding_when_move_integration_failed :: (
	non_integrated_move(_) ->
	next_system_move(icm(understanding, negative))).

select_negative_understanding_when_goal_integration_failed :: (
	non_integrated_goal(_) ->
	next_system_move(icm(understanding, negative))).

store_assertion :: ([
	^next_system_move(M),
	$constative_content(M, P),
	$(\+ @asserted(P))
	]
	-> asserted(P)).

utter_and_remember :: ([
	next_system_move(M),
	previous_system_move(_)
 	] -> [
		utter(M),
		previous_system_move(M)
	]).
