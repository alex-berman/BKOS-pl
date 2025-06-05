:- use_module(db).
:- ensure_loaded(isu_syntax).


% State intitialization

_ :: qud([]).
_ :: previous_system_move(none).


% ISU rules

clear_requested_continuation :: (requested_continuation(_) -> []).

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

integrate_continuation_request :: (
	non_integrated_move(request_continuation(ask(Q))) ->
	[
		non_integrated_goal(question(Q)),
		requested_continuation(question(Q))
	]).

integrate_user_question :: ([
	non_integrated_goal(question(PotentiallyUnderspecifiedQuestion)),
	$resolve_underspecified_question(PotentiallyUnderspecifiedQuestion, ResolvedQuestion),
	qud(Qs)
	] -> [
		qud([ResolvedQuestion|Qs]),
		agenda(respond(question(ResolvedQuestion)))
	]).

resolve_continuation_request :: ([
	requested_continuation(question(PotentiallyUnderspecifiedQuestion)),
	$resolve_underspecified_question(PotentiallyUnderspecifiedQuestion, ResolvedQuestion)
	]
	-> requested_continuation(question(ResolvedQuestion))).

integrate_other_user_goal :: ([
	non_integrated_goal(question(Q)),
	$(Q \= wh_question(supports(_, ?, _))),
	qud(Qs)
	] -> [
		qud([Q|Qs]),
		agenda(respond(question(Q)))
	]).

integrate_user_negative_understanding_concerning_claim :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(M),
	qud([Q|Qs]),
	$(Q \= wh_question(supports(_, _, _))),
	$answer_move(Q, P, M)
	] ->
	[
		qud([wh_question(supports(_, P, _)), Q|Qs]),
		agenda(respond(question(wh_question(supports(_, P, _)))))
	]).

integrate_user_negative_understanding_concerning_datum_with_direct_response_strategy :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(E)),
	qud([wh_question(supports(_, P, _)), Q | Qs]),
	$response_strategy(Q, direct),
	$evidence_strategy(wh_question(supports(_, P, _)), datum)
	] ->
	[
		qud([wh_question(supports(E, P, How)), wh_question(supports(_, P, _)), Q | Qs]),
		agenda(respond(question(wh_question(supports(E, P, How)))))
	]).

integrate_user_negative_understanding_concerning_datum_with_datum_response_strategy :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(E)),
	qud([wh_question(supports(_, P, _)), Q | _]),
	$response_strategy(Q, datum),
	^supports(E, P, _)
	] ->
	next_system_move(assert(P))).

integrate_user_negative_understanding_concerning_warrant :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(supports(E, P, _))),
	qud([wh_question(supports(_, P, _)), Q | _]),
	$response_strategy(Q, direct),
	^E
	] ->
		next_system_move(assert(E))).

integrate_user_negative_understanding_concerning_inference :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(infer(E, P)),
	qud([wh_question(supports(_, P, _))|Qs])
	] ->
	[
		qud([wh_question(supports(E, P, How)), P, Qs]),
		agenda(respond(question(wh_question(supports(E, P, How)))))
	]).

integrate_user_positive_acceptance :: (
	non_integrated_move(icm(acceptance, positive)) ->
	[]).

deduce_negation :: ([
	^agenda(respond(question(boolean_question(P)))),
	$(\+ (belief(F, _), relevant_answer(boolean_question(P), F)))
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
	requested_continuation(question(Q)),
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
	$answer_move(wh_question(supports(_, P, _)), Datum, Hedge, Move),
	qud(Qs)
	] -> [
		qud([wh_question(supports(_, P, _))|Qs]),
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


% Helper relations

direct_response_move(Q, Move) :-
	response_delivery_strategy(Q, incrementally),
	findall((P, Confidence), (
		relevant_answer(Q, P),
		question_and_answer_match_wrt_evidence_strategy(Q, P),
		belief(P, Confidence),
		(\+ @requested_continuation(question(Q)) ; \+ asserted(P))
	), PsAndConfidences),
	select_answer(PsAndConfidences, P, Confidence),
	hedge_level(Confidence, Hedge),
	answer_move(Q, P, Hedge, Move).

direct_response_move(Q, Move) :-
	response_delivery_strategy(Q, single_turn),
	findall(P, (
		relevant_answer(Q, P),
		question_and_answer_match_wrt_evidence_strategy(Q, P),
		belief(P, _)
	), Ps),
	answer_move(Q, Ps, Move).


response_delivery_strategy(wh_question(supports(_, P, _)), Strategy) :-
	relevant_answer(Q, P),
	!,
	explanation_delivery_strategy(Q, Strategy).

response_delivery_strategy(Q, Strategy) :-
	(@answer_delivery_strategy(Q, DeclaredStrategy) -> Strategy = DeclaredStrategy
	; Strategy = incrementally ).


explanation_delivery_strategy(Q, Strategy) :-
	(@explanation_delivery_strategy(Q, DeclaredStrategy) -> Strategy = DeclaredStrategy
	; Strategy = incrementally ).


relevant_answer(Q, not(P)) :-
	nonvar(P),
    relevant_answer(Q, P).

relevant_answer(boolean_question(P), P).

relevant_answer(boolean_question(P), not(P)).

relevant_answer(wh_question(supports(Antecedent, Explanandum, _)), Datum) :-
	var(Antecedent),
	ground(Explanandum),
	supports_directly_or_indirectly(Datum, Explanandum),
	unifiable(Antecedent, Datum, _).

relevant_answer(wh_question(supports(Datum, Explanandum, _)), supports(Antecedent, Consequent, How)) :-
	@supports(Antecedent, Consequent, How),
	unifiable(Antecedent, Datum, _),
	unifiable(Consequent, Explanandum, _).

relevant_answer(wh_question(P), P) :-
	P \= support(_, _, _).


asserted(P) :-
	@asserted(Conjunction),
	member(P, Conjunction).

asserted(P) :-
	@asserted(P).


supports_directly_or_indirectly(P, Q) :-
	@supports(P, Q, _).

supports_directly_or_indirectly(P, Q) :-
	@supports(R, Q, _),
	supports_directly_or_indirectly(P, R).


answer_move(boolean_question(P), P, confirm(P)).

answer_move(boolean_question(P), not(P), disconfirm(not(P))).

answer_move(wh_question(supports(_, _, _)), P, assert(P)).

answer_move(wh_question(P), P, assert(P)).

answer_move(wh_question(P), Conjuncts, assert(Conjuncts)) :-
	\+ ( member(Conjunct, Conjuncts), \+ unifiable(Conjunct, P, _) ).


select_answer(PsAndConfidences, P, Confidence) :-
	@P,
		member((P, Confidence), PsAndConfidences),
	!.

select_answer([(P, Confidence)|_], P, Confidence).


question_and_answer_match_wrt_evidence_strategy(Q, P) :-
	evidence_strategy(Q, Strategy),
	answer_matches_evidence_strategy(P, Strategy).


response_strategy(Q, Strategy) :-
	( @response_strategy(Q, DeclaredStrategy) ->
		Strategy = DeclaredStrategy
	; Strategy = direct ).


evidence_strategy(wh_question(supports(Antecedent, P, _)), Strategy) :-
	var(Antecedent),
	ground(P),
	relevant_answer(Q, P),
	!,
	( @evidence_strategy(Q, DeclaredStrategy) ->
		Strategy = DeclaredStrategy
	; Strategy = datum ).

evidence_strategy(_, none).


answer_matches_evidence_strategy(_, none).

answer_matches_evidence_strategy(P, datum) :-
	P \= supports(_, _, _).

answer_matches_evidence_strategy(supports(_, _, _), warrant).


answer_move(Q, P, none, Move) :-
	answer_move(Q, P, Move).

answer_move(Q, P, Hedge, hedge(Move, Hedge)) :-
	Hedge \= none,
	answer_move(Q, P, none, Move).


constative_content(confirm(P), P).
constative_content(disconfirm(P), P).
constative_content(assert(P), P).
constative_content(hedge(Move), P) :-
	constative_content(Move, P).


compatible_with_facts(P) :-
	\+ (@Q, contradicts(Q, P)).


contradicts(not(P), P).

contradicts(P, not(P)).

contradicts(supports(P, Q1, How), supports(P, Q2, How)) :-
	ground(P),
	contradicts(Q1, Q2).

contradicts(relative_value(Feature, Individual, Value1), relative_value(Feature, Individual, Value2)) :-
	Value1 \== Value2.


belief(P, none) :-
	@P.

belief(P, Confidence) :-
	@confidence(P, Confidence).


hedge_level(none, none) :- !.

hedge_level(Confidence, strong) :-
	Confidence >= 0.9,
	!.

hedge_level(Confidence, medium) :-
	Confidence >= 0.1,
	!.

hedge_level(_, weak).


resolve_underspecified_question(wh_question(supports(_, ?, _)), wh_question(supports(_, P, _))) :-
	@requested_continuation(question(wh_question(supports(_, ?, _)))),
	@qud(Qs),
	member(wh_question(supports(_, P, _)), Qs),
	!.

resolve_underspecified_question(wh_question(supports(_, ?, _)), wh_question(supports(_, P, _))) :-
	@previous_system_move(M),
	constative_content(M, P).

resolve_underspecified_question(boolean_question(supports(P, Q, ?)), boolean_question(supports(P, Q, How))) :-
	@supports(P, Q, How),
	!.

resolve_underspecified_question(boolean_question(supports(P, Q, ?)), boolean_question(supports(P, Q, _))).


inference_answer(Q, QUD, infer(Antecedent, P), NewQUD) :-
	explanation_delivery_strategy(Q, incrementally),
	relevant_answer(Q, P),
	@P,
	@supports(Datum, P, _),
	( (relevant_answer(DatumQ, Datum), response_strategy(DatumQ, inference)) ->
		inference_answer(DatumQ, [wh_question(supports(_, P, _)) | QUD], Antecedent, NewQUD)
	;
		Antecedent = Datum,
		NewQUD = [wh_question(supports(_, P, _)) | QUD]
	).

inference_answer(Q, QUD, infer(Data, P), [wh_question(supports(_, P, _)) | QUD]) :-
	explanation_delivery_strategy(Q, single_turn),
	findall(Datum, (
		relevant_answer(Q, P),
		@P,
		@supports(Datum, P, _)
	), Data).
