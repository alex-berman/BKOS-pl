:- use_module(db).
:- ensure_loaded(isu_syntax).


% State intitialization

_ :: qud([]).
_ :: previous_system_move(none).


% ISU rules

clear_requested_continuation :: (requested_continuation(_) -> []).

get_latest_move :: (
	heard(Move) -> non_integrated_move(Move)).

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

reject_question_with_false_presupposition :: ([
	non_integrated_goal(question(Q)),
	$presupposes(Q, P),
	$(P \== ?),
	$(\+ compatible_with_facts(P))
	] ->
	next_system_move(icm(acceptance, negative, P))).

integrate_underspecified_user_question :: ([
	non_integrated_goal(question(LiteralQuestion)),
	$resolve_underspecified_question(LiteralQuestion, ResolvedQuestion),
	qud(Qs)
	] -> [
		qud([ResolvedQuestion|Qs]),
		agenda(respond(question(ResolvedQuestion)))
	]).

specify_continuation_request :: ([
	requested_continuation(question(LiteralQuestion)),
	$resolve_underspecified_question(LiteralQuestion, ResolvedQuestion)
	]
	-> requested_continuation(question(ResolvedQuestion))).

integrate_other_user_goal :: ([
	non_integrated_goal(question(Q)),
	$(Q \= why(?)),
	$(\+ (Q = why(P), \+ compatible_with_facts(P))),
	qud(Qs)
	] -> [
		qud([Q|Qs]),
		agenda(respond(question(Q)))
	]).

integrate_user_negative_understanding_concerning_claim :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(M),
	qud([Q|Qs]),
	$(Q \= why(_)),
	$answer_move(Q, P, M)
	] ->
	[
		qud([why(P), Q|Qs]),
		agenda(respond(question(why(P))))
	]).

integrate_user_negative_understanding_concerning_datum_with_direct_response_strategy :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(E)),
	qud([why(P), Q | Qs]),
	$response_strategy(Q, direct),
	$evidence_strategy(why(P), datum)
	] ->
	[
		qud([wh_question(supports(E, P, How)), why(P), Q | Qs]),
		agenda(respond(question(wh_question(supports(E, P, How)))))
	]).

integrate_user_negative_understanding_concerning_datum_with_datum_response_strategy :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(E)),
	qud([why(P), Q | _]),
	$response_strategy(Q, datum),
	^supports(E, P, _)
	] ->
	next_system_move(assert(P))).

integrate_user_negative_understanding_concerning_warrant :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(supports(E, P, _))),
	qud([why(P), Q | _]),
	$response_strategy(Q, direct),
	^E
	] ->
		next_system_move(assert(E))).

integrate_user_negative_understanding_concerning_inference :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(infer(E, P)),
	qud([why(P)|Qs])
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
		\+ @asserted(P)
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
	$answer_move(why(P), Datum, Hedge, Move),
	qud(Qs)
	] -> [
		qud([why(P)|Qs]),
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
	@answer_delivery_strategy(Q, incrementally) ; \+ @answer_delivery_strategy(Q, _),
	findall((P, Confidence), (
		relevant_answer(Q, P),
		question_and_answer_match_wrt_evidence_strategy(Q, P),
		belief(P, Confidence),
		(\+ @requested_continuation(question(Q)) ; \+ @asserted(P))
	), PsAndConfidences),
	select_answer(PsAndConfidences, P, Confidence),
	hedge_level(Confidence, Hedge),
	answer_move(Q, P, Hedge, Move).

direct_response_move(Q, Move) :-
	@answer_delivery_strategy(Q, single_turn),
	findall(P, (
		belief(P, _),
		relevant_answer(Q, P),
		question_and_answer_match_wrt_evidence_strategy(Q, P)
	), Ps),
	answer_move(Q, Ps, Move).


relevant_answer(Q, not(P)) :-
	nonvar(P),
    relevant_answer(Q, P).

relevant_answer(boolean_question(P), P).

relevant_answer(boolean_question(P), not(P)).

relevant_answer(why(Explanandum), Datum) :-
	supports_directly_or_indirectly(Datum, Explanandum).

relevant_answer(why(Explanandum), supports(Antecedent, Consequent, How)) :-
	@supports(Antecedent, Consequent, How),
	unifiable(Consequent, Explanandum, _).

relevant_answer(wh_question(P), P).


supports_directly_or_indirectly(P, Q) :-
	@supports(P, Q, _).

supports_directly_or_indirectly(P, Q) :-
	@supports(R, Q, _),
	supports_directly_or_indirectly(P, R).


answer_move(boolean_question(P), P, confirm(P)).

answer_move(boolean_question(P), not(P), disconfirm(not(P))).

answer_move(why(_), P, assert(P)).

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


evidence_strategy(why(P), Strategy) :-
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


resolve_underspecified_question(why(?), why(P)) :-
	@requested_continuation(question(why(?))),
	@qud(Qs),
	member(why(P), Qs),
	!.

resolve_underspecified_question(why(?), why(P)) :-
	@previous_system_move(M),
	constative_content(M, P).

resolve_underspecified_question(boolean_question(supports(P, Q, ?)), boolean_question(supports(P, Q, How))) :-
	@supports(P, Q, How),
	!.

resolve_underspecified_question(boolean_question(supports(P, Q, ?)), boolean_question(supports(P, Q, _))).


presupposes(why(P), P).
presupposes(wh_question(P), P).
presupposes(boolean_question(supports(P, _, _)), P).
presupposes(boolean_question(supports(_, Q, _)), Q).


inference_answer(Q, QUD, infer(Antecedent, P), NewQUD) :-
	relevant_answer(Q, P),
	@P,
	@supports(Datum, P, _),
	( (relevant_answer(DatumQ, Datum), response_strategy(DatumQ, inference)) ->
		inference_answer(DatumQ, [why(P) | QUD], Antecedent, NewQUD)
	;
		Antecedent = Datum,
		NewQUD = [why(P) | QUD]
	).
