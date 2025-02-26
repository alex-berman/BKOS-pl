:- use_module(db).
:- ensure_loaded(isu_syntax).


_ :: agenda(emit_move(offer_help)).
_ :: qud([]).
_ :: previous_system_move(none).


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
	non_integrated_goal(question(Q, false))).

integrate_continuation_request :: (
	non_integrated_move(request_continuation(ask(Q))) ->
	non_integrated_goal(question(Q, true))).

integrate_elliptical_user_why_question :: ([
	non_integrated_goal(question(LiteralQuestion, Continuation)),
	$is_elliptical_question(LiteralQuestion),
	$resolve_elliptical_question(Continuation, ResolvedQuestion),
	qud(Qs)
	] -> [
		qud([ResolvedQuestion|Qs]),
		agenda(respond(question(ResolvedQuestion, Continuation)))
	]).
	
integrate_other_user_question :: ([
	non_integrated_goal(question(Q, Continuation)),
	$(Q \= why(none)),
	$(\+ (Q = why(P), \+ compatible_with_facts(P))),
	qud(Qs)
	] -> [
		qud([Q|Qs]),
		agenda(respond(question(Q, Continuation)))
	]).

integrate_user_negative_understanding_concerning_claim :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(M),
	qud([Q|Qs]),
	$(Q \= why(_)),
	$answer_move(Q, P, none, M)
	] ->
	[
		qud([why(P), Q|Qs]),
		agenda(respond(question(why(P), false)))
	]).

integrate_user_negative_understanding_concerning_enthymeme :: ([
	non_integrated_move(icm(understanding, negative)),
	^previous_system_move(assert(E, _)),
	qud([why(P)|Qs])
	] ->
	[
		qud([why(explains(E, P)), P, Qs]),
		agenda(respond(question(why(explains(E, P)), false)))
	]).

integrate_user_positive_acceptance :: (
	non_integrated_move(icm(acceptance, positive)) ->
	[]).

reject_question_with_false_presupposition :: ([
	non_integrated_goal(question(why(P), _)),
	$(P \== none),
	$(\+ compatible_with_facts(P))
	] ->
	next_system_move(icm(acceptance, negative, P))).

deduce_negation :: ([
	^agenda(respond(question(boolean_question(P), _))),
	$(\+ (belief(F, _), relevant_answer(boolean_question(P), F)))
	] ->
	not(P)).

reject_unanswerable_question :: ([
	agenda(respond(question(Q, _))),
	$(\+ (belief(P, _), relevant_answer(Q, P)))
	] ->
	next_system_move(icm(acceptance, negative, lack_knowledge(Q)))).

acknowledge_user_assertion :: (
	non_integrated_move(assert(_, _)) ->
	next_system_move(icm(acceptance, positive))).

emit_icm_for_no_additional_answer :: ([
	agenda(respond(question(Q, true))),
	$(\+ (
		relevant_answer(Q, P),
		fact(P),
		\+ fact(asserted(P))
	))
	]
	-> next_system_move(icm(acceptance, negative, no_additional_answers(Q)))).

respond_increment :: ([
	agenda(respond(question(Q, Continuation))),
	$(fact(answer_delivery_strategy(Q, incrementally)) ; \+ fact(answer_delivery_strategy(Q, _))),
	$relevant_answer(Q, P),
	$belief(P, Confidence),
	$(Continuation == false ; \+ fact(asserted(P))),
	$hedge_level(Confidence, Hedge),
	$answer_move(Q, P, Hedge, Move)
	] ->
	next_system_move(Move)).

respond_single_turn :: ([
	agenda(respond(question(Q, _))),
	answer_delivery_strategy(Q, single_turn),
	$findall(P, (belief(P, _), relevant_answer(Q, P)), Ps),
	$answer_move(Q, and(Ps), none, Move)
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
	$(\+ fact(asserted(P)))
	]
	-> asserted(P)).

utter_and_remember :: ([
	next_system_move(M),
	previous_system_move(_)
 	] -> [
		utter(M),
		previous_system_move(M)
	]).


relevant_answer(Q, not(P)) :-
	nonvar(P),
    relevant_answer(Q, P).

relevant_answer(boolean_question(P), P).

relevant_answer(boolean_question(P), not(P)).

relevant_answer(why(explains(Q, P)), supports(Q, P)).

relevant_answer(why(Explanandum), Explanans) :-
	fact(supports(Explanans, Explanandum)).

relevant_answer(wh_question(P), P).


answer_move(boolean_question(P), P, Hedge, confirm(P, Hedge)).

answer_move(boolean_question(P), not(P), Hedge, disconfirm(not(P), Hedge)).

answer_move(why(_), P, Hedge, assert(P, Hedge)).

answer_move(wh_question(P), P, Hedge, assert(P, Hedge)).

answer_move(wh_question(P), and(Conjuncts), Hedge, assert(and(Conjuncts), Hedge)) :-
	\+ ( member(Conjunct, Conjuncts), \+ unifiable(Conjunct, P, _) ).



constative_content(confirm(P, _), P).
constative_content(disconfirm(P, _), P).
constative_content(assert(P, _), P).


compatible_with_facts(P) :-
	fact(P).

compatible_with_facts(explains(Q, P)) :-
	fact(supports(Q, P)).


belief(P, none) :-
	fact(P).

belief(P, Confidence) :-
	fact(confidence(P, Confidence)).


hedge_level(none, none) :- !.

hedge_level(Confidence, strong) :-
	Confidence >= 0.9,
	!.

hedge_level(Confidence, medium) :-
	Confidence >= 0.1,
	!.

hedge_level(_, weak).


is_elliptical_question(why(none)).


resolve_elliptical_question(true, why(P)) :-
	fact(qud(Qs)),
	member(why(P), Qs),
	!.

resolve_elliptical_question(_, why(P)) :-
	fact(previous_system_move(M)),
	constative_content(M, P).

