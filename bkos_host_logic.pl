direct_response_move(Q, Move) :-
	response_delivery_strategy(Q, incrementally),
	findall((P, Confidence), (
		relevant_answer(Q, P),
		question_and_answer_match_wrt_evidence_strategy(Q, P),
		belief(P, Confidence),
		(\+ @requested_continuation(question(Q), _) ; \+ asserted(P))
	), PsAndConfidences),
	select_answer(PsAndConfidences, P, Confidence),
	hedge_level(Confidence, Hedge),
	answer_move(Q, P, Hedge, AnswerMove),
	( @requested_continuation(question(Q), implicit) ->
		Move = signal_continuation(AnswerMove)
	;
		Move = AnswerMove
	).

direct_response_move(Q, Move) :-
	response_delivery_strategy(Q, single_turn),
	findall(P, (
		relevant_answer(Q, P),
		question_and_answer_match_wrt_evidence_strategy(Q, P),
		belief(P, _)
	), Ps),
	answer_move(Q, Ps, Move).


response_delivery_strategy([E, H]^supports(E, P, H), Strategy) :-
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

relevant_answer(P, P) :-
	is_polar_question(P).

relevant_answer(P, not(P)) :-
	is_polar_question(P).

relevant_answer([E, H]^supports(E, Explanandum, H), Datum) :-
	ground(Explanandum),
	supports_directly_or_indirectly(Datum, Explanandum).

relevant_answer([E, H]^supports(E, Explanandum, H), supports(Datum, Explanandum, How)) :-
	@supports(Datum, Explanandum, How).

relevant_answer([How]^supports(Datum, Explanandum, How), supports(Datum, Explanandum, How)) :-
	@supports(Datum, Explanandum, How).

relevant_answer(_^P, P) :-
	P \= supports(_, _, _).


is_polar_question(Q) :-
	\+ is_wh_question(Q).


is_wh_question(_^_).


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


answer_move(Q, P, confirm(P)) :-
	is_polar_question(P),
	P = Q.

answer_move(Q, not(P), disconfirm(not(P))) :-
	is_polar_question(P),
	P = Q.

answer_move(_^supports(_, _, _), P, assert(P)).

answer_move(_^P, P, assert(P)).

answer_move(_^P, Conjuncts, assert(Conjuncts)) :-
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


evidence_strategy([E, H]^supports(E, P, H), Strategy) :-
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

constative_content(signal_continuation(Move), P) :-
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


resolve_underspecified_move(?, question(Q)) :-
	@qud([Q | _]).

resolve_underspecified_move(ask(PotentiallyUnderspecifiedQuestion), question(ResolvedQuestion)) :-
	resolve_underspecified_question(PotentiallyUnderspecifiedQuestion, ResolvedQuestion).


resolve_underspecified_question([E, H]^supports(E, ?, H), [E, H]^supports(E, P, H)) :-
	@non_integrated_move(request_continuation(ask([E, H]^supports(E, ?, H)))),
	@qud(Qs),
	member([E, H]^supports(E, P, H), Qs),
	!.

resolve_underspecified_question([E, H]^supports(E, ?, H), [E, H]^supports(E, P, H)) :-
	@previous_system_move(M),
	constative_content(M, P),
	!.

resolve_underspecified_question([X]^uses_feature(?, X), [X]^uses_feature(Q, X)) :-
	@qud([Q | _]),
	!.

resolve_underspecified_question([X]^value(QueriedFeature, QueriedIndividual, X), [X]^value(Feature, Individual, X)) :-
	( QueriedFeature == ? ; QueriedIndividual == ? ),
	@previous_system_move(M),
	constative_content(M, relative_value(Feature, Individual, _)),
	matches_query(Feature, QueriedFeature),
	matches_query(Individual, QueriedIndividual).

resolve_underspecified_question(Q, Q) :-
	\+ is_underspecified(Q).


matches_query(_, X) :-
	(X == ?).

matches_query(X, X).


is_underspecified(Term) :-
    atomic(Term),
	!,
    (Term == ?).

is_underspecified(Term) :-
	nonvar(Term),
	Term =.. [_ | Args],
	member(Arg, Args),
	is_underspecified(Arg).



inference_answer(Q, QUD, infer(Antecedent, P), NewQUD) :-
	explanation_delivery_strategy(Q, incrementally),
	relevant_answer(Q, P),
	@P,
	@supports(Datum, P, _),
	NewQ = [E, H]^supports(E, P, H),
	( (relevant_answer(DatumQ, Datum), response_strategy(DatumQ, inference)) ->
		inference_answer(DatumQ, [NewQ | QUD], Antecedent, NewQUD)
	;
		Antecedent = Datum,
		NewQUD = [NewQ | QUD]
	).

inference_answer(Q, QUD, infer(Data, P), [[E, H]^supports(E, P, H) | QUD]) :-
	explanation_delivery_strategy(Q, single_turn),
	findall(Datum, (
		relevant_answer(Q, P),
		@P,
		@supports(Datum, P, _)
	), Data).
