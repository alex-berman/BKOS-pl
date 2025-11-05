:- module(nlg, [generate/2]).


assert(P) --> proposition(P), glue('.').

proposition(rel_prob(Event, R)) --> [there, is, a, R, probability, that], event(Event).
proposition(not(rel_prob(Event, R))) --> [the, probability, that], event(Event), [is, not, R].
proposition(value(P, V)) --> property(P), [is, V].
proposition(not(value(P, V))) --> property(P), [is, not, V].
proposition(value(has_other_illness(pat_1), true)) --> [the, patient, has, other, illnesses].
proposition(value(has_other_illness(pat_1), false)) --> [the, patient, has, no, other, illnesses].
proposition(rel_value(back_pain(pat_1), V)) -->
    [the, patient, has], relative_back_pain_level(V), [back, pain].
proposition(rel_value(disability(pat_1), V)) --> [the, patient, has, relatively, V, disability].
proposition([X]) --> proposition(X).
proposition([X,Y]) --> proposition(X), [and], proposition(Y).
proposition([X,Y|Rest]) --> proposition(X), glue(','), proposition(Y), [and], proposition(Rest).
proposition(supports(A, C, association)) --> ['I', associate], concept(A), [with], concept(C).
proposition(surgery_recommended(pat_1)) --> [surgery, is, recommended, for, the, patient].
proposition(not(surgery_recommended(pat_1))) --> [surgery, is, not, recommended, for, the, patient].
proposition(not(P)) --> ['it\'s', not, the, case, that], proposition(P).

confirm(P) --> ['yes,'], proposition(P), glue('.').
disconfirm(P) --> ['no,'], proposition(P), glue('.').

event(satisfied(pat_1)) --> [the, patient, will, be, satisfied, with, surgery].

property(back_pain(pat_1)) --> [the, 'patient\'s', back, pain, level].
property(disability(pat_1)) --> [the, 'patient\'s', disability].

concept(rel_value(back_pain(_), V)) --> back_pain_level(V), [back, pain].
concept(rel_value(disability(_), V)) --> [V, disability].
concept(value(has_other_illness(_), V)) --> presence_or_absense(V), [of, other, illnesses].
concept(rel_prob(satisfied(_), R)) --> [a, R, probability, of, being, satisfied, with, surgery].

relative_back_pain_level(low) --> [relatively, mild].
relative_back_pain_level(high) --> [relatively, severe].
relative_back_pain_level(moderate) --> [moderate].

back_pain_level(low) --> [mild].
back_pain_level(high) --> [severe].
back_pain_level(moderate) --> [moderate].

presence_or_absense(false) --> [absense].
presence_or_absense(true) --> [presence].

signal_resumption --> [also], glue(',').

infer(A, C) --> [since], proposition(A), glue(','), proposition(C), glue('.').

icm(acceptance, negative, lack_knowledge) --> ['I', 'don\'t', have, any, information, about, that], glue('.').
icm(acceptance, negative, P) --> proposition(P), glue('.').
icm(understanding, negative, unresolvable_phrase(P)) -->
    [sorry], glue(','), ['I', 'don\'t', understand, what,
    '"'], glue(P), glue('"'), [means, in, this, context], glue('.').

glue(Token) --> ['_glue_', Token].


generate(Move, Sentence) :-
    ( is_list(Move) ->
        dcg_sequence(Move, Sequence),
        phrase(Sequence, Tokens)
    ;
        phrase(Move, Tokens)
    ),
    !,
    concat(Tokens, Raw),
    capitalize_first(Raw, Sentence).

dcg_sequence([X], X).
dcg_sequence([X|Xs], (X, Rest)) :-
    dcg_sequence(Xs, Rest).

concat([], '').
concat([X], X) :- !.
concat([X,'_glue_'|Xs], Result) :-
    !,
    concat(Xs, XsResult),
    atomic_list_concat([X, XsResult], Result).
concat([X|Xs], Result) :-
    concat(Xs, XsResult),
    atomic_list_concat([X, ' ', XsResult], Result).

capitalize_first(AtomIn, AtomOut) :-
    atom_chars(AtomIn, [First|Rest]),
    char_type(First, to_lower(Lower)),
    char_type(Cap, to_upper(Lower)),
    atom_chars(AtomOut, [Cap|Rest]).
