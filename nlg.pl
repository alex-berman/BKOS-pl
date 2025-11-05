:- module(nlg, [generate/2]).


assert(P) --> proposition(P).

proposition(rel_prob(Event, R)) --> [there, is, a, R, probability, that], event(Event).
proposition(value(P, V)) --> property(P), [is, V].
proposition(value(has_other_illness(pat_1), true)) --> [the, patient, has, other, illnesses].
proposition(value(has_other_illness(pat_1), false)) --> [the, patient, has, no, other, illnesses].
proposition(rel_value(back_pain(pat_1), V)) -->
    [the, patient, has, relatively], back_pain_level(V), [back, pain].
proposition(rel_value(disability(pat_1), V)) --> [the, patient, has, relatively, V, disability].
proposition([X]) --> proposition(X).
proposition([X,Y]) --> proposition(X), [and], proposition(Y).
proposition([X,Y|Rest]) --> proposition(X), [','], proposition(Y), [and], proposition(Rest).
proposition(supports(A, C, association)) --> ['I', associate], concept(A), [with], concept(C).
proposition(surgery_recommended(pat_1)) --> [surgery, is, recommended, for, the, patient].
proposition(not(surgery_recommended(pat_1))) --> [surgery, is, not, recommended, for, the, patient].

confirm(P) --> ['yes,'], proposition(P).
disconfirm(P) --> ['no,'], proposition(P).

event(satisfied(pat_1)) --> [the, patient, will, be, satisfied, with, surgery].

property(back_pain(pat_1)) --> [the, 'patient\'s', back, pain, level].
property(disability(pat_1)) --> [the, 'patient\'s', disability].

concept(rel_value(back_pain(_), V)) --> back_pain_level(V), [back, pain].
concept(rel_value(disability(_), V)) --> [V, disability].
concept(value(has_other_illness(_), V)) --> presence_or_absense(V), [of, other, illnesses].
concept(rel_prob(satisfied(_), R)) --> [a, R, probability, of, being, satisfied, with, surgery].

back_pain_level(low) --> [mild].
back_pain_level(high) --> [severe].
presence_or_absense(false) --> [absense].
presence_or_absense(true) --> [presence].

signal_resumption --> [also, ','].

infer(A, C) --> [since], proposition(A), [','], proposition(C).

icm(acceptance, negative, lack_knowledge) --> ['I', 'don\'t', have, any, information, about, that].


generate(Move, Sentence) :-
    ( is_list(Move) ->
        dcg_sequence(Move, Sequence),
        phrase(Sequence, Words1)
    ;
        phrase(Move, Words1)
    ),
    !,
    normalize(Words1, Words),
    atomic_list_concat(Words, ' ', Raw),
    capitalize_first(Raw, Capitalized),
    add_period(Capitalized, Sentence).

dcg_sequence([X], X).
dcg_sequence([X|Xs], (X, Rest)) :-
    dcg_sequence(Xs, Rest).

normalize([X, ','|RestIn], [X1|RestOut]) :-
    !,
    atom_concat(X, ',', X1),
    normalize(RestIn, RestOut).

normalize([], []).

normalize([X|RestIn], [X|RestOut]) :-
    normalize(RestIn, RestOut).

capitalize_first(AtomIn, AtomOut) :-
    atom_chars(AtomIn, [First|Rest]),
    char_type(First, to_lower(Lower)),
    char_type(Cap, to_upper(Lower)),
    atom_chars(AtomOut, [Cap|Rest]).

add_period(AtomIn, AtomOut) :-
    atom_concat(AtomIn, '.', AtomOut).
