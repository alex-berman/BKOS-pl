:- module(nlg, [generate/2]).


assert(rel_prob(Event, R)) --> [there, is, a, R, probability, that], event(Event).

assert(value(P, V)) --> property(P), [is, V].

assert(value(has_other_illness(pat_1), true)) --> [the, patient, has, other, illnesses].

assert(value(has_other_illness(pat_1), false)) --> [the, patient, has, no, other, illnesses].

assert([X]) --> assert(X).

assert([X,Y]) --> assert(X), [and], assert(Y).

assert([X,Y|Rest]) --> assert(X), [','], assert(Y), [and], assert(Rest).

confirm(rel_prob(Event, R)) --> ['yes,', there, is, a, R, probability, that], event(Event).

disconfirm(rel_prob(Event, R)) --> ['no,', there, is, a, R, probability, that], event(Event).

event(satisfied(pat_1)) --> [the, patient, will, be, satisfied, with, surgery].

property(back_pain(pat_1)) --> [the, 'patient\'s', back, pain, level].

property(disability(pat_1)) --> [the, 'patient\'s', disability].


generate(Move, Sentence) :-
    phrase(Move, Words1), !,
    normalize(Words1, Words),
    atomic_list_concat(Words, ' ', Raw),
    capitalize_first(Raw, Capitalized),
    add_period(Capitalized, Sentence).

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
