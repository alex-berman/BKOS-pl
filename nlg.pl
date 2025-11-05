:- module(nlg, [generate/2]).


assert(rel_prob(Event, R)) --> [there, is, a, R, probability, that], event(Event).

assert(value(P, V)) --> property(P), [is, V].

confirm(rel_prob(Event, R)) --> ['yes,', there, is, a, R, probability, that], event(Event).

disconfirm(rel_prob(Event, R)) --> ['no,', there, is, a, R, probability, that], event(Event).

event(satisfied(pat_1)) --> [the, patient, will, be, satisfied, with, surgery].

property(back_pain(pat_1)) --> [the, 'patient\'s', back, pain, level].


generate(Move, Sentence) :-
    phrase(Move, Words),
    atomic_list_concat(Words, ' ', Raw),
    capitalize_first(Raw, Capitalized),
    add_period(Capitalized, Sentence).

capitalize_first(AtomIn, AtomOut) :-
    atom_chars(AtomIn, [First|Rest]),
    char_type(First, to_lower(Lower)),
    char_type(Cap, to_upper(Lower)),
    atom_chars(AtomOut, [Cap|Rest]).

add_period(AtomIn, AtomOut) :-
    atom_concat(AtomIn, '.', AtomOut).
