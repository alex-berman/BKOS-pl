:- ensure_loaded(bkos_host_logic).
:- ensure_loaded(isu_syntax).
:- ensure_loaded(db).

given_db(Facts) :-
    retractall(@_),
    forall(member(Fact, Facts), assertz(@Fact)).

:- begin_tests(host_logic).

test(valid_answer_for_polar_questions) :-
    assertion(valid_answer(p, p)),
    assertion(valid_answer(p, not(p))),
    assertion(\+ valid_answer(p, a)),
    assertion(\+ valid_answer(p, not(a))).

test(valid_answer_for_support) :-
    given_db([
        supports(e, c, m)
        ]),
    assertion(valid_answer([E, M]^supports(E, c, M), e)),
    assertion(valid_answer([E, M]^supports(E, c, M), supports(e, c, m))),
    assertion(valid_answer([C, M]^supports(e, C, M), c)).
    % TODO?
    %assertion(valid_answer([M]^supports(e, c, M), supports(e, c, m))).

:- end_tests(host_logic).