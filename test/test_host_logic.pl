:- ensure_loaded(bkos_host_logic).
:- ensure_loaded(isu_syntax).
- ensure_loaded(db).


given_db(Facts) :-
    retractall(@_),
    forall(member(Fact, Facts), assertz(@Fact)).


test_valid_answers(Q, Expected) :-
    findall(A, valid_answer(Q, A), Actual),
    assertion(Actual =@= Expected).


:- begin_tests(host_logic).


test(valid_answer_for_polar_questions) :-
    test_valid_answers(p, [p, not(p)]).


test(valid_answer_for_supports) :-
    given_db([
        supports(e(X), c(X), m)
        ]),
    test_valid_answers([E, M]^supports(E, c(x), M), [
        e(x),
        supports(e(Y), c(Y), m)
    ]),
    test_valid_answers([C, M]^supports(e(x), C, M), [
        c(x)
    ]).


test(valid_answer_for_basic_lookup) :-
    given_db([
        value(back_pain, patient, 4)
        ]),
    test_valid_answers([V]^value(back_pain, patient, V), [value(back_pain, patient, 4)]).


test(remove_pragmatical_redundance) :-
    given_db([
        supports(e(X), c(X), m)
        ]),
    Q = [E, M]^supports(E, c(x), M),
    R = _{q:Q},
    remove_pragmatical_redundance(R, [supports(e(X), c(X), m), e(x)], Actual), !,
    Expected = [supports(e(Y), c(Y), m)],
    assertion(Actual =@= Expected).


:- end_tests(host_logic).
