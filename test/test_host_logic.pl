:- ensure_loaded(bkos_host_logic).
:- ensure_loaded(isu_syntax).


set_facts(Facts) :-
    retractall(@_),
    forall(member(Fact, Facts), assertz(@Fact)).


test_valid_answers(Facts, Q, Expected) :-
    set_facts(Facts),
    findall(A, valid_answer(Q, A), Actual),
    assertion(Actual =@= Expected).


:- begin_tests(host_logic).


test(valid_answer_for_polar_questions) :-
    test_valid_answers([], []>>p, []),
    test_valid_answers([p, not(p)], []>>p, [p, not(p)]).


test(valid_answer_for_basic_support) :-
    % evidence question
    test_valid_answers(
        [],
        [E, M]>>supports(E, c(x), M),
        []),
    test_valid_answers(
        [
            e(x),
            c(x),
            supports(e(X), c(X), m)
        ],
        [E, M]>>supports(E, c(x), M),
        [
            e(x),
            supports(e(Y), c(Y), m)
        ]),
    
    % claim question
    test_valid_answers(
        [],
        [C, M]>>supports(e(x), C, M),
        []
    ),
    test_valid_answers(
        [
            e(x),
            c(x),
            supports(e(X), c(X), m)
        ],
        [C, M]>>supports(e(x), C, M),
        [c(x)]
    ),

    % means question
    test_valid_answers(
        [],
        [M]>>supports(e(x), c(x), M),
        []
    ),
    test_valid_answers(
        [supports(e(X), c(X), m)],
        [M]>>supports(e(x), c(x), M),
        [supports(e(X), c(X), m)]
    ),

    % polar question
    test_valid_answers(
        [],
        []>>supports(e(X), c(X), _),
        []
    ),
    test_valid_answers(
        [supports(e(X), c(X), m)],
        []>>supports(e(X), c(X), _),
        [supports(e(X), c(X), m)]
    ).


test(valid_answer_for_chained_support) :-
    test_valid_answers(
        [
            e(x),
            c(x),
            c1(x),
            supports(e(X), c1(X), m),
            supports(c1(X), c(X), m)
        ],
        [E, M]>>supports(E, c(x), M),
        [
            c1(x),
            e(x),
            supports(c1(Y), c(Y), m)
        ]
    ),
    test_valid_answers(
        [
            e(x),
            c(x),
            c1(x),
            supports(e(X), c1(X), m),
            supports(c1(X), c(X), m)
        ],
        [C, M]>>supports(e(x), C, M),
        [
            c1(x),
            c(x)
        ]
    ).


test(valid_answer_for_basic_lookup) :-
    test_valid_answers(
        [value(back_pain, patient, 4)],
        [V]>>value(back_pain, patient, V),
        [value(back_pain, patient, 4)]
    ).


test(remove_pragmatical_redundance) :-
    set_facts([
        supports(e(X), c(X), m)
        ]),
    Q = [E, M]>>supports(E, c(x), M),
    R = _{q:Q},
    remove_pragmatical_redundance(R, [supports(e(X), c(X), m), e(x)], Actual), !,
    Expected = [supports(e(Y), c(Y), m)],
    assertion(Actual =@= Expected).


:- end_tests(host_logic).
