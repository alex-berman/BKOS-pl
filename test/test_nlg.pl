:- use_module(nlg).

nlg_test(Test, TestsPath) :-
    yaml_read(TestsPath, Tests),
    member(Test, Tests).

run_test(Test) :-
    ( is_list(Test.move) ->
        maplist({}/[A,T]>>atom_to_term(A,T,_), Test.move, Move)
    ;
        atom_to_term(Test.move, Move, _)
    ),
    generate(Move, ActualForm),
    atom_chars(ExpectedForm, Test.form),
    assertion(ExpectedForm == ActualForm).

:- begin_tests(nlg).

test(nlg, [forall(nlg_test(Test, 'test/nlg_coverage.yml'))]) :-
    run_test(Test).

:- end_tests(nlg).
