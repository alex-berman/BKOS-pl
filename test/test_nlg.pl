:- use_module(nlg).

nlg_test(Test, TestsPath) :-
    yaml_read(TestsPath, Tests),
    member(Test, Tests).

run_test(Test) :-
    atom_to_term(Test.move, Move, _),
    generate(Move, ActualForm),
    atom_chars(ExpectedForm, Test.form),
    assertion(ExpectedForm == ActualForm).

:- begin_tests(nlg).

test(nlg, [forall(nlg_test(Test, 'test/nlg.yml'))]) :-
    run_test(Test).

:- end_tests(nlg).
