:- ensure_loaded(bkos).
:- use_module('test/dialog_testing').

run_test(Name) :-
    run_test('test/dialog_coverage.yml', Name).

:- begin_tests(dialog_coverage).

test(bkos, [forall(get_test(TestAsDict, 'test/dialog_coverage.yml'))]) :-
    run_test_from_dict(TestAsDict).

:- end_tests(dialog_coverage).
