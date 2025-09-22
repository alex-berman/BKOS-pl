:- ensure_loaded(bkos).
:- use_module('test/dialog_testing').

:- begin_tests(dialog_coverage).

test(coverage, [forall(get_test(TestAsDict, 'test/dialog_coverage.yml'))]) :-
    run_test_from_dict(TestAsDict).

:- end_tests(dialog_coverage).
