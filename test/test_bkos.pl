:- ensure_loaded(bkos).
:- use_module('test/dialog_testing').

:- begin_tests(dialog_coverage).

test(thesis, [forall(get_test(TestAsDict, 'test/dialog_coverage_thesis.yml'))]) :-
    run_test_from_dict(TestAsDict).

test(various, [forall(get_test(TestAsDict, 'test/dialog_coverage_various.yml'))]) :-
    run_test_from_dict(TestAsDict).

:- end_tests(dialog_coverage).
