:- ensure_loaded(bkos).
:- use_module('test/dialog_testing').
:- begin_tests(dialog_coverage).

test(bkos, [forall(get_test(Test, 'test/dialog_coverage.yml'))]) :-
    run_test(Test).

:- end_tests(dialog_coverage).
