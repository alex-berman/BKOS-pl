This repo contains a SWI-Prolog implementation and extension of BKOS (pronounced _because_), a dialogue manager for conversationally explainable AI (XAI) interfaces. For more information about BKOS, see the repo for the [Python implementation](https://github.com/alex-berman/BKOS).

# Key differences with Python implementation

The syntax of the [rules](bkos.pl) that govern update of dialogue context is much more concise than in the Python implementation. In a rough comparison, the size of Prolog rules is less than half compared to Python (in terms of number of characters). Arguably, and perhaps more importantly, the Prolog implementation reflects the underlying theorical concepts with better transparency.

# Running the system

There is currently no interactive version of the system. However, [dialogue coverage tests](test/dialog_coverage.yml) can be validated by running

```
swipl -g run_tests -t halt test/test_bkos.pl
```

# Contact
For correspondence, please contact alexander.berman@gu.se
