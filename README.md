This repo contains a SWI-Prolog implementation and extension of BKOS (pronounced _because_), a dialogue manager for conversationally explainable AI (XAI) interfaces. For more information about BKOS, see the repo for the [Python implementation](https://github.com/alex-berman/BKOS).

# Key differences with Python implementation

* Prolog's flexible way of representing relations and propositions makes it **easier to represent and operate with richer semantics**. For example, one can easily distinguish between instance-level propositions (e.g. `extraverted(person_1)`, i.e. a particular person is extraverted) and general propositions (e.g. `supports(high_value(energy, X), extraverted(X))`, i.e. that if someone likes high-energy music supports, then that person is likely to be extraverted).

- In the Prolog implementation, the dialogue model explicitly represents not only the fact that certain premises support certain claims, but also **the nature of such support**. This makes it possible to **distinguish between, e.g., causal and statistical explanations**. In the example domain, this is illustrated by the fact that the system's knowledge about the relationship between music preferences and predicted personality is explicitly encoded as associative (rather than, e.g., causal). In other words, when the system expresses that it _associates_ a preference for danceable music with extraversion, then this surface realization reflects the actual semantics of the dialogue model. Arguably, this correspondence between surface form and semantics facilitates explanatory **faithfulness**.

- The syntax of the [rules](bkos.pl) that govern update of dialogue context is much more concise than in the Python implementation. In a rough comparison, the size of Prolog rules is less than half compared to Python (in terms of number of characters). Arguably, and perhaps more importantly, the Prolog implementation reflects the underlying theorical concepts with better transparency.

# Running the system

There is currently no interactive version of the system. However, [dialogue coverage tests](test/dialog_coverage.yml) can be validated by running

```
swipl -g run_tests -t halt test/test_bkos.pl
```

# Contact
For correspondence, please contact alexander.berman@gu.se
