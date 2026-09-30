* This is a regular feature and bug-fix release. There are no reverse
  dependencies.

* The minimum `climenu` version in Imports is raised to 0.2.0, which is on
  CRAN. `puniform` is added to Suggests and is only used when it is
  installed.

* A contributor (Matyáš Tvrz, ctb) is added to Authors@R for the
  cluster-robust standard errors in frequentist model averaging.

* Some reported numbers change by design. Bayesian and frequentist model
  averaging now report coefficients on the data scale rather than the
  standardized one, winsorization uses order statistics instead of
  interpolated quantiles, and the p-hacking tests run on unwinsorized data.
