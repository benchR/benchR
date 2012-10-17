surv-bench.r
------------

Applies one of three algorithms on a simulated or real world data set.

Command line options:

data [character(1) %in% c("real", "sim")]
  Use real data set (mainz cohort, GSE11121) or simulate a data set. 
  If data == "sim" you may further pass "n" (number observations) and "p"
  (number covariates/genes) for the dimensions of the simulated gene
  expression matrix and survival times.
  Defaults to "unisel" with n=200 and p=100.

fun [character(1) %in% c("unisel", "penalizedL1", "coxboost")]
  Model/Learning algorithm to use. Univariate selection ("unisel"), Cox Lasso
  Regression ("penalizedL1") or a boosted Cox model.
  Defaults to "unisel".

m [integer(1)]
  Number of splits into training and test set. 
  Defaults to 1.


Exemplary call of the script:
Rscript surv-bench.r -data sim -n 200 -p 100 -fun unisel -m 1
