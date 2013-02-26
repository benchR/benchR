# ------------------------------------------------------------------
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------

getSimulatedData <- function(n = 200L, p = 100L) {
  set.seed(20120222)
  x <- matrix(rnorm(n * p), n, p)
  colnames(x) <- sprintf(paste("gene%0", nchar(p), "i", sep = ""), seq_len(p))


  lambda <- 10
  beta <- c(rep(1, 10L), rep(0, p - 10L))
  real.time <- -(log(runif(n))) / (lambda * exp(drop(x %*% beta)))
  cens.time <- rexp(n, rate = 1/lambda)
  status <- real.time <= cens.time
  obs.time <- ifelse(status, real.time, cens.time)
  surv <- Surv(obs.time, status)

  return(list(surv = surv, expr = x))
}

getRealData <- function() {
  load("mk.clinical.RData")
  load("mk.genes.RData")
  surv <- with(clinical, Surv(surv.time, surv.status))

  return(list(surv = surv, expr = t(exprmat)))
}

evalRiskScore <- function(surv, expr, beta, test) {
    RS <- drop(expr[test, names(beta), drop = FALSE] %*% beta)
    mod <- try(coxph(surv[test] ~ RS))
    if(inherits(mod, "try-error"))
      return(NA)
    return(1 - pchisq(mod$score, df = 1L))
}


penalizedL1 = function(surv, expr) {
  prof <- profL1(response = surv, penalized = expr, fold = 10L, trace = FALSE)
  lambda.opt <- prof$lambda[which.max(prof$cvl)]
  steplength <- abs(prof$lambda[2L] - prof$lambda[1L])
  opt <- optL1(response = surv, penalized = expr, fold = 10L,
               minlambda = lambda.opt - 2 * steplength,
               maxlambda = lambda.opt + 2 * steplength,
               trace = FALSE)

  beta <- coef(opt$fullfit)
  return(beta)
}

coxboost <- function(surv, expr) {
  optim <- optimCoxBoostPenalty(time = surv[, 1L], status = surv[, 2L],
                               x = expr, iter.max = 30L)
  cb <- CoxBoost(time = surv[, 1L], status = surv[, 2L],
                x = expr, stepno = optim$cv.res$optimal.step)

  # find betas != 0
  tol <- sqrt(.Machine$double.eps)
  active <- abs(cb$coefficients[cb$stepno + 1L, ]) > tol
  coefnames <- cb$xnames[active]
  beta <- cb$coefficients[cb$stepno + 1L, ][active]
  names(beta) <- coefnames
  return(beta)
}


### univariate selection
unisel = function(surv, expr) {
  # calculate cross validated partial likelihood
  cvpl <- function(surv, x, folds = ncol(x)) {
    time <- surv[, 1L]
    status <- as.logical(surv[, 2L])

    # cross validation groups
    groups <- (sample(nrow(x)) %% folds) + 1L

    # matrix of risksets
    RS <- outer(time, time, "<=")

    # save each folds difference in partial likelihoods
    diffs <- numeric(folds)

    for(k in seq_len(folds)) {
      # training group for this fold
      train <- (groups != k)

      # try to fit a model
      m <- try(suppressWarnings(coxph(surv ~ x, subset = train)))

      if(inherits(m, "try-error") || any(is.na(coef(m)))) {
        # -> most likely singularities
        return(NA)
      }

      # difference between PL on data and PL on data with k-th fold left out
      expBetaZ <- exp(as.numeric(coef(m) %*% t(x)))
      diffs[k] <- (sum(log(expBetaZ[status] / (RS[status, , drop = FALSE] %*% expBetaZ)), na.rm = TRUE)
                   - sum(log(expBetaZ[train & status] / (RS[train & status, train, drop = FALSE] %*% expBetaZ[train])), na.rm = TRUE))
    }

    # sum of differences in each fold -> cvpl
    return(sum(diffs))
  }

  uni.score <- function(surv, x) coxph(surv ~ x)$score
  ord <- order(apply(expr, 2L, uni.score, surv = surv), decreasing = TRUE)

  # calculate cross validated likelihood for [i] best genes
  # stop when getting problems with singularities
  cvl <- rep(NA, length(ord))
  for(i in seq_along(ord)) {
    cvl[i] <- cvpl(surv, expr[, head(ord, i), drop = FALSE], folds = 10L)
    if(is.na(cvl[i]))
      break
  }

  # get best model formula and corresponding model
  f <- reformulate(head(colnames(expr)[ord], which.max(cvl)), response = "surv")
  mod <- tryCatch(coxph(f, data = as.data.frame(expr)),
                  error = function(e) stop("Error fitting final model (", e, ")"))
  beta <- coef(mod)
  return(beta)
}


library(survival)
library(penalized)
library(CoxBoost)


### set default options
data <- "sim"
n <- 200L
p <- 100L
m <- 1L
fun <- "unisel"

### fetch command line args and overwrite defaults
args <- commandArgs(TRUE)
if(length(args) > 0L) {
    for(i in seq(length(args)-1)) {
       param = args[[ i ]]
       value = args[[ i+1 ]]
			 if (param == "-data") data = value
       if (param == "-n") n = as.integer(value)
       if (param == "-p") p = as.integer(value)
       if (param == "-m") m = as.integer(value)
       if (param == "-fun") fun = value
    }
}

### check command line args
stopifnot(data %in% c("sim", "real"))
stopifnot(! (data == "sim" && ! is.numeric(n)))
stopifnot(! (data == "sim" && ! is.numeric(p)))
stopifnot(is.numeric(m))
stopifnot(fun %in% c("penalizedL1", "unisel", "coxboost"))
learner <- match.fun(fun)

### simulate/create data
if(data == "sim") {
  data <- getSimulatedData(n, p)
  message(sprintf("Using simulated data with n=%i and p=%i", n, p))
} else if(data == "real") {
  data <- getRealData()
  message("Using real data from mainz cohort")
}

### pre-allocation of result containers
pval <- time <- as.numeric(rep(NA, m))
beta <- as.list(pval)


### iterate over number of splits m
for(i in seq_len(m)) {
  now <- as.numeric(Sys.time())
  cat(sprintf("Running split %i/%i with %s ... ", i, m, fun))

  ### split in training and test
  set.seed(i)
  train <- sample(nrow(data$surv), floor(nrow(data$surv) * 2/3))
  test <- setdiff(seq(nrow(data$surv)), train)

  ### fit the model
  b <- try(learner(data$surv[train], data$expr[train, ]))

  ### check for successful model fit and evaluate
  if(! inherits(b, "try-error")) {
    beta[[i]] <- b
    pval[i] = evalRiskScore(data$surv, data$expr, b, test)
    time[i] = as.numeric(Sys.time() - now)
    cat(sprintf("finished after %.04f seconds\n", time[i]))
  } else {
    cat("Error!\n")
  }
}

### print summary table
print(data.frame(num.covars = sapply(beta, length), pval = pval, time = time))
