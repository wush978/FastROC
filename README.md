FastROC
=======

Fast Computation of ROC Curve in R


```r
n <- 10^3
pred.x <- rnorm(n)
true.x <- runif(n) < 1/(1 + exp(-pred.x))

suppressPackageStartupMessages({
    library(microbenchmark)
    library(ROCR)
    library(FastROC)
    library(pROC)
})

f.ROCR <- function() {
    pred <- prediction(pred.x, true.x)
    perf <- performance(pred, "auc")
    perf@y.values[[1]]
}

f.FastROC <- function() {
    roc <- FastROC::ROC(true.x, pred.x)
    auc.RScupio <- FastROC::AUC(roc$x, roc$y)
}

f.pROC <- function() {
    g <- pROC::roc(true.x, pred.x)
    auc.pROC <- auc(g)
}

microbenchmark(auc.ROCR <- f.ROCR(), auc.FastROC <- f.FastROC(), auc.pROC <- f.pROC())
```

```
## Unit: microseconds
##                        expr     min      lq  median      uq   max neval
##        auc.ROCR <- f.ROCR()  5126.0  6221.9  7332.0  9148.2 12764   100
##  auc.FastROC <- f.FastROC()   331.1   444.2   500.3   599.6  2390   100
##        auc.pROC <- f.pROC() 19774.7 25487.6 29021.3 33600.9 73644   100
```

```r

all.equal(auc.ROCR, auc.FastROC)
```

```
## [1] TRUE
```

```r
all.equal(auc.pROC[1], auc.FastROC)
```

```
## [1] TRUE
```

