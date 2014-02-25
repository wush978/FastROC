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
##        auc.ROCR <- f.ROCR()  5607.4  5754.0  5988.9  7104.0 35115   100
##  auc.FastROC <- f.FastROC()   370.1   437.2   480.8   523.6  1564   100
##        auc.pROC <- f.pROC() 18607.8 19713.7 20156.0 21208.3 49445   100
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

