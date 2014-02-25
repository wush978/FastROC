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
    library(caTools)
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

f.caTools <- function() {
    colAUC(pred.x, true.x)[1]
}

microbenchmark(auc.ROCR <- f.ROCR(), auc.FastROC <- f.FastROC(), auc.pROC <- f.pROC(), 
    auc.caTools <- f.caTools())
```

```
## Unit: microseconds
##                        expr     min      lq  median      uq   max neval
##        auc.ROCR <- f.ROCR()  5264.6  6116.8  6766.5  8374.7 13121   100
##  auc.FastROC <- f.FastROC()   334.9   420.6   475.6   550.5  2417   100
##        auc.pROC <- f.pROC() 23141.6 25761.0 28522.7 31067.3 75321   100
##  auc.caTools <- f.caTools()  1215.3  1558.0  1818.9  2044.0  5002   100
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

```r
all.equal(auc.caTools, auc.FastROC)
```

```
## [1] TRUE
```

