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
##                        expr     min      lq median      uq   max neval
##        auc.ROCR <- f.ROCR()  5947.6  8694.0  10763 12217.8 33495   100
##  auc.FastROC <- f.FastROC()   336.5   499.8    589   712.5  3546   100
##        auc.pROC <- f.pROC() 22048.3 33449.5  36683 42022.7 82188   100
##  auc.caTools <- f.caTools()  1412.6  2117.5   2524  3326.7 81345   100
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

```r

n <- 10^4
pred.x <- rnorm(n)
true.x <- runif(n) < 1/(1 + exp(-pred.x))

microbenchmark(times = 10, auc.ROCR <- f.ROCR(), auc.FastROC <- f.FastROC(), 
    auc.pROC <- f.pROC(), auc.caTools <- f.caTools())
```

```
## Unit: milliseconds
##                        expr      min       lq  median       uq      max
##        auc.ROCR <- f.ROCR()   64.181   83.225  110.66  126.421  142.236
##  auc.FastROC <- f.FastROC()    2.071    2.204    2.68    3.196    6.507
##        auc.pROC <- f.pROC() 1548.224 1629.045 1697.36 1777.592 1798.688
##  auc.caTools <- f.caTools()    8.053   10.973   14.03   15.455   16.778
##  neval
##     10
##     10
##     10
##     10
```

```r

n <- 10^5
pred.x <- rnorm(n)
true.x <- runif(n) < 1/(1 + exp(-pred.x))

microbenchmark(times = 10, auc.ROCR <- f.ROCR(), auc.FastROC <- f.FastROC(), 
    auc.caTools <- f.caTools())
```

```
## Warning: 整數向上溢位產生了 NA Warning: 整數向上溢位產生了 NA Warning:
## 整數向上溢位產生了 NA Warning: 整數向上溢位產生了 NA Warning:
## 整數向上溢位產生了 NA Warning: 整數向上溢位產生了 NA Warning:
## 整數向上溢位產生了 NA Warning: 整數向上溢位產生了 NA Warning:
## 整數向上溢位產生了 NA Warning: 整數向上溢位產生了 NA
```

```
## Unit: milliseconds
##                        expr    min     lq median     uq     max neval
##        auc.ROCR <- f.ROCR() 655.97 755.16  837.5 925.90 1147.94    10
##  auc.FastROC <- f.FastROC()  24.02  26.37   34.1  35.42   44.42    10
##  auc.caTools <- f.caTools() 102.72 128.49  135.4 171.37  216.43    10
```

```r
print(auc.ROCR)
```

```
## [1] 0.7415
```

```r
print(auc.FastROC)
```

```
## [1] 0.7415
```

```r
print(auc.caTools)
```

```
## [1] NA
```

```r

n <- 10^7
pred.x <- rnorm(n)
true.x <- runif(n) < 1/(1 + exp(-pred.x))
print(system.time(auc.ROCR <- f.ROCR()))
```

```
##    user  system elapsed 
##  82.500   3.443  93.354
```

```r
print(system.time(auc.FastROC <- f.FastROC()))
```

```
##    user  system elapsed 
##   4.798   0.230   5.345
```

