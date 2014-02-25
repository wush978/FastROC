\name{AUC}
\alias{AUC}
\title{Area under the Curve of ROC}
\usage{
AUC(x, y)
}
\arguments{
  \item{x}{sorted numeric vector. The x-coordinates of the
  ROC curve.}

  \item{y}{numeric vector. The y-coordinates corresponding
  to \code{x} of the ROC curve.}
}
\value{
numeric value, the AUC of the ROC curve.
}
\description{
Area under the Curve of ROC
}
\examples{
n <- 10^5
pred.x <- rnorm(n)
true.x <- runif(n) < 1 / (1 + exp(-pred.x))
roc <- ROC(pred.x, true.x)
auc <- AUC(roc$x, roc$y)
}

