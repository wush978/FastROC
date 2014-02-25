\name{ROC}
\alias{ROC}
\title{Fast Computation of the Receiver Operating Characteristic Curve (ROC)}
\usage{
  ROC(y.true, y.estimate, resize = NULL)
}
\arguments{
  \item{y.true}{logical vector. The answer of the
  classification problem.}

  \item{y.estimate}{numeric vector. The ranking of the
  predicted result corresponding to \code{Ranswer}.}

  \item{resize}{integer vector or \code{NULL}(default). If
  \code{is.null(resize)}, the complete ROC curve is
  returned. Otherwise, the number of returned points is
  down sampled to \code{resize}.}
}
\value{
  \code{list(x, y)}. The x-coordinate and y-coordinate of
  the ROC curve.
}
\description{
  Fast Computation of the Receiver Operating Characteristic
  Curve (ROC)
}
\examples{
n <- 10^5
pred.x <- rnorm(n)
true.x <- runif(n) < 1 / (1 + exp(-pred.x))
roc <- ROC(true.x, pred.x)
auc <- AUC(roc$x, roc$y)
}

