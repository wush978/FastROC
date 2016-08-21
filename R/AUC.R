#'@title Area under the Curve of ROC
#'@param x sorted numeric vector. The x-coordinates of the ROC curve. 
#'@param y numeric vector. The y-coordinates corresponding to \code{x}
#'of the ROC curve.
#'@return numeric value, the AUC of the ROC curve.
#'@examples
#'n <- 10^5
#'pred.x <- rnorm(n)
#'true.x <- runif(n) < 1 / (1 + exp(-pred.x))
#'roc <- ROC(true.x, pred.x)
#'auc <- AUC(roc$x, roc$y)
#'@importFrom stats filter
#'@export
AUC <- function(x, y) {
  sum(diff(x) * filter(y, c(0.5, 0.5))[-length(y)])
}

#'@param y.true logical vector. The answer of the classification problem.
#'@param y.estimate numeric vector. The ranking of the predicted result corresponding to \code{Ranswer}.
#'@param resize integer vector or \code{NULL}(default). If \code{is.null(resize)}, the complete ROC curve is returned.
#'@export
AUC2 <- function(y.true, y.estimate, resize = NULL) {
  roc <- ROC(y.true, y.estimate, resize)
  AUC(roc$x, roc$y)
}
