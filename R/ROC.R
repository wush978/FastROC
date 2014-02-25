#'@title Fast Computation of the Receiver Operating Characteristic Curve (ROC)
#'@param y.true logical vector. The answer of the classification problem.
#'@param y.estimate numeric vector. The ranking of the predicted result corresponding to \code{Ranswer}.
#'@param resize integer vector or \code{NULL}(default). If \code{is.null(resize)}, the complete ROC curve is returned.
#'Otherwise, the number of returned points is down sampled to \code{resize}.
#'@return \code{list(x, y)}. The x-coordinate and y-coordinate of the ROC curve. 
#'@export
ROC <- function(y.true, y.estimate, resize = NULL) {
  y.true <- as.integer(!y.true)
  y.estimate <- as.numeric(y.estimate)
  if (is.null(resize)) return(.Call("ROC", y.true, y.estimate))
  retval <- .Call("ROC", y.true, y.estimate)
  index <- floor(seq(from = 1, to = length(retval$x), length = resize))
  list(x=retval$x[index], y=retval$y[index])
}