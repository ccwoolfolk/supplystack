#' supplystack Constructor
#'
#' Create a supplystack object
#' @param p Numeric vector. Represents cost or price for each producer.
#' @param q Numeric vector. Represents quantity for each producer.
#' @param nms Character vector (optional). Producer names for visualization.
#' @export
#' @examples
#' supplystack(p=c(100, 50, 75), q=rep(50, 3), nms=c('A','B','C'))
supplystack <- function(p, q, nms=NULL) {

  if (length(p) != length(q)) {
    stop(paste("'p' length of", length(p),
               " vs. 'q' length of", length(q)))
  }

  if (!is.null(nms) && length(nms) != length(p)) {
    stop(paste("'p' length of", length(p),
               " vs. 'nms' length of", length(nms)))
  }

  idx <- order(p, decreasing=FALSE)

  if (!is.null(nms)) nms <- nms[idx]

  newStack <- list(p=p[idx],
                   q=q[idx],
                   nms=nms)
  
  class(newStack) <- "supplystack"

  return(newStack)

}
