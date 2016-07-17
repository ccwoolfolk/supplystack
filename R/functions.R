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

  return(newStack)

}
