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

  # Send to alternative constructor if multiple cost components
  if (is.matrix(p))
    return(ssmulti(p=p, q=q, nms=nms))
  
  # Input validation
  if (length(p) != length(q)) {
    stop(paste("'p' length of", length(p),
               " vs. 'q' length of", length(q)))
  }

  if (!is.null(nms) && length(nms) != length(p)) {
    stop(paste("'p' length of", length(p),
               " vs. 'nms' length of", length(nms)))
  }
  
  if (any(is.na(p)) || any(is.na(q)))
    stop("NAs not allowed in p or q arguments")

  # Create supplystack object
  idx <- order(p, decreasing=FALSE)

  if (!is.null(nms)) nms <- nms[idx]

  newStack <- list(p=p[idx],
                   q=q[idx],
                   nms=nms)
  
  class(newStack) <- "supplystack"

  return(newStack)

}


#' ssmulti Constructor
#'
#' Create a supplystack object with multiple cost components
#' @param p Numeric matrix. Represents cost or price for each producer. Column values belong to a single producer; rows represent different cost components.
#' @param q Numeric vector. Represents quantity for each producer.
#' @param nms Character vector (optional). Producer names for visualization.
ssmulti <- function(p, q, nms=NULL) {
  stopifnot(is.matrix(p))
  
  total_p <- colSums(p)
  idx <- order(total_p)
  
  stack <- supplystack(p=total_p[idx], q=q[idx], nms=nms[idx])
  class(stack) <- append("ssmulti", class(stack))
  stack[["components"]] <- p[ ,idx]
  return(stack)
  
}

#' supplystack Addition
#'
#' Combine two supplystack objects
#' @param a supplystack object.
#' @param b supplystack object.
#' @export
#' @examples
#' supplystack(p=c(100, 50), q=rep(50, 2)) + supplystack(p=c(75, 150), q=rep(20, 2))
"+.supplystack" <- function(a, b) {
  c <- supplystack(p=c(a$p, b$p),
                   q=c(a$q, b$q),
                   if(!is.null(a$nms) && !is.null(b$nms)) nms=c(a$nms, b$nms))
  return(c)
}

#' ssmulti Addition
#'
#' Combine two multi component supplystack objects
#' @param a ssmulti object.
#' @param b ssmulti object.
"+.ssmulti" <- function(a, b) {
  
  if (is.null(a$components) || is.null(b$components))
    stop("Inputs must both be multi cost component stacks")
  
  if (!identical(nrow(a$components), nrow(b$components)))
    stop("Inputs must both have identical number of cost components")
  
  if (!identical(rownames(a$components), rownames(b$components)))
    stop("Cannot add cost components with different names")
  
  c <- supplystack(p=cbind(a$components, b$components),
                   q=c(a$q, b$q),
                   if(!is.null(a$nms) && !is.null(b$nms)) nms=c(a$nms, b$nms))
  return(c)
}