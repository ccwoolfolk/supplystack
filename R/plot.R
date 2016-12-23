#' plot.supplystack
#'
#' Plot a supplystack object
#' @param x supplystack object to be plotted.
#' @param rect_col Optional character. Color of shaded rectangles or NULL for none.
#' @param rect_border Optional character. Color of rectangle borders or NA for none.
#' @param ... Other graphical arguments excluding "ann".
#' @export
#' @examples
#' plot(supplystack(c(10, 20, 30), c(30, 20, 10)), rect_border="darkblue")
plot.supplystack <- function(x, rect_col="lightblue", rect_border="white", ...) {

  if ("ann" %in% names(list(...)))
    stop("Do not specify \"ann\" in ... argument.")
  
  # Calculate cost stack dimensions
  n <- length(x$q)
  cumu_q <- c(0, cumsum(x$q))
  xleft <- head(cumu_q, n)
  xright <- tail(cumu_q, n)
  
  ybottom <- rep(0, n)
  ytop <- x$p
  
  # Plot the stack
  plot(x=NULL, y=NULL, xlim=c(0, tail(cumu_q, 1)), ylim=c(0, tail(x$p, 1)), ann=FALSE, ...)
  rect(xleft=xleft, xright=xright, ytop=ytop, ybottom=ybottom, col=rect_col, border=rect_border)
  
}