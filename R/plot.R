#' plot.supplystack
#'
#' Plot a supplystack object
#' @param x supplystack object to be plotted.
#' @param plot_names Optional boolean. Should names be plotted on the stack?
#' @param names_cutoff Optional numeric. Cutoff for plotting of names. Suppliers with total Q less than this proportion of the total will not be plotted. Defaults to 0.10.
#' @param rect_col Optional character. Color of shaded rectangles or NULL for none.
#' @param rect_border Optional character. Color of rectangle borders or NA for none.
#' @param ... Other graphical arguments excluding "ann".
#' @export
#' @examples
#' plot(supplystack(c(10, 20, 30), c(30, 20, 10)), rect_border="darkblue")
plot.supplystack <- function(x, plot_names=TRUE, names_cutoff=0.10, rect_col="lightblue", rect_border="white", ...) {

  if ("ann" %in% names(list(...)))
    stop("Do not specify \"ann\" in ... argument.")
  
  if (!is.numeric(names_cutoff) && names_cutoff >= 0 && names_cutoff <= 1.0)
    stop("\"nms_cutoff\" specified improperly.")
  
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
  
  # Plot names if necessary
  if (plot_names) {
    nms_idx <- x$q / sum(x$q) >= names_cutoff
    
    text(x=(xleft + xright)[nms_idx] / 2, y=0.05 * ytop[1], labels=x$nms[nms_idx], srt=90, adj=0)
  }
  
}