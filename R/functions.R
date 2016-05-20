#' Method for creating xyloplots
#'
#' Plots xylophone(s) for the input vector(s), provided either as a single vector or list of vectors. Either numeric vectors or character vectors/factors are admissible. If numeric vectors are provided, \code{cut} will be used to aggregate values, whereas if character vectors or factors are provided, each 'level' will get it's own 'key'. Note, that the height of the plot/length of the level labels may need adjusting to fit.
#'
#' @param x Vector or list of vectors to use for creating xyloplots.
#' @param ... Other arguments to be passed to \code{\link{xyloplot.list}} and \code{plot}.
#' @examples
#' xyloplot(rnorm(1000))
#' xyloplot(
#'  x=lapply(1:3, function(mean) rnorm(mean=mean, n=1000)), 
#'  breaks=20,
#'  col=rainbow(3), 
#'  main="title")
#' xyloplot(
#'  replicate(n=5, simplify=FALSE, 
#'   expr=factor(sample(c(0.01, 0.1, 0.2, 0.25, 0.5, 1), size=10, replace=TRUE))), 
#'  col=rainbow(5))
#' xyloplot(
#'  sample(c("goldfish","cat","dog","fish","mouse","giraffe"), 
#'  size=100, replace=TRUE))
#' @seealso xyloplot.list xyloplot.numeric
#' @export
xyloplot <- function(x, ...) {
	UseMethod("xyloplot")	
}

#' Function for creating xyloplots from factor vectors
#'
#' @param x Character vector of values.
#' @param ... Other arguments passed to \code{\link{xyloplot.list}}.
#'
#' @method xyloplot factor
#' @seealso xyloplot.list
#' @export
xyloplot.factor <- function(x, ...) {
	xyloplot.list(list(x), discrete=TRUE, ...)
}

#' Function for creating xyloplots from character vectors
#'
#' @param x Character vector of values.
#' @param ... Other arguments passed to \code{\link{xyloplot.list}}.
#'
#' @method xyloplot character
#' @seealso xyloplot.list
#' @export
xyloplot.character <- function(x, ...) {
	xyloplot.list(list(x), discrete=TRUE, ...)
}

#' Function for creating xyloplots from numeric vectors
#'
#' @param x Numeric vector of values.
#' @param ... Other arguments passed to \code{\link{xyloplot.list}}.
#'
#' @method xyloplot numeric
#' @seealso xyloplot.list
#' @export
xyloplot.numeric <- function(x, ...) {
	xyloplot.list(list(x), ...)
}

#' Function for creating multiple xyloplots sharing a y-axis from lists of numeric vectors
#'
#' @param x List of numeric vectors of values.
#' @param discrete Logical value indicating whether to treat values as discrete (suitable for character vectors and factors) or continuous (suitable for numeric vectors).
#' @param ylim Limits of hte y-axis.
#' @param breaks A single positive integer value giving the number of histogram classes to evenly split the values in \code{x} into, or a numeric vector explicitly giving the boundaries of the histogram classes.
#' @param space The proportion of the total distance on the x-axis allocated to each 'xylophone' which should be left blank.
#' @param ylab Label for y-axis.
#' @param xlab Label for x-axis.
#' @param ... Other arguments to be passed to \code{\link{plot}}.
#' @method xyloplot list
#' @export
#' @importFrom graphics axis box plot rect title par strwidth text
xyloplot.list <- function(x, discrete=!is.numeric(unlist(x, use.names=FALSE)), ylim=if (discrete) c(0.5, length(levels(factor(unlist(x, use.names=FALSE))))+0.5) else range(unlist(x, use.names=FALSE)), breaks=if (discrete) seq(from=0.5, by=1, length.out=length(levels(factor(unlist(x, use.names=FALSE))))+1) else 30, space=0.1, ylab="Value", xlab="Frequency density", ...) { 
	stopifnot(length(breaks) > 1 | (length(breaks) == 1 & breaks > 1))
	stopifnot(discrete | is.numeric(unlist(use.names=FALSE, x)))
	if (!discrete) stopifnot(diff(range(unlist(use.names=FALSE, x)))>0)

	brk.pts <- if (length(breaks) == 1) seq(from=ylim[1], to=ylim[2], length.out=breaks) else breaks
	
	blks <- lapply(FUN=function(x) as.array(table(x)/length(x)), X=lapply(if (discrete) lapply(x, function(xyl) as.integer(factor(xyl, levels=levels(factor(unlist(x, use.names=FALSE)))))) else x, cut, breaks=brk.pts, right=FALSE))

	if (discrete) {
		inches_to_lines <- ( par("mar") / par("mai") )[1]
		old_mar <- par("mar")
		on.exit(par(mar=old_mar))
		add_w <- max(strwidth(levels(factor(unlist(x, use.names=FALSE))), units="inches")) * inches_to_lines
		par(mar=ifelse(1:4 == 2, 3 + add_w, old_mar))
	}

	plot(x=NULL, xlim=0:1, xaxs="i", yaxs="i", xlab="", ylab="", ylim=if (is.null(ylim)) range(brk.pts) else ylim, axes=FALSE, ...)

	violin.width <- (1-space)/length(x)

	pivots <- seq(from=1/2/length(x),by=1/length(x),length.out=length(x))
	ord <- as.integer(t(matrix(1:(length(x) * (length(brk.pts)-1)), nrow=length(brk.pts)-1,ncol=length(x))))

	rect(
		xleft=(rep(pivots, each=length(brk.pts)-1)-unlist(blks)/max(unlist(blks))*violin.width/2)[ord],
		xright=(rep(pivots, each=length(brk.pts)-1)+unlist(blks)/max(unlist(blks))*violin.width/2)[ord],
		ybottom=(rep(brk.pts[-1], times=length(x))-rep(diff(brk.pts), times=length(x)))[ord],
		ytop=(rep(brk.pts[-1], times=length(x)))[ord],
		...
	)

	box()

	if (!is.null(names(x))) {
		axis(tick=FALSE, side=1,at=pivots,labels=names(x))
		title(xlab=xlab, line=3)
	} else {
		title(xlab=xlab, line=1)
	}

	if (discrete) {
		axis(side=2, at=brk.pts[-1]-0.5, labels=FALSE)
		text(y=brk.pts[-1]-0.5, x=0, pos=2, offset=1, xpd=TRUE, labels=levels(factor(unlist(x, use.names=FALSE))))
		title(ylab=ylab, line=2+add_w)
	} else {
		axis(side=2)
		title(ylab=ylab, line=3)
	}
}
