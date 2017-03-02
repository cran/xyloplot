#' Get x-axis positions of \code{n} xylophones
#'
#' @param n Number of xylophones
#' @export
xylo_positions <- function(n) {
	seq(from=1/2/n,by=1/n,length.out=n)
}

#' Method for creating xyloplots
#'
#' Plots xylophone(s) [essentially vertical histograms] for the input vector(s), provided either as a single vector or list of vectors. Either numeric vectors or character vectors/factors are admissible. If numeric vectors are provided, \code{cut} will be used to aggregate values, whereas if character vectors or factors are provided, each 'level' will get it's own `key'. Note, that the height of the plot/length of the level labels may need adjusting to fit. A second vector or list of vectors can be provided as the \code{rhs} argument. If so, `split' xyloplots are created, with the left hand sides corresponding to distributions in \code{x} and right hand sides corresponding to distributions in \code{rhs}. Note that if \code{rhs} is not null, it must have the same class as \code{x}, and if it is a \code{list}, it must be the same length as \code{x} and it's elements must be the same class as those in \code{x}.
#'
#' @param x Vector or list of vectors to use for creating xyloplots.
#' @template rhs
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
xyloplot <- function(x, rhs=NULL, ...) {
	UseMethod("xyloplot")	
}

#' Function for creating xyloplots from factor vectors
#'
#' @param x Character vector of values.
#' @template rhs
#' @param ... Other arguments passed to \code{\link{xyloplot.list}}.
#'
#' @method xyloplot factor
#' @seealso xyloplot.list
#' @export
xyloplot.factor <- function(x, rhs=NULL, ...) {
	if (!is.null(rhs)) {
		msg <- "If specified, the 'rhs' argument must be a factor with the same levels as 'x'"
		if (class(rhs) != "factor") stop(msg)
		if (!identical(levels(rhs), levels(x))) stop(msg)
	}
	xyloplot.list(x=list(x), rhs=list(rhs), discrete=TRUE, ...)
}

#' Function for creating xyloplots from character vectors
#'
#' @param x Character vector of values.
#' @template rhs
#' @param ... Other arguments passed to \code{\link{xyloplot.list}}.
#'
#' @method xyloplot character
#' @seealso xyloplot.list
#' @export
xyloplot.character <- function(x, rhs=NULL, ...) {
	xyloplot.list(x=list(x), rhs=list(rhs), discrete=TRUE, ...)
}

#' Function for creating xyloplots from numeric vectors
#'
#' @param x Numeric vector of values.
#' @template rhs
#' @param ... Other arguments passed to \code{\link{xyloplot.list}}.
#'
#' @method xyloplot numeric
#' @seealso xyloplot.list
#' @export
xyloplot.numeric <- function(x, rhs=NULL, ...) {
	xyloplot.list(x=list(x), rhs=list(rhs), ...)
}

#' Function for creating multiple xyloplots sharing a y-axis from lists of numeric vectors
#'
#' @param x List of numeric vectors of values.
#' @template rhs
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
xyloplot.list <- function(
	x, 
	rhs=NULL,
	discrete=!is.numeric(unlist(c(x, rhs), use.names=FALSE)), 
	breaks=if (discrete) seq(from=0.5, by=1, length.out=length(levels(factor(unlist(c(x, rhs), use.names=FALSE))))+1) else 30, 
	ylim=if (discrete) { range(breaks) } else { if (length(breaks) == 1) range(unlist(c(x, rhs), use.names=FALSE)) + c(-1, 1) * diff(range(unlist(c(x, rhs), use.names=FALSE)))/((breaks-1)*2) else range(breaks) }, 
	space=0.1, 
	ylab="Value", 
	xlab="Frequency density", 
	...
) { 

	unlisted <- unlist(x, use.names=FALSE)

	if (!is.null(rhs)) {
		msg <- "If specified, 'rhs' argument must be a list with the same length as 'x'"
		if (!is.list(rhs)) stop(msg)
		if (!length(rhs) == length(x)) stop(msg)
	}

	stopifnot(length(breaks) > 1 | (length(breaks) == 1 & breaks > 1 & (breaks - floor(breaks)) == 0))
	stopifnot(discrete | is.numeric(unlisted))
	if (!discrete) stopifnot(diff(range(unlisted))>0)

	brk.pts <- if (length(breaks) == 1) seq(from=ylim[1], by=diff(range(ylim))/breaks, length.out=breaks+1) else breaks
	
	if (discrete) {
		inches_to_lines <- ( par("mar") / par("mai") )[1]
		old_mar <- par("mar")
		on.exit(par(mar=old_mar))
		add_w <- max(strwidth(levels(factor(unlisted)), units="inches")) * inches_to_lines
		par(mar=ifelse(1:4 == 2, 3 + add_w, old_mar))
	}

	if ("xlim" %in% names(list(...)))
		stop("xyloplot does not accept an 'xlim' argument")

	plot(x=NULL, xlim=0:1, xaxs="i", yaxs="i", xlab="", ylab="", ylim=ylim, axes=FALSE, ...)

	violin.width <- (1-space)/length(x)

	pivots <- xylo_positions(length(x))
	ord <- as.integer(t(matrix(1:(length(x) * (length(brk.pts)-1)), nrow=length(brk.pts)-1,ncol=length(x))))

	make_blocks <- function(x) lapply(FUN=function(x) as.array(table(x)/length(x)), X=lapply(if (discrete) lapply(x, function(xyl) as.integer(factor(xyl, levels=levels(factor(unlisted))))) else x, cut, breaks=brk.pts, right=FALSE))
	blks <- make_blocks(x)

	if (is.null(unlist(rhs))) {
		rect(
			xleft=(rep(pivots, each=length(brk.pts)-1)-unlist(blks)/max(unlist(blks))*violin.width/2)[ord],
			xright=(rep(pivots, each=length(brk.pts)-1)+unlist(blks)/max(unlist(blks))*violin.width/2)[ord],
			ybottom=(rep(brk.pts[-1], times=length(x))-rep(diff(brk.pts), times=length(x)))[ord],
			ytop=(rep(brk.pts[-1], times=length(x)))[ord],
			...
		)
	} else {
		rhs_blks <- make_blocks(rhs)
		ord2 <- as.integer(rbind(ord, ord + max(ord)))
		rect(
			xleft=c((rep(pivots, each=length(brk.pts)-1)-0.5*unlist(blks)/max(unlist(c(blks, rhs_blks)))*violin.width),rep(pivots, each=length(brk.pts)-1))[ord2],
			xright=c(rep(pivots, each=length(brk.pts)-1),(rep(pivots, each=length(brk.pts)-1)+0.5*unlist(rhs_blks)/max(unlist(c(blks, rhs_blks)))*violin.width))[ord2],
			ybottom=rep((rep(brk.pts[-1], times=length(x))-rep(diff(brk.pts), times=length(x))), 2)[ord2],
			ytop=rep((rep(brk.pts[-1], times=length(x))),2)[ord2],
			...
		)
	}

	box()

	if (!is.null(names(x))) {
		axis(tick=FALSE, side=1,at=pivots,labels=names(x))
		title(xlab=xlab, line=3)
	} else {
		title(xlab=xlab, line=1)
	}

	if (discrete) {
		axis(side=2, at=brk.pts[-1]-0.5, labels=FALSE)
		text(y=brk.pts[-1]-0.5, x=0, pos=2, offset=1, xpd=TRUE, labels=levels(factor(unlisted)))
		title(ylab=ylab, line=2+add_w)
	} else {
		axis(side=2)
		title(ylab=ylab, line=3)
	}
}
