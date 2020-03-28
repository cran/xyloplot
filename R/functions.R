#' @title Create a xyloplot
#' @description Plots xylophones (centre-aligned histograms) for the input vector(s), provided either as a single vector or list of vectors. Numeric vectors and factors are admissible (character vectors are transformed to factors). If numeric vectors are provided, \code{cut} will be used to aggregate values, whereas if character vectors or factors are provided, each 'level' will have it's own `key' on the `xylophone'. Note that if factors are used, all factors in `x` must have identical levels.
#' @param x Vector or list of vectors to use for creating xyloplots.
#' @param ... Additional arguments passed to \code{\link{xyloplot.list}}, or other graphical parameters (e.g. \code{"col"}, \code{"lwd"}, ..., etc.) for \code{xyloplot.list} which are recycled along the xylophones and then used by functions for rendering the individual rectangles (e.g. \code{rect}).
#' @return Returns an object of class \code{"xyloplot"} containing the specification of graphical elements required to create a corresponding plot, including the coordinates of the corners of rectangles (in terms of the location on the value value axis and the pivot axis across which the xyloplots are spread) and the positions of the breakpoints used to partition the range of values.
#' @seealso \code{\link{plot.xyloplot}}
#' @export
xyloplot <- function(x, ...) {
	UseMethod("xyloplot")	
}

#' @title Create xyloplots from lists of numeric vectors or factors
#' @param breaks A single positive integer value giving the number of breakpoints to use for an evenly spaced partition of the values in \code{x}, a numeric vector explicitly giving the the breakpoints, or \code{NULL} to use the default partition.
#' @param space The proportion of the total distance on the pivots axis allocated to each 'xylophone' which should be empty or \code{NULL}, in which case the pivot axis coordinates for the xyloplot rectangles for each pivot are transformed to [0, 1].
#' @param pivot Vector the same length as \code{x} used to determine which pivot to place the xylophone representing corresponding distributions of \code{x} onto (duplicated values go on the same pivots).
#' @param pivot_labels Character vector giving names for each pivot or \code{NULL}.
#' @param just Vector whose elements should take values in \code{0, 0.5, 1} which determines whether to centre-align the xylophones (\code{0.5}, default), left align them (\code{0}) or right align them (\code{1}).
#' @param freq Logical value. If \code{TRUE}, the frequencies/counts of data points falling in each interval are represented. If \code{FALSE} (default), the frequency density of data points in each interval are represented.
#' @method xyloplot list
#' @export
#' @importFrom graphics axis box plot rect title par strwidth text
#' @rdname xyloplot
xyloplot.list <- function(
	x, 
	breaks=NULL,
	space=0.1,
	pivot=if (!is.null(names(x))) factor(names(x), levels=names(x)) else seq_along(x),
	pivot_labels=if (is.factor(pivot)) levels(pivot) else NULL,
	just=0.5,
	freq=FALSE,
	...
) {
	if (length(x) == 0L) stop("No data!")
	if (any(is.na(factor(just, levels=0:2/2)))) {
		stop("elements of 'just' argument must take values in {0,0.5,1}")
	}
	if (length(just) != length(x)) {
		if (length(x) %% length(just) != 0) {
			stop("length of 'just' must divide length of 'x'")
		} else {
			just <- rep(just, times=length(x) %/% length(just))
		}
	}

	pivot_f <- if (!is.factor(pivot)) factor(pivot) else pivot

	ul <- unlist(use.names=FALSE, x)
	if (length(ul) == 0L) stop("No data!")
	if (is.factor(ul)) {
		if (any(vapply(FUN.VALUE=logical(1), X=x, FUN=function(x_i) { !identical(levels(x_i), levels(ul)) }))) {
			stop("if 'x' contains factors, then the levels of all elements must be identical!")
		}
	}
	if (is.numeric(ul) && diff(range(ul)) == 0) ul <- factor(ul)

	if (is.null(breaks)) {
		breaks <- 
			if (is.factor(ul)) {
				c(0L, seq_along(levels(ul)))
			} else if (!is.numeric(ul)) {
				stop("'x' must be a list of either numerics or factors!")
			} else {
				seq(from=min(ul), to=max(ul), length.out=30L)
			}
	} else {
		if (is.factor(x)) stop("'breaks' is already set to the levels of factors in 'x'!")
		if (length(breaks) == 1L) breaks <- seq(from=min(ul), to=max(ul), length.out=breaks)
	}

	ulf <- if (is.factor(ul)) ul else cut(ul, breaks=breaks, include.lowest=TRUE)
	tab <- table(data.frame(xylo=factor(rep(seq_along(x), times=lengths(x)), levels=seq_along(x)), y=ulf))

	bar_widths <- if (freq) { tab/max(tab) } else { 
		nm <- tab/rowSums(tab)
		nm/unsplit(f=pivot_f, tapply(X=apply(nm, 1, max), INDEX=pivot_f, FUN=max))
	}

	snap <- lapply(list(l=0, r=1), function(v) vapply(FUN=all, FUN.VALUE=logical(1), X=split(f=pivot_f, just==v)))
	violin_width <- ifelse(snap$l|snap$r, 2, 1)*(if (is.null(space)) 1 else (1-space)/nlevels(pivot_f))
	pivot_positions <- if (is.null(space)) { rep(0.5, nlevels(pivot_f)) + (snap$r-snap$l)/2 } else { xylo_positions(nlevels(pivot_f)) + (snap$r - snap$l)*(1-space)/nlevels(pivot_f)/2 }
	bw <- bar_widths * unsplit(f=pivot_f, value=violin_width)
	
	structure(
		class="xyloplot",
		list(
			corners=list(
				pivot=lapply(list(
					low=rep(pivot_positions[as.integer(pivot_f)], times=nlevels(ulf))-(bw/2*(just!=0)),
					high=rep(pivot_positions[as.integer(pivot_f)], times=nlevels(ulf))+bw/2*(just!=1)
				), as.numeric),
				value=list(
					low=(rep(breaks[-1], each=length(x))-rep(diff(breaks), each=length(x))),
					high=(rep(breaks[-1], each=length(x)))
				)
			),
			rectangle_pivot=rep(pivot_f, times=nlevels(ulf)),
			breaks=breaks,
			pivot_positions=pivot_positions,
			interval_names=if (is.factor(ul)) levels(ul) else NULL,
			pivot_labels=pivot_labels,
			graphical_parameters=lapply(list(...), function(param) rep(param, times=length(bar_widths) %/% length(param)))
		)
	)
}

#' @title Left-right xyloplots
#' @description Create a xyloplot with independent histograms on the left and right hand side of each pivot.
#' @param lhs A set of values to use as the left-hand side histograms (see \code{\link{xyloplot}} for details on admissible types)..
#' @param rhs As \code{lhs} for the right-hand side.
#' @param ... Additional arguments passed to \code{\link{xyloplot}}.
#' @return A \code{"xyloplot"} object.
#' @seealso \code{\link{xyloplot}}
#' @export
left_right_xylo <- function(
	lhs,
	rhs,
	...
) {
	if (!is.list(lhs)) lhs <- list(lhs)
	if (!is.list(rhs)) rhs <- list(rhs)
	if (length(lhs) != length(rhs))
		stop("'lhs' and 'rhs' lists must be the same length!")
	if (!identical(names(lhs), names(rhs)))
		stop("'lhs' and 'rhs' lists must have the same names attribute!")

	xyloplot(
		unname(do.call(what=c, Map(USE.NAMES=FALSE, lhs, rhs, f=list))),
		pivot_labels=names(lhs),
		just=rep(1:0, times=length(lhs)),
		pivot=rep(seq_along(lhs), each=2L),
		...
	)
}

#' @title Plot a xyloplot object
#' @description Render a xyloplot object using base graphics.
#' @param x Object of class \code{"xyloplot"}
#' @param vertical Logical value determining whether to plot vertical or horizontal xylophones.
#' @param value_lab Text to put on value axis.
#' @param pivots_lab Text to put on pivots axis.
#' @param box Logical value determining whether to draw a box around the plot.
#' @param draw_empty Logical value determining whether to draw `empty' boxes in the xylophones. If \code{TRUE} (default), empty boxes will appear as lines.
#' @param ... Additional arguments passed to \code{plot}.
#' @return Renders a xyloplot.
#' @method plot xyloplot
#' @seealso \code{\link{xyloplot}}
#' @export
plot.xyloplot <- function(
	x,
	vertical=TRUE,
	value_lab="Value",
	pivots_lab="Frequency",
	box=TRUE,
	draw_empty=TRUE,
	...
) {
	grect <- function(...) do.call(what=rect, lapply(c(x$graphical_parameters, list(...)), if (draw_empty) identity else function(v) v[x$corners$pivot$high>x$corners$pivot$low]))
	if (vertical) {
		if (!is.null(x$interval_names)) {
			inches_to_lines <- ( par("mar") / par("mai") )[1]
			old_mar <- par("mar")
			on.exit(par(mar=old_mar))
			add_w <- max(strwidth(x$interval_names, units="inches")) * inches_to_lines
			par(mar=ifelse(1:4 == 2, 3 + add_w, old_mar))
		}

		plot(x=NULL, xlim=0:1, xaxs="i", yaxs="i", xlab="", ylab="", ylim=range(x$breaks), axes=FALSE, ...)

		grect(
			xleft=x$corners$pivot$low,
			xright=x$corners$pivot$high,
			ybottom=x$corners$value$low,
			ytop=x$corners$value$high
		)
	
		if (!is.null(x$pivot_labels)) {
			axis(tick=FALSE, side=1, at=x$pivot_positions, labels=x$pivot_labels)
			title(xlab=pivots_lab, line=3)
		} else {
			title(xlab=pivots_lab, line=1)
		}

		if (!is.null(x$interval_names)) {
			axis(pos=0, side=2, at=x$breaks[-1]-0.5, labels=FALSE)
			text(y=x$breaks[-1]-0.5, x=0, pos=2, offset=1, xpd=TRUE, labels=x$interval_names)
			title(ylab=value_lab, line=2+add_w)
		} else {
			axis(pos=0, side=2)
			title(ylab=value_lab, line=3)
		}
	} else {
		plot(x=NULL, ylim=0:1, xaxs="i", yaxs="i", xlab="", ylab="", xlim=range(x$breaks), axes=FALSE, ...)

		grect(
			ybottom=x$corners$pivot$low,
			ytop=x$corners$pivot$high,
			xleft=x$corners$value$low,
			xright=x$corners$value$high
		)
	
		if (!is.null(x$pivot_labels)) {
			axis(tick=FALSE, side=2, at=x$pivot_positions, labels=x$pivot_labels)
			title(ylab=pivots_lab, line=3)
		} else {
			title(ylab=pivots_lab, line=1)
		}

		if (!is.null(x$interval_names)) {
			axis(pos=0, side=1, at=x$breaks[-1]-0.5, labels=FALSE)
			text(x=x$breaks[-1]-0.5, y=0, pos=1, offset=1, xpd=TRUE, labels=x$interval_names)
			title(xlab=value_lab, line=3)
		} else {
			axis(pos=0, side=1)
			title(xlab=value_lab, line=3)
		}

	}
	if (box) box(which="plot")
}

#' @method print xyloplot
#' @export
#' @rdname plot.xyloplot
print.xyloplot <- function(x, ...) plot.xyloplot(x, ...)

#' @title Get x-axis positions for \code{n} xylophones
#' @param n Number of xylophones.
#' @export
xylo_positions <- function(n) {
	seq(from=1/2/n,by=1/n,length.out=n)
}

#' @rdname xyloplot
#' @export
xyloplot.factor <- function(x, ...) {
	xyloplot.list(x=list(x), ...)
}

#' @rdname xyloplot
#' @export
xyloplot.logical <- function(x, ...) {
	xyloplot.list(x=list(factor(x, levels=c(FALSE, TRUE))), ...)
}

#' @rdname xyloplot
#' @export
xyloplot.character <- function(x, ...) {
	xyloplot.list(x=list(factor(x)), ...)
}

#' @rdname xyloplot
#' @export
xyloplot.numeric <- function(x, ...) {
	xyloplot.list(x=list(x), ...)
}
