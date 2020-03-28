## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(dev="svg", fig.width=7, fig.height=5)
library(xyloplot)
knit_print.xyloplot <- function(x, ...) xyloplot:::plot.xyloplot(x)
set.seed(1)

## ------------------------------------------------------------------------
xyloplot(rnorm(1000))

## ------------------------------------------------------------------------
xyloplot(
 x=lapply(1:3, function(mean) rnorm(mean=mean, n=1000)), 
 breaks=20,
 col=rainbow(3))

## ------------------------------------------------------------------------
values <- c(0.01, 0.1, 0.2, 0.25, 0.5, 1)
xyloplot(
	replicate(n=5, simplify=FALSE, 
	expr=factor(sample(values, size=10, replace=TRUE), levels=values)), 
	col=rainbow(5))

## ------------------------------------------------------------------------
xyloplot(
 sample(c("goldfish","cat","dog","fish","mouse","giraffe"), 
 size=100, replace=TRUE))

## ------------------------------------------------------------------------
data <- lapply(1:3, function(mean) rnorm(mean=mean, n=1000))
xyloplot(x=data)
n <- length(data)
positions <- xylo_positions(n)
upper_qs <- sapply(data, function(sample) quantile(sample, prob=0.75))
segments(x0=positions-1/n/2, x1=positions+1/n/2, y0=upper_qs, y1=upper_qs, col="red", pch=19)

## ------------------------------------------------------------------------
data_rhs <- lapply(2:4, function(mean) rnorm(mean=mean, sd=2, n=2000))
left_right_xylo(lhs=data, rhs=data_rhs, col=rainbow(2))

## ----eval=FALSE----------------------------------------------------------
#  xyloplot(c(data, data_rhs), pivot=rep(1:3, 2), just=rep(1:0, each=3), col=rep(rainbow(2), each=3))

## ------------------------------------------------------------------------
xyloplot(list(rnorm(1000), rnorm(1000)+2), pivot=rep("a", 2), col=rainbow(n=2, alpha=0.5))

## ----echo=FALSE----------------------------------------------------------
old_mfrow <- par("mfrow")
par(mfrow=c(1, 2))

## ------------------------------------------------------------------------
lhs <- rnorm(n=1000)
rhs <- rnorm(n=2000)
plot(main="freq=FALSE", left_right_xylo(lhs=lhs, rhs=rhs, col=rainbow(2), freq=FALSE))
plot(main="freq=TRUE", left_right_xylo(lhs=lhs, rhs=rhs, col=rainbow(2), freq=TRUE))

## ----echo=FALSE----------------------------------------------------------
par(mfrow=old_mfrow)

## ------------------------------------------------------------------------
plot(
	main="Horizontal unboxed xyloplot", 
	vertical=FALSE, 
	box=FALSE, 
	pivots_lab="set this text with the `pivots_lab` parameter",
	value_lab="set this text with the `value_lab` parameter",
	x=xyloplot(x=rnorm(1000)))

