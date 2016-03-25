\name{xyloplot-package}
\alias{xyloplot-package}
\docType{package}
\title{
  A Method for Creating Xylophone-Like Frequency Density Plots
}
\description{
A method for creating vertical histograms sharing a y-axis using
    base graphics.
}
\details{
	xyloplot provides a generic method for plotting frequency density plots in the style of histograms akin to violin plots for numeric vectors and lists of numeric vectors. 
}
\author{
Daniel Greene
Maintainer: Daniel Greene <dg333@cam.ac.uk>
}
\keyword{ xyloplot }
\examples{
  xyloplot(rnorm(1000))
  xyloplot(lapply(1:3, function(mean) rnorm(mean=mean, n=1000)), col=rainbow(3))
}
