# xyloplot 2.0

Breaking changes introduced, including that now the argument `discrete` is obsolete (so that it is now up to the user to use convert input to factors to use a discrete scale), and the function `xyloplot` returns xyloplot objects (`plot.xyloplot` must now be called on these objects in order to pass arguments to `plot`). Now multiple xyloplots can be placed on the same pivot, and more control over output is allowed through setting the justification for each histogram individually.

# xyloplot 1.6

Add `freq` argument which is equivalent to the `freq` argument in `hist`: if `FALSE` (default), frequency densities are represented; if `TRUE`, frequencies/counts are represented.

# xyloplot 1.5

Add `rhs` argument which, if specified, splits xyloplots in the middle, with left hand side distribution given by `x` and right hand side by `rhs`.

# xyloplot 1.3

Added 'Examples' vignette and `xylo_positions` function which lets you get the positions of the xylophones lest the user wants to add more graphical objects to the plot.

# xyloplot 1.2

Fix bug whereby `max` valued data point not shown for continuous data.

# xyloplot 1.1

Added support for plotting discrete data supplied as character vectors or factors - e.g. `xyloplot(sample(c("goldfish","cat","dog","fish","mouse","giraffe"), size=100, replace=TRUE))`, `xyloplot(replicate(n=5, simplify=FALSE, expr=factor(sample(c(0.01, 0.1, 0.2, 0.25, 0.5, 1), size=10, replace=TRUE))), col=rainbow(5))`
