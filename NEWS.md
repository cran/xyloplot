# xyloplot 1.1

Added support for plotting discrete data supplied as character vectors or factors - e.g. `xyloplot(sample(c("goldfish","cat","dog","fish","mouse","giraffe"), size=100, replace=TRUE))`, `xyloplot(replicate(n=5, simplify=FALSE, expr=factor(sample(c(0.01, 0.1, 0.2, 0.25, 0.5, 1), size=10, replace=TRUE))), col=rainbow(5))`
