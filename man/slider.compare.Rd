\name{slider.compare}
\alias{slider.compare}
\title{Interactive comparison of groups by a power transformation}
\description{
Constructs a boxplot of power-transformed data, where the power of
the transformation is controlled by a slider.
}
\usage{
slider.compare(y,group)
}
\arguments{
  \item{y}{numeric vector of values}
  \item{group}{grouping variable}
}

\author{Jim Albert}

\examples{
## Not run: 
## This example cannot be run by examples() but should be work in an interactive R session

with(homeruns.2000, slider.compare(HOMERUNS, YEARS))

## End(Not run)}

\keyword{manip}