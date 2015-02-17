\name{slider.match}
\alias{slider.match}
\title{Interactive reexpression by a power transformation using matched reexpressions}
\description{
Constructs boxplots of matched raw and power-transformed data, where the power of
the transformation is controlled by a slider.  Hinkley's method of
asymmetry of the batch is displayed.
}
\usage{
slider.match(x)
}
\arguments{
  \item{x}{numeric vector of values}
}

\author{Jim Albert}

\examples{
## Not run: 
## This example cannot be run by examples() but should be work in an interactive R session

slider.match(rchisq(100,df=4))

## End(Not run)}

\keyword{manip}