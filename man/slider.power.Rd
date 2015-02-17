\name{slider.power}
\alias{slider.power}
\title{Interactive reexpression by a power transformation}
\description{
Constructs a histogram of power-transformed data, where the power of
the transformation is controlled by a slider.  Hinkley's method of
asymmetry of the batch is displayed.
}
\usage{
slider.power(y)
}
\arguments{
  \item{y}{numeric vector of values}
}

\author{Jim Albert}

\examples{
## Not run: 
## This example cannot be run by examples() but should be work in an interactive R session

slider.power(rchisq(100,df=4))

## End(Not run)}

\keyword{manip}