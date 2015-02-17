\name{slider.straighten}
\alias{slider.straighten}
\title{Interactive straightening of plots by power transformations}
\description{
Constructs a scatterplot and residual plot of power-transformed data, 
where the powers of the x and y transformations are controlled by a slider.
The half-slope ratio of the three summary points is displayed.
}
\usage{
slider.straighten(x, y)
}
\arguments{
  \item{x}{numeric vector of values along the horizontal axis}
  \item{y}{numeric vector of values along the vertical axis}
}

\author{Jim Albert}

\examples{
## Not run: 
## This example cannot be run by examples() but should be work in an interactive R session

slider.straighten(car.measurements$Displacement,
  car.measurements$MPG)

## End(Not run)}

\keyword{manip}