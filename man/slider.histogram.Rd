\name{slider.histogram}
\alias{slider.histogram}
\title{Interactive binning of a histogram}
\description{
Constructs a histogram of data, where the number of bins of the histogram is controlled by a slider. 
}
\usage{
slider.histogram(y)
}
\arguments{
  \item{y}{numeric vector of values}
}

\author{Jim Albert}

\examples{
## Not run: 
## This example cannot be run by examples() but should be work in an interactive R session

slider.histogram(rchisq(200,df=4))

## End(Not run)}

\keyword{manip}