\name{read.gatingML}
\alias{read.gatingML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to parse xml gating file into a structured filter}
\description{
   Parsing Gating-ML into a structured filter.
}
\usage{
read.gatingML(file)
}

\arguments{
  \item{file}{xml file describing a gate}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  Structured filter of class as defined in the \code{\link[flowCore]{flowCore}} package.
}
\references{ ~put references to the literature/web site here ~ }
\author{ B. Ellis}
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{\code{\link[flowCore]{filter-class}}}
\examples{

EGate <-read.gatingML(system.file("xml",
        "EllipsoidGates.xml", package="flowUtils"))
}

\keyword{methods}

