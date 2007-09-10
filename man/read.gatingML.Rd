\name{read.gatingML}
\alias{read.gatingML}
\alias{internal.read.gatingML}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to parse xml gating file into a structured filter}
\description{
   Parsing Gating-ML into a structured filter.
}
\usage{
read.gatingML(file,...,version="1.3")
}

\arguments{
  \item{file}{xml file describing a gate}
  \item{version}{An optional version string identifying the Gating-ML specification to parse}
}
\details{
The Gating-ML specification has been developed as an interchange format for the description of
gates relevant to a flow cytometry experiment. Presently, we can read version 1.3 of the specification,
which was the most recent at the time of this writing. When used in conjunction with Compensation-ML and
Transformation-ML, this specification can 
}
\value{
	A \code{\linkS4class{filterSet}} object containing all of the filters defined in
	the specified file.
  Structured filter of class as defined in the \code{\link[flowCore]{flowCore}} package.
}
\references{ ~put references to the literature/web site here ~ }
\author{ B. Ellis}
\seealso{\code{\linkS4class{filterSet}}}
\examples{

EGate <-read.gatingML(system.file("xml",
        "EllipsoidGates.xml", package="flowUtils"))
}

\keyword{methods}

