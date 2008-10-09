\name{read.gatingML}
\alias{read.gatingML}
\alias{internal.read.gatingML}


\title{
       Function to parse an XML gating File into objects in the R enviroment
      }
\description{
  This function parses an XML file defined in compliance with the Gating-ML candidate 
  recommendation into objects in the R environment, which can then be evaluated using 
  functions provided by the flowCore package.
 }
\usage{
read.gatingML(file,flowEnv,...)
}

\arguments{
  \item{file}{XML file describing gates,transformations or compensations}
  \item{flowEnv}{environment into which the objects created from the XML
  file are to be stored
  }
  \item{\dots}{additional arguments that are passed to the methods}
}
\details{
The Gating-ML specification has been developed as an interchange format for the description of
gates relevant to a flow cytometry experiment. Presently, we can read version 1.5 of the specification,
which was the most recent at the time of this writing. 

}
\value{
	A \code{\link[flowCore]{filterSet-class}} object containing all of the filters defined in
	the specified file.
  Structured filter of class as defined in the \code{\link[flowCore]{flowCore}} package.
}
\references{ ~put references to the literature/web site here ~ }
\author{Gopalakrishnan N}
\seealso{\code{\link[flowCore]{filterSet-class}}}
\examples{
\dontrun{
EGate <-read.gatingML(system.file("xml",
        "EllipsoidGates.xml", package="flowUtils"))
}
}

\keyword{methods}

