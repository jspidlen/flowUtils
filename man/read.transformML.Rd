\name{read.transformML}
\alias{read.transformML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to parse xml file to transform FCS data}
\description{
   Parsing Transform-ML into methods to transform FCS data.
}
\usage{
read.transformML(file)
}

\arguments{
  \item{file}{xml file describing a transformation}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  Function of class trnsform as defined in the \code{\link[flowCore]{flowCore}} package.
}
\references{ ~put references to the literature/web site here ~ }
\author{ N. LeMeur B. Ellis}
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{\code{\link[flowCore]{filter-class}}}
\examples{

Atransform <-read.transformML(system.file("xml",
        "linearTramsform.xml", package="flowUtils"))
}

\keyword{methods}

