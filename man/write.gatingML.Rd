\name{write.gatingML}
\alias{write.gatingML}

\title{
  UNDER DEVELOPMENT (DO NOT USE). Function to write a Gating-ML XML file based on gating 
  and transformation objects stored in an R environment.
}

\description{
  This function saves gating and transformation objects stored in an R environment to
  a Gating-ML 2.0 XML file. The objects expected and supported in the R environment are
  those that can normally be created by the read.gatingML function when a Gating-ML 2.0
  XML file is read.
}

\usage{
  write.gatingML(flowEnv, file = NULL)
}

\arguments{
  \item{flowEnv}{The R environment that is being searched for gating objects and transformations}
  \item{file}{The name of the output Gating-ML XML file. The standard output will be used if file
    is NULL.}  
}

\details{
  The Gating-ML specification has been developed as an interchange format for the description of
  gates relevant to a flow cytometry experiment. Presently, flowUtils can read Gating-ML versions
  1.5 and 2.0 of the specification (see \code{\link{read.gatingML}}). Gating-ML version 2.0 only 
  is being used when saving Gating-ML.
}

\references{
  Spidlen J, Leif RC, Moore W, Roederer M, ISAC DSTF, Brinkman RR. 2008.
  Gating-ML: XML-based gating descriptions in flow cytometry.
  Cytometry A. 73A(12):1151--7.

  Spidlen J, ISAC DSTF, Brinkman RR. 2008. 
  Gating-ML Candidate Recommendation for Gating Description in Flow Cytometry version 1.5.
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.pdf}
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.full.zip}
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.Compliance-tests.081030.zip}

  Spidlen J, ISAC DSTF, Brinkman RR. 2013.
  Gating-ML 2.0. International Society for Advancement of Cytometry (ISAC) standard for 
  representing gating descriptions in flow cytometry.
  \url{http://flowcyt.sf.net/gating/20130122.pdf}
  \url{http://flowcyt.sf.net/gating/20130122.full.zip}
}

\author{Spidlen, J.}

\seealso{
\code{\link{read.gatingML}}
}

\examples{

#######################
# TODO
#######################

}

\keyword{methods}
