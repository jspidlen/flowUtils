\name{read.gatingML}
\alias{read.gatingML}
\alias{internal.read.gatingML}

\title{
  Function to parse a Gating-ML XML file into objects in the R environment
}

\description{
  This function parses a Gating-ML XML file defined in compliance with the Gating-ML
  recommendation into objects in the R environment, which can then be evaluated using 
  functions provided by the flowCore package.
 }

\usage{
  read.gatingML(file, flowEnv, ...)
}

\arguments{
  \item{file}{Gating-ML XML file describing gates, transformations and/or compensations}
  \item{flowEnv}{environment into which the R objects created from the Gating-ML XML
  file are to be stored}
  \item{\dots}{additional arguments that are passed to the methods}
}

\details{
  The Gating-ML specification has been developed as an interchange format for the description of
  gates relevant to a flow cytometry experiment. Presently, we can read Gating-ML versions 1.5 and 2.0 
  of the specification. Version 2.0 is the most recent at the time of this writing.
}

\references{
  Spidlen J, ISAC DSTF, Brinkman RR. 2014.\cr
  Gating-ML 2.0. International Society for Advancement of Cytometry (ISAC) standard for 
  representing gating descriptions in flow cytometry.\cr
  \url{http://flowcyt.sf.net/gating/20141009.pdf}\cr
  \url{http://flowcyt.sf.net/gating/20141009.full.zip}

  Spidlen J, Leif RC, Moore W, Roederer M, ISAC DSTF, Brinkman RR. 2008.\cr
  Gating-ML: XML-based gating descriptions in flow cytometry.\cr
  Cytometry A. 2008 Dec; 73A(12):1151--7. doi: 10.1002/cyto.a.20637.

  Spidlen J, ISAC DSTF, Brinkman RR. 2008.\cr
  Gating-ML Candidate Recommendation for Gating Description in Flow Cytometry version 1.5.\cr
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.pdf}\cr
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.full.zip}\cr
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.Compliance-tests.081030.zip}
}

\author{J. Spidlen, N. Gopalakrishnan}

\seealso{
  \code{\link{write.gatingML}}
}

\examples{
library("flowCore")

#########################
# Gating-ML 2.0 example #
#########################
flowEnv <- new.env()

fcsFile <- system.file("extdata/Gml2/FCSFiles", 
  "data1.fcs", package="gatingMLData")
fcs <- read.FCS(fcsFile, 
  transformation="linearize-with-PnG-scaling")

gateFile <- system.file("extdata/Gml2/Gating-MLFiles", 
  "gates1.xml", package="gatingMLData")
read.gatingML(gateFile, flowEnv)
ls(flowEnv)

result = filter(fcs, flowEnv$Polygon1)
summary(result)

#########################
# Gating-ML 1.5 example #
#########################
flowEnv <- new.env()

fcsFile <- system.file("extdata/List-modeDataFiles",
  "fcs2_int16_13367ev_8par_GvHD.fcs", package="gatingMLData")
fcs <- read.FCS(fcsFile, transformation=FALSE)

gateFile <- system.file("extdata/Gating-MLFiles",
  "02CtSRectangular.xml", package="gatingMLData")
read.gatingML(gateFile, flowEnv)
ls(flowEnv)
  
result <- filter(fcs, flowEnv$CtSR_03)
summary(result)

}

\keyword{methods}

