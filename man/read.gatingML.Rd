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
  \item{flowEnv}{environment into which the R objects created from the XML
  file are to be stored
  }
  \item{\dots}{additional arguments that are passed to the methods}
}
\details{
The Gating-ML specification has been developed as an interchange format for the description of
gates relevant to a flow cytometry experiment. Presently, we can read version 1.5 of the specification,
which was the most recent at the time of this writing. 

}

\references{Gating-ML Candidate Recommendation for Gating Description in Flow Cytometry V 1.5}

\author{Gopalakrishnan N}

\examples{

 flowEnv=new.env()

 fcsFile<-system.file("extdata/List-modeDataFiles","fcs2_int16_13367ev_8par_GvHD.fcs",package="gatingMLData")
 fcs <- read.FCS(fcsFile,transformation=FALSE)
 
 gateFile <- system.file("extdata/Gating-MLFiles","02CtSRectangular.xml",package="gatingMLData")
 read.gatingML(gateFile,flowEnv)
 
 ls(flowEnv)
  
 result=filter(fcs,flowEnv$CtSR_03)
 summary(result)
}

\keyword{methods}

