\name{testGatingMLCompliance}
\alias{testGatingMLCompliance}

\title{Function to perform all the Gating-ML compliance tests}

\description{
This function performs the Gating-ML compliance tests.
Either Gating-ML 1.5 or Gating-ML 2.0 compatibility may be checked.
The Gating-ML XML files, FCS data files and the expected results provided 
by the gatingMLData are utilized in performing the compliance tests.
The results obtained are compared with the expected results and an HTML summary 
report is generated.
}

\usage{
  testGatingMLCompliance(file = "GatingMLComplianceReport", version = 2.0)
}

\arguments{
  \item{file}{Name of the file in which the generated Gating-ML compliance 
    report is to be saved. The .html extension will be added.}
  \item{version}{The Gating-ML version that is supposed to be checked. 
    Currently, versions 1.5 and 2.0 are supported.}
}

\details{
  The testGatingMLCompliance function depends on the gatingMLData data package 
  for performing the compliance tests.
}

\references{
  Spidlen J, ISAC DSTF, Brinkman RR. 2014.\cr
  Gating-ML 2.0. International Society for Advancement of Cytometry (ISAC) standard for 
  representing gating descriptions in flow cytometry.\cr
  \url{http://flowcyt.sf.net/gating/20141009.pdf}\cr
  \url{http://flowcyt.sf.net/gating/20141009.full.zip}

  Spidlen J, Leif RC, Moore W, Roederer M; International Society for the Advancement of Cytometry Data Standards Task Force, Brinkman RR.\cr
  Gating-ML: XML-based gating descriptions in flow cytometry.\cr
  Cytometry A. 2008 Dec; 73A(12):1151--7. doi: 10.1002/cyto.a.20637.
  
  Spidlen J, ISAC DSTF, Brinkman RR. 2008.\cr
  Gating-ML Candidate Recommendation for Gating Description in Flow Cytometry version 1.5.\cr
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.pdf}\cr
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.full.zip}\cr
  \url{http://flowcyt.sf.net/gating/Gating-ML.v1.5.081030.Compliance-tests.081030.zip}
}

\author{Spidlen J., Gopalakrishnan N.}

\examples{

\dontrun{
################################################################
### Performs Gating-ML 1.5 compliance tests and                #
### writes the results to Gating-ML_1.5_Compliance_Report.html #
################################################################
testGatingMLCompliance("Gating-ML_1.5_Compliance_Report", version = 1.5)

################################################################
### Performs Gating-ML 2.0 compliance tests and                #
### writes the results to Gating-ML_2.0_Compliance_Report.html #
################################################################
testGatingMLCompliance("Gating-ML_2.0_Compliance_Report", version = 2.0)
}

}

\keyword{utilities}
