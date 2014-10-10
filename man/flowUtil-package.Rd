\name{flowUtils-package}
\alias{flowUtils-package}
\alias{flowUtils}
\docType{package}
\title{
  Utilities for flow cytometry data
}

\description{
  This package includes functions to import gates, transformations and 
  compensations defined in compliance with Gating-ML specification version 1.5 
  and 2.0. This package depends on the flowCore package for methods to evaluate 
  the Gating-ML files read into the workspace.

  The main features of this package provide compatability to the data
  standards defined by the Gating-ML specification version 1.5 and 2.0.

  The package also includes a Test Suite, which allows the user to test whether 
  the implmentation of gates an, transformations are in compliance with the 
  Gating-ML standard.
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

\author{
  Maintainer: Josef Spidlen <jspidlen@bccrc.ca>
  
  Authors: J. Spidlen, N. Gopalakrishnan, F. Hahne, B. Ellis, 
           R. Gentleman, M. Dalphin, N. Le Meur, B. Purcell
}

\keyword{package}
\seealso{
\code{\link[flowCore:flowCore-package]{flowCore}}
}

