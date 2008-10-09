\name{flowUtils-package}
\alias{flowUtils-package}
\alias{flowUtils}
\docType{package}
\title{
  Utilities for flow cytometry data
}
\description{
  This package includes methods to import Gates,transformations and 
  compensations defined in compliance with Gating-ML Candidate recommendation
  for Gating Description.(Version V 1.5)
  This package depends on the flowCore package for methods to evaluate the 
  gatingML files read into the workspace.
}
\details{
\tabular{ll}{
Package: \tab flowUtils\cr
Type: \tab Package\cr
Version: \tab 0.2.1\cr
Date: \tab 2006-11-16\cr
License: \tab Artistic\cr
}
The main features of this package provide compatability to the data
standards defined by Gating-ML candidate recommendation for gating description
in flow cytometry (Version 1.5).

The package also includes a Test Suite, which allows the user to test whether 
the implmentation of gates, trasnformations are in compliance with the 
Gating-ML standard. 

}
\author{


  Maintainer: Nishant Gopalakrishnan <ngopalak@fhcrc.org>
  Authors: M. Dalphin, B. Ellis, R. Gentleman, F. Hahne, N. Le Meur, B. Purcell
}


\keyword{ package }
\seealso{
\code{\link[flowCore:flowCore-package]{flowCore}}
}
\examples{
## examples go here
}
