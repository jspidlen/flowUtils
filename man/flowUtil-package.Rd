\name{flowUtils-package}
\alias{flowUtils-package}
\alias{flowUtils}
\docType{package}
\title{
  Utilities for flow cytometry data
}
\description{
  This package includes functions to import Gates,transformations and 
  compensations defined in compliance with Gating-ML specification version 1.5 
  and 2.0. This package depends on the flowCore package for methods to evaluate 
  the Gating-ML files read into the workspace.
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
standards defined by the Gating-ML specification version 1.5 and 2.0.

The package also includes a Test Suite, which allows the user to test whether 
the implmentation of gates, trasnformations are in compliance with the 
Gating-ML 1.5 standard. (At this point, Gating-ML 2.0 does not come with a
similar test suite).

}
\author{
  Maintainer: Nishant Gopalakrishnan <ngopalak@fhcrc.org>
  Authors: N. Gopalakrishnan, F. Hahne, B. Ellis, R. Gentleman,
           M. Dalphin, N. Le Meur, B. Purcell, J. Spidlen
}


\keyword{ package }
\seealso{
\code{\link[flowCore:flowCore-package]{flowCore}}
}

