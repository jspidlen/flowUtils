\name{testGatingCompliance}
\alias{testGatingCompliance}

\title{
       
Function to perfom all the gating-ML Compliance tests
      }

\description{
  This funtion performs all the gating-ML compliance tests. The XML files, data files and the Expected 
  results provided by the gatingMLData are utilized in performing the compliance tests.
  The results obtained are compared with the expected results and a summary html report is
  generated.
  
 }

\usage{
 testGatingCompliance(file)
}

\arguments{
  \item{file}{name of the file in which the report generated is to be saved }
  }

\details{
The testGatingCompliance depends on the data package gatingMLData for performing 
the compliance tests 
}

\references{Gating-ML Candidate Recommendation for Gating Description in Flow Cytometry V 1.5 }
\author{Gopalakrishnan N}

\examples{

\dontrun{
### performs all the Gating compliance tests  and writes the results to mytest.html
testGatingCompliance("mytest")
}


}

\keyword{utilities}

