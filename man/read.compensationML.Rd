\name{read.compensationML}
\alias{read.compensationML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.compensationML(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ ~~Describe \code{file} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }

\author{ ~~who you are~~ }

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file) 
{
    x = xmlTreeParse(file, handlers = list(comment = function(x, 
        ...) NULL, startElement = function(x) {
        cn = paste(make.names(c(xmlNamespace(x), xmlName(x))), 
            collapse = "_")
        tn = make.names(xmlName(x))
        class(x) = c(cn, tn, class(x))
        x
    }), asTree = TRUE)
    x
  }
}

\keyword{ IO }
