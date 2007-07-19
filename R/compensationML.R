read.compensationML <- function(file) {
    x = xmlTreeParse(file, handlers=list(
                              "comment"=function(x,...) NULL,
                              "startElement"=function(x) {
                                  cn = paste(make.names(c(xmlNamespace(x),xmlName(x))),collapse="_")
                                  tn = make.names(xmlName(x))
                                  class(x) = c(cn,tn, class(x))
                                  x
                               }
        ),asTree=TRUE)

     x
}

