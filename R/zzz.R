## Parse the xml file containing the defaults for the respective FlowJo tags
.onLoad <- function(...)
{
    def <- xmlSApply(xmlTreeParse(system.file("defaults.xml",
                                              package="flowUtils"))[["doc"]][[1]],
                     function(x)
                     if(!is(x, "XMLCommentNode")) as.list(xmlAttrs(x)))
    def <- def[!sapply(def, is.null)]
    assign("fjDefaults", def, .fuEnv)
}


## Internal environment to store the parsed defaults
.fuEnv <-  new.env()
 
