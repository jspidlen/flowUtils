## Parsing Utilities

## Obtains children of a particular type
xmlGrep = function(x,type) UseMethod("xmlGrep")
xmlGrep.default = function(x,type)
	stop(paste("Looking for",type,"in",paste(class(x),collapse=",")))
xmlGrep.XMLNode = function(x,type) {
	children = xmlChildren(x)
	children[sapply(children,"is",type)]
}

smartTreeParse = function(file,...) {
	#Drops comments and tags XMLNode objects with class information related to the 
	#tag type and namespace.
	handlers = list(comment=function(x,...) NULL,startElement=function(x,...) { 
		class(x) = c(paste(make.names(c(xmlNamespace(x),xmlName(x))),collapse="_"),
					 make.names(xmlNamespace(x)),
					 class(x))
		x
		})
		xmlTreeParse(file,handlers=handlers,asTree=TRUE,fullNamespaceInfo=TRUE,...)
}


smartProcess = function(node,params,.defaults=list(),...) {
	lapply(params,function(name,...) {
		if(is.character(name))
			xmlGetAttr(node,name,.defaults[[name]])
		else if(is.function(name)) {
			def = .defaults[[name]]
			if(is.list(def)) do.call(name,c(node,def)) else name(node,def)
		} else name
	})
}

## xmlToFUN <- function(FUN, ..., .defaults=list())
## {
##     function(node, ...)
##     do.call(FUN,smartProcess(node, list(params), .defaults, ...))
## }

## xmlToClass <- function(className, ..., .defaults=list())
## {
##     function(node,...)
##         do.call("new", c(className,
##                          smartProcess(node, list(params), .defaults, ...)))
    

