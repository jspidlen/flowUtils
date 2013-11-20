#############################################
## Code related to writing Gating-ML files ##
#############################################

write.gatingML <- function(file, flowEnv)
{
	# TODO Drop this note when done.
	cat("Please do not use this function, it is currently under development!!\n--------------------------------------------------------------------\n\n")
	
    if(is.null(file) || !is(file, "character")) 
        stop("A file with the name of the output file is required.", call. = FALSE)
    if(is.null(flowEnv) || !is.environment(flowEnv))
        stop("A flowEnv environment with objects to be saved is requred.", call. = FALSE)
    if(substr(file, nchar(file) - 3, nchar(file)) != ".xml")
        file <- paste(file, "xml", sep=".")

    namespaces <- c(
        "gating" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating", 
        "xsi" = "http://www.w3.org/2001/XMLSchema-instance", 
        "transforms" = "http://www.isac-net.org/std/Gating-ML/v2.0/transformations", 
        "data-type" = "http://www.isac-net.org/std/Gating-ML/v2.0/datatypes")
    
    gatingMLNode = suppressWarnings(xmlTree("gating:Gating-ML", namespaces = namespaces, 
        attrs = c("xsi:schemaLocation" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating http://flowcyt.sourceforge.net/gating/2.0/xsd/Gating-ML.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/transformations http://flowcyt.sourceforge.net/gating/2.0/xsd/Transformations.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/datatypes http://flowcyt.sourceforge.net/gating/2.0/xsd/DataTypes.v2.0.xsd")))

    for (x in ls(flowEnv)) 
    { 
        switch(class(flowEnv[[x]]),
            "rectangleGate" = addRectangleGateNode(gatingMLNode, x, flowEnv),
            cat(paste(x, ": class", class(flowEnv[[x]]), "- not supported yet.\n"))
        )
    }
    
    sink(file = file)
    cat(saveXML(gatingMLNode$value(), encoding = "UTF-8"))
    sink()
}

addRectangleGateNode <- function(gatingMLNode, x, flowEnv)
{
    cat(paste("Working on ", x, ".\n", sep=""))
    attrs = c("gating:id" = filterIdtoXMLId(flowEnv[[x]]@filterId))
    gatingMLNode$addNode("gating:RectangleGate", attrs = attrs, close = FALSE)
    for (i in 1:length(flowEnv[[x]]@parameters))
    {
        min = flowEnv[[x]]@min[[i]]
        max = flowEnv[[x]]@max[[i]]
		parameter = flowEnv[[x]]@parameters[[i]]
		
        attrs = c()
        if(min != -Inf) attrs = c(attrs, "gating:min" = min)
        if(max != Inf) attrs = c(attrs, "gating:max" = max)
		
		# TODO Refactor this piece of code
		if(is(parameter, "transformReference")) parameter = eval(parameter)
		if(is(parameter, "unitytransform")) attrs = c(attrs, "gating:compensation-ref" = "uncompensated")
        else if(is(parameter, "singleParameterTransform"))
		{
			attrs = c(attrs, "gating:transformation-ref" = parameter@transformationId)
			parameter = parameter@parameters
			if(is(parameter, "transformReference")) parameter = eval(parameter)
			
			if(is(parameter, "unitytransform")) attrs = c(attrs, "gating:compensation-ref" = "uncompensated")
			else if(is(parameter, "compensatedParameter"))
			{
				if (parameter@spillRefId != "SpillFromFCS") 
					attrs = c(attrs, "gating:compensation-ref" = filterIdtoXMLId(parameter@spillRefId))
				else 
					attrs = c(attrs, "gating:compensation-ref" = "FCS")
				parameter = parameter@parameters
			}
				
		} 
		else if(is(parameter, "compensatedParameter"))
		{
			if (parameter@spillRefId != "SpillFromFCS") 
				attrs = c(attrs, "gating:compensation-ref" = filterIdtoXMLId(parameter@spillRefId))
			else 
				attrs = c(attrs, "gating:compensation-ref" = "FCS")
			parameter = parameter@parameters
		} 
		else
		{
			cat(paste("This should not happen; unexpected parameter class", class(parameter))) # TODO
		}
		
        gatingMLNode$addNode("gating:dimension", attrs = attrs, close = FALSE)
        addDimensionContents(gatingMLNode, parameter, flowEnv)
        gatingMLNode$closeTag()
        
    }
    gatingMLNode$closeTag()
}

addDimensionContents <- function(gatingMLNode, parameter, flowEnv)
{
	if(is(parameter, "unitytransform")) attrs = c("data-type:name" = parameter@parameters)
	else if(is(parameter, "character")) attrs = c("data-type:name" = parameter)
	else {
		cat("\nThis should not happen!\n\n") # TODO
		attrs = c("data-type:name" = "????")
	}
    gatingMLNode$addNode("data-type:fcs-dimension", attrs = attrs)
}

filterIdtoXMLId <- function(x)
{
    # TODO figure out something safe
    x
}
