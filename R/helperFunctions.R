getTransformationList <- function(dimensionList, flowEnv)
{  
    len=length(dimensionList)
    transformationList=list()
    while(len>0)
    {    
        subNodes=xmlChildren(dimensionList[[len]])[[1]]
        temp=switch(names.XMLNode(dimensionList[[len]]),
            "transformation"={
                              transName=sapply(xmlChildren(subNodes),xmlName)
                              dispatchTransform(transName,subNodes,flowEnv)
                             },
            "parameter"=unitytransform(xmlGetAttr(subNodes,"name")),
            "fcs-dimension"=unitytransform(xmlGetAttr(subNodes,"name")), # TODO Gating-ML 2.0, transformations work differently
            "transformationReference"=transformReference(referenceId=as.character(xmlGetAttr(subNodes,"ref")),flowEnv)
                   ) 
        transformationList[[len]]=temp
        len=len-1
    }
    
    return(transformationList)
}

getTransformationListGml2 <- function(dimensionList, flowEnv)
{
    len = length(dimensionList)
    transformationRefs = list()
	compensationRefs = list()
    while (len > 0)
    {
        temp = xmlGetAttr(dimensionList[[len]], "transformation-ref")
        if (is.null(temp)) transformationRefs[[len]] = "unitytransform" 
        # Don't want to assign null to make sure the length of this list is right and the indexes match
        else transformationRefs[[len]] = temp
		
		temp = xmlGetAttr(dimensionList[[len]], "compensation-ref")
		if (is.null(temp)) compensationRefs[[len]] = "FCS"
		else compensationRefs[[len]] = temp

        len = len - 1
    }
    
    len = length(dimensionList)
    transformationList = list()
    while (len > 0)
    {   
        if (transformationRefs[[len]] == "unitytransform")
        {
            subNodes = xmlChildren(dimensionList[[len]])[[1]]
            temp = switch(names.XMLNode(dimensionList[[len]]),
                "fcs-dimension" = 
                {
					parName = xmlGetAttr(subNodes, "name")
					# TODO for FCS-based compensation and uncompensated parameters
					if (compensationRefs[[len]] == "FCS") 
					{
						compensatedParameter(
							parameters=parName,
							spillRefId="SpillFromFCS",
							transformationId=paste(parName, "_compensated_according_to_FCS"),
							searchEnv=flowEnv
						)
					}
					else if (compensationRefs[[len]] == "uncompensated") unitytransform(parName)
					else 
					{
						if (exists(parName, envir=flowEnv) 
							&& class(flowEnv[[parName]])[1] == "compensatedParameter" 
							&& flowEnv[[parName]]@spillRefId == compensationRefs[[len]])
						flowEnv[[parName]]
						else 
						{
							write(paste("Failed to use spillover/spectrum matrix ", compensationRefs[[len]], " for compensated parameter ", parName, ". It seems that the matrix was not properly defined in the Gating-ML file.\n", sep=""), stderr())
							unitytransform(parName)
						}
					}
                },
                "new-dimension" = unitytransform("TODO") #TODO
            )
        }
        else
        {
            subNodes = xmlChildren(dimensionList[[len]])[[1]]
            temp = switch(names.XMLNode(dimensionList[[len]]),
				# TODO Support compensation together with other trasforms!
                "fcs-dimension" = {
                    newId = createOrUseGml2Transformation(transformationRefs[[len]], xmlGetAttr(subNodes, "name"), flowEnv)
                    transformReference(referenceId=newId, flowEnv)
                },
                "new-dimension" = unitytransform("TODO") #TODO
            )
        }
        transformationList[[len]]=temp
        len=len-1
    }
    
    return(transformationList)
}

createOrUseGml2Transformation <- function(genericTransformationId, parameterName, flowEnv)
{
    appliedName <- paste(genericTransformationId, parameterName, sep = ".")
    if (!exists(appliedName, envir=flowEnv))
    {
        if (exists(genericTransformationId, envir=flowEnv))
        {
            # The generic transformation exists -> we will a transformation
            # applied to the specific FCS parameter based on the generic transformation
            appliedTransformation <- flowEnv[[genericTransformationId]]
            appliedTransformation@parameters = unitytransform(parameterName)
            appliedTransformation@transformationId = appliedName
            flowEnv[[appliedName]] <- appliedTransformation
        }
        else
        {
			write(paste("Failed to locate transformation ", genericTransformationId, ". It seems that the transformation was not defined in the Gating-ML file. You won't be able to apply gates that are using this transformation.\n", sep=""), stderr())
			# TODO fix doc, we no longer do this.
			
            # The generic transformation does not exists (it is probably defined later in the XML file)
            # -> We will same what transformation is needed in flowEnv[['transformationsToCreate']] and
            # then create all required applied transformations after the XML parsing is done.
			
#            if (exists('transformationsToCreate', envir=flowEnv)) transformationsToCreate <- flowEnv[['transformationsToCreate']]
#            else transformationsToCreate <- list()
#            if (is.null(transformationsToCreate[[genericTransformationId]])) transformationsToCreate[[genericTransformationId]] <- list()
#            transformationsToCreate[[genericTransformationId]][[parameterName]] <- parameterName
#            flowEnv[['transformationsToCreate']] <- transformationsToCreate
        }
    }
    appliedName
}

#createMissingAppliedTransforms <- function(flowEnv)
#{
#    if (exists('transformationsToCreate', envir=flowEnv))
#    {
#        for (genericTransformationId in names(flowEnv[['transformationsToCreate']]))
#        {
#            if (exists(genericTransformationId, envir=flowEnv))
#            {
#                for (parameterName in flowEnv[['transformationsToCreate']][[genericTransformationId]]) 
#                { 
#                    appliedName <- paste(genericTransformationId, parameterName, sep = ".")
#                    if (!exists(appliedName, envir=flowEnv))
#                    {
#                        # Create applied transformation based on the generic transformation by copying it and 
#                        # changing the FCS parameters and transformationId 
#                        appliedTransformation <- flowEnv[[genericTransformationId]]
#                        appliedTransformation@parameters = unitytransform(parameterName)
#                        appliedTransformation@transformationId = appliedName
#                        flowEnv[[appliedName]] <- appliedTransformation
#                    }
#                }
#            }
#            else
#            {
#                write(paste("Failed to locate transformation ", genericTransformationId, ". It seems that the transformation was not defined in the Gating-ML file. You won't be able to apply gates that are using this transformation.\n", sep=""), stderr())    
#            }
#        }
#        rm('transformationsToCreate', envir=flowEnv)
#    }
#}

# TODO: This should be done properly
getTransformationListForQuadrantGate <- function(quadrant, dividers, flowEnv) 
{
    len <- length(quadrant)
    transformationList <- list()
    while(len > 0)
    {   
        name <- names(quadrant)[[len]]
        parameterName <- dividers[[name]][[length(dividers[[name]])]]
        transformationList[[len]] <- unitytransform(parameterName)
        len <- len-1
    }
    return(transformationList)
}

getElementValueAsNumeric <- function(element)
{
    # This is ugly but it just extracts a numeric value of an element, 
    # i.e., <value>500</value> --> 500
    as.numeric((as.character((xmlChildren(element))[['text']]))[[6]])
}

getBounds <- function(value, name, dividers)
{
    lowerBound = -Inf
    upperBound = +Inf
    for (i in seq(length(dividers[[name]])-1))
    {
        if (dividers[[name]][[i]] < value) lowerBound = dividers[[name]][[i]] 
    }
    for (i in (length(dividers[[name]])-1):1)
    {
        if (dividers[[name]][[i]] > value) upperBound = dividers[[name]][[i]] 
    }
    c(lowerBound, upperBound)
}
    
getParameterList<-function(node,type,flowEnv)
{   parameters=list()
    nodeNames=names.XMLNode(node[[1]])
    len=length(nodeNames)
    subNodes=xmlChildren(node[[1]])

    for (i in seq(len))
    {
        temp=switch(nodeNames[i],
        "transformation"=
         {  
            if(type==0)
                trans<-subNodes     
            else
                trans<-subNodes[[i]]
            transName=sapply(xmlChildren(subNodes[[i]]),xmlName)
            dispatchTransform(transName,trans,flowEnv)
        },
        "parameter"=unitytransform(xmlGetAttr(subNodes[[i]],"name")),
        "transformationReference"=transformReference(xmlGetAttr(subNodes[[i]],"ref"),flowEnv)
                  ) 
        parameters[[i]]=temp
    }
    return(parameters)
}

getFluorochromeList<-function(node,flowEnv)
{   parameters=list()
	nodeNames=names.XMLNode(node[[1]])
	len=length(nodeNames)
	subNodes=xmlChildren(node[[1]])
	
	for (i in seq(len))
	{
		temp=switch(nodeNames[i],
				"fcs-dimension"=unitytransform(xmlGetAttr(subNodes[[i]],"name"))
		) 
		parameters[[i]]=temp
	}
	return(parameters)
}

getDetectorList<-function(node,flowEnv)
{   parameters=list()
	nodeNames=names.XMLNode(node[[2]])
	len=length(nodeNames)
	subNodes=xmlChildren(node[[2]])
	
	for (i in seq(len))
	{
		temp=switch(nodeNames[i],
				"fcs-dimension"=unitytransform(xmlGetAttr(subNodes[[i]],"name"))
		) 
		parameters[[i]]=temp
	}
	return(parameters)
}

getSide = function(g,side) 
    {       
          leaf = paste("http...www.isac.net.org.std.Gating.ML.v1.5.gating_leaf",side,sep="")
          node = paste("http...www.isac.net.org.std.Gating.ML.v1.5.gating_node",side,sep="")
          VAL=xmlElementsByTagName(g,paste("leaf",side,sep=""))
          if(length(VAL)==0) 
          {
            VAL=  xmlElementsByTagName(g,paste("node",side,sep=""))
            if(length(VAL)==0) stop(paste(leaf,"or",node,"is required at all levels of a decision tree."))
          }
          VAL[[1]]
    }
            
makeCall = function(param,thres,LT,GTE) 
        {       
          NUM = if((is.logical(LT) && LT) || is.call(LT)) 1 else 0
          NUM = NUM + if((is.logical(GTE) && GTE) || is.call(GTE)) 2 else 0
            
          LESS  = as.call(c(as.symbol("<"),as.symbol(param),thres))
          MORE  = as.call(c(as.symbol(">="),as.symbol(param),thres))
          switch(NUM+1,
          {FALSE},
          {if(is.logical(LT)) LESS else as.call(c(as.symbol("&"),LESS,LT))},
          {if(is.logical(GTE)) MORE else as.call(c(as.symbol("&"),MORE,GTE))},
          {as.call(c(as.symbol("|"),as.call(c(as.symbol("&"),LESS,LT)),as.call(c(as.symbol("&"),MORE,GTE))))}
                )
        }
decisionHelper = function(g,...) 
{       
          
      thres = xmlGetAttr(g,"threshold",Inf,as.numeric)
      param = getParameters(xmlChildren(g)[[1]])
      LT    = getSide(g,"LT")
      GTE   = getSide(g,"GTE")
      if(is(LT,"http...www.isac.net.org.std.Gating.ML.v1.5.gating_leafLT"))
      {       
              LT  = if(xmlGetAttr(LT,"inside")=="true") TRUE else FALSE 
      }    
      else
      {
              LT  = decisionHelper(LT)
      }
      if(is(GTE,"http...www.isac.net.org.std.Gating.ML.v1.5.gating_leafGTE")) 
      {
              GTE = if(xmlGetAttr(GTE,"inside")=="true") TRUE else FALSE 
      }
      else 
      {    
              GTE = decisionHelper(GTE)
      }
      
      makeCall(param,thres,LT,GTE)
        
}            


    
smartTreeParse = function(file,...) 
{
  handlers = list(comment=function(x,...)
  {
      NULL
  },
  startElement=function(x,...) 
  {
      class(x)=c(paste(make.names(c(xmlNamespace(x),xmlName(x))),collapse="_"),make.names(xmlNamespace(x)),class(x))
      x
  }
  )
  xmlTreeParse(file,handlers=handlers,asTree=TRUE,fullNamespaceInfo=TRUE,...)
}


performGateTest<-function(gateId,fcs,expectedResult,flowEnv)
{   
    gateFound<-FALSE
    gate<-NULL
    gate<-flowEnv[[gateId]]
    if(!is.null(gate))
    {   
        gateFound<-TRUE
        result<-as.numeric(filter(fcs,gate)@subSet)
        lenResult=length(result)
        lenExpected=length(expectedResult[,1])
    }
 
    message<-paste("Gate",gateId,"not found")
    checkTrue(gateFound, message)

    message<-paste("Number of events do not correspond to number of events in expected results file, for", gateId,"Wrong event numbers")
    checkTrue(lenResult == lenExpected,message)

    evMatch <- as.logical(unlist(expectedResult) == result)
    eventsCorrect<-TRUE
    message<-paste("Numbers in gate and expected numbers don't match:<br>")
    message<-paste(message,"\t", sum(expectedResult), "vs", sum(result))
    if(!all(evMatch)) 
        {
        eventsCorrect<-FALSE
        }
          checkTrue(eventsCorrect, message)
 }

testGatingCompliance<-function(file)
{
  testsuite <- defineTestSuite(
  "GatingTestSuite",
  dir=system.file("RUnitScript_Files", package="flowUtils"),
  testFileRegexp="^runit.+\\.[rR]$", 
  testFuncRegexp="^test.+")

  testResult <- runTestSuite(testsuite)

  printHTMLProtocol(testResult,file=paste(file,".html",sep=""))

}
