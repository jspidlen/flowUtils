setMethod("identifyNode", "http...www.isac.net.org.std.Gating.ML.v2.0.transformations_transformation",
    function(object,flowEnv,...)
    {
        if (flowEnv[['.flowUtilsRead.GatingML.PassNo']] == 1)
		{
            transName=sapply(xmlChildren(object), xmlName)
            dispatchGatingML2Transform(transName, object, flowEnv)
		}
    }
)

dispatchGatingML2Transform <- function(transName, node, flowEnv)
{
    temp = switch(transName,
                "fasinh" = fasinh(node, flowEnv),
				"flin" = flin(node, flowEnv),
				"flog" = flog(node, flowEnv)
                )

    name=as.character(slot(temp,"transformationId"))
    flowEnv[[name]]=temp
    temp
}



setMethod("identifyNode","http...www.isac.net.org.std.Gating.ML.v1.5.transformations_transformation",
          function(object,flowEnv,...)
	  {  
	      transName=sapply(xmlChildren(object),xmlName)
              dispatchTransform(transName,object,flowEnv)
	  } 
       	 )
         
dispatchTransform<-function(transName,node,flowEnv)
{      
    temp=switch(transName,
                "split-scale"=transSplitScale(node,flowEnv),
                "ratio"=transRatio(node,flowEnv),
                "dg1polynomial"=transDg1polynomial(node,flowEnv),
                "quadratic"=transQuadratic(node,flowEnv),
                "sqrt"=transSquareroot(node,flowEnv),
                "ln"=transLogarithm(node,flowEnv),
                "exponential"=transExponential(node,flowEnv),
                "asinh"=transInverseHyperbolicSin(node,flowEnv),
                "sinh"=transHyperbolicSin(node,flowEnv),
                "inverse-split-scale"=transInvSplitScale(node,flowEnv),
                "compensation"=transCompensation(node,flowEnv),
                "hyperlog"=transHyperLog(node,flowEnv),
                "EH"=transEH(node,flowEnv)
                )

    name=as.character(slot(temp,"transformationId"))
    flowEnv[[name]]=temp
    temp
                  
}

####################################################
####--------- Gating-ML 2.0 transforms -------------
####################################################

####----------- fasinh transformation --------------
fasinh <- function(node, flowEnv)
{       
    transformationId = (xmlGetAttr(node, "id", genid(flowEnv)))
    coefficientList = xmlElementsByTagName(node, "fasinh", recursive=FALSE)
    pT = sapply(coefficientList, xmlGetAttr, "T", default=262144) # It's questionable whether to use
    pM = sapply(coefficientList, xmlGetAttr, "M", default=4.5)    # defaults here or just let the
    pA = sapply(coefficientList, xmlGetAttr, "A", default=0)      # method fail...

    # Gating-ML 2.0 transforms are defined as applicable to any FCS parameters, so we will create one
    # "placeholder" transformation with parameters = "any" and later on, when a transformation is 
    # actually needed (i.e., paired with FCS parameters), then we will "copy" this transformation and
    # fill out the parameters accodingly.
	
    return(asinhtGml2(parameters = "any", T = as.numeric(pT), M = as.numeric(pM), 
        A = as.numeric(pA), transformationId = transformationId))
}

####----------- fasinh transformation --------------
flin <- function(node, flowEnv)
{       
    transformationId = (xmlGetAttr(node, "id", genid(flowEnv)))
    coefficientList = xmlElementsByTagName(node, "flin", recursive=FALSE)
    pT = sapply(coefficientList, xmlGetAttr, "T", default=262144) # It's questionable whether to use defaults
    pA = sapply(coefficientList, xmlGetAttr, "A", default=0)      # here or just let the method fail...

    # Gating-ML 2.0 transforms are defined as applicable to any FCS parameters, so we will create one
    # "placeholder" transformation with parameters = "any" and later on, when a transformation is 
    # actually needed (i.e., paired with FCS parameters), then we will "copy" this transformation and
    # fill out the parameters accodingly.

    return(lintGml2(parameters = "any", T = as.numeric(pT), A = as.numeric(pA), 
        transformationId = transformationId))
}

####----------- fasinh transformation --------------
flog <- function(node, flowEnv)
{       
	transformationId = (xmlGetAttr(node, "id", genid(flowEnv)))
	coefficientList = xmlElementsByTagName(node, "flog", recursive=FALSE)
	pT = sapply(coefficientList, xmlGetAttr, "T", default=262144) # It's questionable whether to use defaults
	pM = sapply(coefficientList, xmlGetAttr, "M", default=4.5)    # here or just let the method fail...
	
	# Gating-ML 2.0 transforms are defined as applicable to any FCS parameters, so we will create one
	# "placeholder" transformation with parameters = "any" and later on, when a transformation is 
	# actually needed (i.e., paired with FCS parameters), then we will "copy" this transformation and
	# fill out the parameters accodingly.
	
	return(logtGml2(parameters = "any", T = as.numeric(pT), M = as.numeric(pM), 
					transformationId = transformationId))
}


####################################################
####--------- Gating-ML 1.5 transforms -------------
####################################################

####----------Degree n polynomial ------------------

transDg1polynomial<-function(node,flowEnv)
{        
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"coefficient",recursive=TRUE)
    len=length(coefficientList)-1
    values=sapply(coefficientList,xmlGetAttr,"value")		
    a=as.numeric(values[1:len])
    b=as.numeric(values[len+1])
    parameters<-getParameterList(node,0,flowEnv)
    parm=new("parameters",.Data=parameters)
    return(dg1polynomial(parameters=parm,a=a,b=b,transformationId=transformationId))				
}
####-----------Ratio transformation ------------------
transRatio<-function(node,flowEnv)
{      
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    parameters<-getParameterList(node,1,flowEnv)
    return(ratio(numerator=parameters[[1]],denominator=parameters[[2]],transformationId=transformationId))			
}

####-----------Quadratic transformation ------------------

transQuadratic<-function(node,flowEnv)
{       
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"quadratic",recursive=FALSE)
    coefficients=sapply(coefficientList,xmlGetAttr,"a")		
    parameters<-getParameterList(node,1,flowEnv)
    return(quadratic(parameters=parameters[[1]],a=as.numeric(coefficients),transformationId=transformationId))	

}
	
####-----------Square root transformation ------------------
transSquareroot<-function(node,flowEnv)
{      
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"sqrt",recursive=FALSE)
    coefficients=sapply(coefficientList,xmlGetAttr,"a")		
    parameters<-getParameterList(node,1,flowEnv)
    return(squareroot(parameters=parameters[[1]],a=as.numeric(coefficients),transformationId=transformationId))
}
####-----------Logarithmic transformation ------------------	
transLogarithm<-function(node,flowEnv)
{      
        
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"ln",recursive=FALSE)
    a=sapply(coefficientList,xmlGetAttr,"a")		
    b=sapply(coefficientList,xmlGetAttr,"b")		
    parameters<-getParameterList(node,0,flowEnv)   
    return(logarithm(parameters=parameters[[1]],a=as.numeric(a),b=as.numeric(b),transformationId=transformationId))
}
####-----------Exponential transformation ------------------	
transExponential<-function(node,flowEnv)
{       
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"exponential",recursive=FALSE)
    a=sapply(coefficientList,xmlGetAttr,"a")		
    b=sapply(coefficientList,xmlGetAttr,"b")		
    parameters<-getParameterList(node,1,flowEnv)   
    return(exponential(parameters=parameters[[1]],a=as.numeric(a),b=as.numeric(b),transformationId=transformationId))
}
####-----------Inverse Hyperbolic sin transformation ------------------	
transInverseHyperbolicSin<-function(node,flowEnv)
{       
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"asinh",recursive=FALSE)
    a=sapply(coefficientList,xmlGetAttr,"a")		
    b=sapply(coefficientList,xmlGetAttr,"b")		
    parameters<-getParameterList(node,0,flowEnv)   
    return(asinht(parameters=parameters[[1]],a=as.numeric(a),b=as.numeric(b),transformationId=transformationId))
}

####-----------Hyperbolic sin transformation ------------------	
transHyperbolicSin<-function(node,flowEnv)
{      
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"sinh",recursive=FALSE)
    a=sapply(coefficientList,xmlGetAttr,"a")		
    b=sapply(coefficientList,xmlGetAttr,"b")		
    parameters<-getParameterList(node,0,flowEnv)  
    return(sinht(parameters=parameters[[1]],a=as.numeric(a),b=as.numeric(b),transformationId=transformationId))
}

####-----------Split scale transformation ------------------	
transSplitScale<-function(node,flowEnv)
{      
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"split-scale",recursive=FALSE)
    r=sapply(coefficientList,xmlGetAttr,"r")		
    maxValue=sapply(coefficientList,xmlGetAttr,"maxValue")	
    transitionChannel=sapply(coefficientList,xmlGetAttr,"transitionChannel")		
    parameters<-getParameterList(node,0,flowEnv)  
    return(splitscale(parameters=parameters[[1]],r=as.numeric(r),maxValue=as.numeric(maxValue),transitionChannel=as.numeric(transitionChannel),transformationId=transformationId))
}


####-----------Inverse Split scale transformation ------------------	
transInvSplitScale<-function(node,flowEnv)
{      
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"inverse-split-scale",recursive=FALSE)
    r=sapply(coefficientList,xmlGetAttr,"r")		
    maxValue=sapply(coefficientList,xmlGetAttr,"maxValue")	
    transitionChannel=sapply(coefficientList,xmlGetAttr,"transitionChannel")		
    parameters<-getParameterList(node,0,flowEnv)  
    return(invsplitscale(parameters=parameters[[1]],r=as.numeric(r),maxValue=as.numeric(maxValue),transitionChannel=as.numeric(transitionChannel),transformationId=transformationId))
}

transHyperLog<-function(node,flowEnv)
{     
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"hyperlog",recursive=FALSE)
    a=sapply(coefficientList,xmlGetAttr,"a")		
    b=sapply(coefficientList,xmlGetAttr,"b")		
    parameters<-getParameterList(node,0,flowEnv)  
    return(hyperlog(parameters=parameters[[1]],a=as.numeric(a),b=as.numeric(b),transformationId=transformationId))
}

transEH<-function(node,flowEnv)
{     
    transformationId=(xmlGetAttr(node,"id",genid(flowEnv)))
    coefficientList=xmlElementsByTagName(node,"EH",recursive=FALSE)
    a=sapply(coefficientList,xmlGetAttr,"a")		
    b=sapply(coefficientList,xmlGetAttr,"b")		
    parameters<-getParameterList(node,0,flowEnv)  
    return(EHtrans(parameters=parameters[[1]],a=as.numeric(a),b=as.numeric(b),transformationId=transformationId))
}

