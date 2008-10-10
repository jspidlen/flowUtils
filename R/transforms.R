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

