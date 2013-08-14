
getTransformationList<-function(dimensionList,flowEnv)
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
