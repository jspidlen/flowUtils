
setGeneric("identifyNode",
           def=function(object,flowEnv,...) standardGeneric("identifyNode"),
           useAsDefault=function(object,flowEnv,...)
           {
               stop(paste("Not a supported node in GatingML format:", paste(object, collapse = ", "), sep = " "))
           }
)


setMethod("identifyNode",
          "http...www.isac.net.org.std.Gating.ML.v2.0.gating_PolygonGate",
          function(object, flowEnv, ...)
          {	
              gateId=(xmlGetAttr(object,"id",genid(flowEnv)))
              parentId=(xmlGetAttr(object,"parent_id","NULL"))
              dimensionList= xmlElementsByTagName(object,"dimension")
              len=length(dimensionList)	
              transformationList<-getTransformationList(dimensionList,flowEnv)
              vertexList=xmlElementsByTagName(object,"vertex")
              len=length(vertexList)
              vertexLimits=matrix(nrow=len,ncol=length(dimensionList))
              for (i in seq(len)) vertexLimits[i,]=getParameters(vertexList[[i]]) 
              filt=polygonGate(filterId=gateId,.gate=vertexLimits,transformationList)				

              if(parentId=="NULL")
              {
                  flowEnv[[as.character(gateId)]]=filt
              }
              else
              {
                  temp=new("filterReference",name=parentId,env=flowEnv,filterId="NULL")
                  flowEnv[[as.character(gateId)]]=new("subsetFilter",filters=list(filt,temp),filterId=gateId)    
              }
          }
)


setMethod("identifyNode",
	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_and",
 	  function(object,flowEnv,...)
	  {   gateList=xmlChildren(object)
              len=length(gateList)
              parameters=list()
              while(len>0)
              {   
                parameters[[len]]=identifyNode(gateList[[len]],flowEnv)
                len=len-1
              }
              new("intersectFilter",filterId="",filters=parameters)
   	  }
	 )

setMethod("identifyNode",
	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_or",
 	  function(object,flowEnv,...)
	  {
              gateList=xmlChildren(object)
              len=length(gateList)
              parameters=list()
              while(len>0)
              {
                  parameters[[len]]=identifyNode(gateList[[len]],flowEnv)
                  len=len-1
   	      }
              new("unionFilter",filterId="",filters=parameters)
   	  }
	 )
setMethod("identifyNode",
	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_not",
    	   function(object,flowEnv,...)
	   {
	      gateList=xmlChildren(object)
              len=length(gateList)
              parameters=list()
              if(len==1)
              {
                parameters[[len]]=identifyNode(gateList[[len]],flowEnv)
              }
              else
              {
                stop("Not element should have only one operand")
              }
              new("complementFilter",filterId="",filters=parameters)
	   }
	 )

setMethod("identifyNode",
          "http...www.isac.net.org.std.Gating.ML.v1.5.gating_gateReference",
          function(object,flowEnv,...)
	  {     
		gateRefId=getParameters(object)
                new("filterReference",name=gateRefId,env=flowEnv,filterId=gateRefId)
	  }
	 )

setMethod("identifyNode",
    	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_BooleanGate",
    	   function(object,flowEnv,...)
	   {  			
		gateId=(xmlGetAttr(object,"id",genid(flowEnv)))
                parentId=(xmlGetAttr(object,"parent_id","NULL"))
                filt<-identifyNode(xmlChildren(object)[[1]],flowEnv)
                if(parentId=="NULL")
                {
                    flowEnv[[as.character(gateId)]]=filt
                }
                else
                {
                    temp=new("filterReference",name=parentId,env=flowEnv,filterId="NULL")
                    flowEnv[[as.character(gateId)]]=new("subsetFilter",filters=list(filt,temp),filterId=gateId)
                }
               
           }
         ) 


setMethod("identifyNode",
          "http...www.isac.net.org.std.Gating.ML.v1.5.gating_RectangleGate",
          function(object,flowEnv,...)
	  {   
	        gateId=(xmlGetAttr(object,"id",genid(flowEnv)))
                parentId=(xmlGetAttr(object,"parent_id","NULL"))
                dimensionList=xmlElementsByTagName(object,"dimension")
		len=length(dimensionList)
                gateLimits=matrix(nrow=2,ncol=len)
                for (i in seq(len)) {gateLimits[,i]=getParameters(dimensionList[[i]]) }  
                transformationList<-getTransformationList(dimensionList,flowEnv)
                filt=rectangleGate(filterId=gateId,.gate=gateLimits,transformationList)
                if(parentId=="NULL")
                {
                  flowEnv[[as.character(gateId)]]=filt
                }
                else
                {		
                  temp=new("filterReference",name=parentId,env=flowEnv,filterId="NULL")
                  flowEnv[[as.character(gateId)]]=new("subsetFilter",filters=list(filt,temp),filterId=gateId)
                }		
          }
         ) 

setMethod("identifyNode",
    	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_PolygonGate",
    	   function(object,flowEnv,...)
	   {  			
              gateId=(xmlGetAttr(object,"id",genid(flowEnv)))
              parentId=(xmlGetAttr(object,"parent_id","NULL"))

              dimensionList= xmlElementsByTagName(object,"dimension")
              len=length(dimensionList)	
              transformationList<-getTransformationList(dimensionList,flowEnv)
              vertexList=xmlElementsByTagName(object,"vertex")
              len=length(vertexList)
              vertexLimits=matrix(nrow=len,ncol=length(dimensionList))
              for (i in seq(len)) {vertexLimits[i,]=getParameters(vertexList[[i]]) }  
                                
              filt=polygonGate(filterId=gateId,.gate=vertexLimits,transformationList)				
              if(parentId=="NULL")
              {
                flowEnv[[as.character(gateId)]]=filt
              }
              else
              {
                temp=new("filterReference",name=parentId,env=flowEnv,filterId="NULL")
                flowEnv[[as.character(gateId)]]=new("subsetFilter",filters=list(filt,temp),filterId=gateId)    
              }                      
	   }
         ) 


setMethod("identifyNode",
    	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_EllipsoidGate",
    	   function(object,flowEnv,...)
	   {  			
              
              gateId=(xmlGetAttr(object,"id",genid(flowEnv)))
              parentId=(xmlGetAttr(object,"parent_id","NULL"))
              dimensionList= xmlElementsByTagName(object,"dimension")
              len=length(dimensionList)	
              transformationList<-getTransformationList(dimensionList,flowEnv)
              meanList=xmlElementsByTagName(object,"mean")
              meanLimits=getParameters(meanList[[1]])
              covarianceList=xmlChildren(xmlElementsByTagName(object,"covarianceMatrix")[[1]])
              len=length(covarianceList)
              covMat=matrix(nrow=len,ncol=length(dimensionList))
              for (i in seq(len)) {covMat[i,]=getParameters(covarianceList[[i]]) }  
              distance=getParameters(xmlElementsByTagName(object,"distanceSquare")[[1]])
              filt=ellipsoidGate(filterId=gateId,.gate=covMat,mean=meanLimits,distance=distance,transformationList)
              if(parentId=="NULL")
              {
              flowEnv[[as.character(gateId)]]=filt
              }
              else
              {
                temp=new("filterReference",name=parentId,env=flowEnv,filterId="NULL")
                flowEnv[[as.character(gateId)]]=new("subsetFilter",filters=list(filt,temp),filterId=gateId)
              }
		
           }
         ) 
             
setMethod("identifyNode",
    	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_PolytopeGate",
    	   function(object,flowEnv,...)
	   {  			
              gateId=(xmlGetAttr(object,"id",genid(flowEnv)))
              parentId=(xmlGetAttr(object,"parent_id","NULL"))
              dimensionList= xmlElementsByTagName(object,"dimension")
              len=length(dimensionList)	
              transformationList<-getTransformationList(dimensionList,flowEnv)
              halfList=xmlElementsByTagName(object,"halfspace")
              len=length(halfList)
              halfValues=matrix(nrow=len,ncol=length(dimensionList)+1)
              for (i in seq(len)) {halfValues[i,]=sapply(xmlChildren(halfList[[i]]),getParameters) }  
              a=matrix(halfValues[,1:length(dimensionList)],ncol=length(dimensionList))
              b=(halfValues[,length(dimensionList)+1])
              filt=polytopeGate(filterId=gateId,.gate=a,b=b,transformationList)
              if(parentId=="NULL")
              {
                  flowEnv[[as.character(gateId)]]=filt
              }
              else
              {   
                  temp=new("filterReference",name=parentId,env=flowEnv,filterId="NULL")
                  flowEnv[[as.character(gateId)]]=new("subsetFilter",filters=list(filt,temp),filterId=gateId)
              }
           }
         )      

        
setMethod("identifyNode",
    	  "http...www.isac.net.org.std.Gating.ML.v1.5.gating_DecisionTreeGate",
    	   function(object,flowEnv,...)
	   {  			
	       
               gateId=(xmlGetAttr(object,"id",genid(flowEnv)))
               parentId=(xmlGetAttr(object,"parent_id","NULL"))

               root = xmlChildren(object)[[1]]
               test = decisionHelper(root)
               filt=do.call(expressionFilter,list(expr=test))
               if(parentId=="NULL")
               {
                 flowEnv[[as.character(gateId)]]=filt
               }
               else
               {  
                 temp=new("filterReference",name=parentId,env=flowEnv,filterId="NULL")
                 flowEnv[[as.character(gateId)]]=new("subsetFilter",filters=list(filt,temp),filterId=gateId)
               } 
                
           }
           
         )
