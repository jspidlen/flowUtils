
transCompensation<-function(node,flowEnv)
{
  
      transformationId=(xmlGetAttr(node,"id",genid()))
      tempComp=xmlElementsByTagName(node,"compensation")
      spillRef=xmlGetAttr(tempComp[[1]],"spilloverMatrixRef")
      parameter=getParameters(xmlChildren(tempComp[[1]])[[1]])
      compensatedParameter(parameters=parameter,
                                 spillRefId=spillRef,
                                 transformationId=transformationId,
                                 searchEnv=flowEnv
                                )
}

setMethod("identifyNode",
          "http...www.isac.net.org.std.Gating.ML.v1.5.transformations_spilloverMatrix",
          function(object,flowEnv,...)
          {   
              transformationId=(xmlGetAttr(object,"id",genid()))
              tempCoeff=xmlElementsByTagName(object,"coefficient",recursive=TRUE)
              coefficients=as.numeric(sapply(tempCoeff,getParameters))
              parameters<-getParameterList(object,0,flowEnv)  
              spillMatrix=matrix(coefficients,ncol=length(parameters),byrow=TRUE)
              colnames(spillMatrix)=sapply(parameters,getParameters)
              flowEnv[[as.character(transformationId)]]=
                      compensation(spillover=spillMatrix,compensationId=as.character(transformationId),parameters )
          }
         )	

