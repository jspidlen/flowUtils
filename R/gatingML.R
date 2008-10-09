
####Generic function
setGeneric("parseGatingML",def=function(object,flowEnv,...)
standardGeneric("parseGatingML"),
	    useAsDefault=function(object,flowEnv,...)
			 {
				stop("Not a supported Gating-ML XML format")
			 }
	  )
 
### Methods definitions
setMethod("parseGatingML","http...www.isac.net.org.std.Gating.ML.v1.5.gating_Gating.ML",
          function(object,flowEnv,...)
          {   
              idnum = 0
              genid = function()
              {
                  idnum <<- idnum + 1;
                  paste("genid",idnum,sep="")
              }
        
        
              for (node in xmlChildren(object))
              {
                  identifyNode(node,flowEnv);
              }
              
        }
      )

read.gatingML = function(file,flowEnv,...)
{       
	 parseGatingML(xmlRoot(smartTreeParse(file,...)),flowEnv)
        
}
