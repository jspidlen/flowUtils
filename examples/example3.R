library('flowUtils')

#####################################################################################
# Create an ellipse gate on compensated parameters and write it to a Gating-ML file #
#####################################################################################
flowEnv <- new.env()
covM <- matrix(c(62.5, 37.5, 37.5, 62.5), nrow = 2, byrow=TRUE)
colnames(covM) <- c("FL1-H", "FL2-H")
compPars <- list(
  compensatedParameter(parameters="FL1-H", spillRefId="SpillFromFCS", 
    transformationId=paste("FL1-H", "_compensated_according_to_FCS", sep=""), 
    searchEnv=flowEnv),
  compensatedParameter(parameters="FL2-H", spillRefId="SpillFromFCS", 
    transformationId=paste("FL2-H", "_compensated_according_to_FCS", sep=""), 
    searchEnv=flowEnv)
)
myEl <- ellipsoidGate(mean=c(12, 16), distance=1, .gate=covM, filterId="myEl")
myEl@parameters <- new("parameters", compPars)
flowEnv[['myEl']] <- myEl
write.gatingML(flowEnv)
# Use write.gatingML(flowEnv, filename)
# if you want the output to go to a file 

