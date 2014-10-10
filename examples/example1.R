library('flowUtils')
library('gatingMLData')

#######################################################################
# Read an FCS file and a Gating-ML file, and apply a gate on the data #
#######################################################################
flowEnv <- new.env()
gateFile <- system.file("extdata/Gml2/Gating-MLFiles", 
  "gates1.xml", package="gatingMLData")
read.gatingML(gateFile, flowEnv)
ls(flowEnv)

fcsFile <- system.file("extdata/Gml2/FCSFiles", "data1.fcs", package="gatingMLData")
fcs <- read.FCS(fcsFile, transformation="linearize-with-PnG-scaling")
result = filter(fcs, flowEnv$Polygon1)
summary(result)
