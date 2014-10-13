library('flowUtils')

######################################################################################
# Create a simple rectangular gate in FSC-A and SSC-A and export it in Gating-ML 2.0 #
######################################################################################
flowEnv=new.env()
gateRect <- rectangleGate(filterId = "gateRect", 
                          "FSC-A" = c(500, 12000), "SSC-A" = c(1000, 24000))
flowEnv[['gateRect']] <- gateRect
write.gatingML(flowEnv)
# Use write.gatingML(flowEnv, filename)
# if you want the output to go to a file 
