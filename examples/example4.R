library('flowUtils')

#############################################################################
# Create an quad gate on scaled parameters and write it to a Gating-ML file #
#############################################################################

flowEnv=new.env()
myTrQuad <- quadGate(filterId = "myTrQuad", "APC-A" = 0.5, "APC-Cy7-A" = 0.5)
trArcSinH1 = asinhtGml2(parameters = "APC-A", 
  T = 1000, M = 4.5, A = 0, transformationId="trArcSinH1")
trLogicle1 = logicletGml2(parameters = "APC-Cy7-A", 
  T = 1000, W = 0.5, M = 4.5, A = 0, transformationId="trLogicle1", 
  boundMin = 0.01, boundMax = 0.95)
flowEnv[['trArcSinH1']] <- trArcSinH1
flowEnv[['trLogicle1']] <- trLogicle1
trPars = list(
  transformReference("trArcSinH1", flowEnv),
  transformReference("trLogicle1", flowEnv)
)
myTrQuad@parameters = new("parameters", trPars)
flowEnv[['myTrQuad']] <- myTrQuad
write.gatingML(flowEnv)
# Use write.gatingML(flowEnv, filename)
# if you want the output to go to a file 
