transCompensation <- function(node, flowEnv)
{
    transformationId = (xmlGetAttr(node, "id", genid(flowEnv)))
    tempComp = xmlElementsByTagName(node, "compensation")
    spillRef = xmlGetAttr(tempComp[[1]], "spilloverMatrixRef")
    parameter = getParameters(xmlChildren(tempComp[[1]])[[1]])
    compensatedParameter(
        parameters=parameter,
        spillRefId=spillRef,
        transformationId=transformationId,
        searchEnv=flowEnv
    )
}

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v1.5.transformations_spilloverMatrix",
    function(object, flowEnv, ...)
    {
        transformationId = (xmlGetAttr(object, "id", genid(flowEnv)))
        tempCoeff = xmlElementsByTagName(object, "coefficient", recursive=TRUE)
        coefficients = as.numeric(sapply(tempCoeff, getParameters))
        parameters <- getParameterList(object, 0, flowEnv)  
        spillMatrix = matrix(coefficients, ncol=length(parameters), byrow=TRUE)
        colnames(spillMatrix) = sapply(parameters, getParameters)
        flowEnv[[as.character(transformationId)]] =
            compensation(spillover=spillMatrix, compensationId=as.character(transformationId), parameters)
    }
)

setMethod(
    "identifyNode",
    "http...www.isac.net.org.std.Gating.ML.v2.0.transformations_spectrumMatrix",
    function(object, flowEnv, ...)
    {
        transformationId = (xmlGetAttr(object, "id", genid(flowEnv)))
        tempCoeff = xmlElementsByTagName(object, "coefficient", recursive=TRUE)
        coefficients = as.numeric(sapply(tempCoeff, getParameters))
        fluorochromes = getFluorochromeList(object, flowEnv)
        detectors = getDetectorList(object, flowEnv)
        if (length(fluorochromes) != length(detectors))
        {
            write(paste("Only square spillover (spectrum) matrices are currently supported. Matrix ", transformationId, " cannot be used.\n", sep=""), stderr())
        }
        else
        {
            spillMatrix = matrix(coefficients, ncol=length(detectors), byrow=TRUE)
            colnames(spillMatrix) = sapply(detectors, getParameters)
            rownames(spillMatrix) = sapply(fluorochromes, getParameters)
            spillId = as.character(transformationId) 
            flowEnv[[spillId]] = compensation(spillover=spillMatrix, compensationId=spillId, detectors)
            
            len = length(fluorochromes)
            while (len > 0)
            {
                compPar = compensatedParameter(
                    parameters=as.character(detectors[[len]]@parameters),
                    spillRefId=spillId,
                    transformationId=as.character(fluorochromes[[len]]@parameters),
                    searchEnv=flowEnv
                )
                flowEnv[[as.character(fluorochromes[[len]]@parameters)]] = compPar
                len = len - 1
            }
        }
    }
)
