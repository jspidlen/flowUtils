fcsFile <-  system.file("extdata/Gml2/FCSFiles", "data1.fcs", package = "gatingMLData")
gateFile <- system.file("extdata/Gml2/Gating-MLFiles","gates1.xml", package = "gatingMLData")
csvFile <-  system.file("extdata/Gml2/ExpectedResults/set_1", package = "gatingMLData")

flowEnv=new.env()
read.gatingML(gateFile, flowEnv)
fcs <- read.FCS(fcsFile)

test.Results_And1 <- function() 
{
    gateId  <- "And1"
    csvFile <- paste(csvFile, .Platform$file.sep, "Results_", gateId, ".txt", sep="")
    expectedResult <- read.csv(csvFile, header=FALSE)
    flowUtils:::performGateTest(gateId, fcs, expectedResult, flowEnv)
}

