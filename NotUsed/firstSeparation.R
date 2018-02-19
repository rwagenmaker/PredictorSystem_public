 library(knitr)

generateTables <- function() {
	# ---------------------------------------------------------
	# FIND THE INFORMATION ABOUT THE PREVIOUS MODELED FUNCTIONS
	modelResultsLocaton <- file.path(getwd(), "Demos", "AnalysisData", "ResultsTables")
	fileLocation_Primitive <- file.path(modelResultsLocaton,file = paste("Primitive_ModelResultsDF.Rda" ,sep = ""))
	

	if(file.exists(fileLocation_Primitive)) {

		modelResults_Primitive <- readRDS(fileLocation_Primitive)

		slope0 <- subset(modelResults_Primitive, Slope == 0.0 ,)# select=c(FunctionName, Slope)) 
		#slope0 <- modelResults_Primitive[which(modelResults_Primitive$Slope<1),]# which(modelResults_Primitive$Slope == 0.0)
		#kable(slope0)

		directoryLocation <- file.path(getwd(), "1stAnalysisTables")
		dir.create(directoryLocation, showWarnings = FALSE)
		fileLocation <- file.path(directoryLocation,file = paste("lowComplexityTable.Rda" ,sep = ""))

		saveRDS(slope0,file=fileLocation)
	}

	#kable(slope0)



	



}

# ----------------------------------------------------------------------------------
# CHECKS IF THE GIVEN FUNCTION IS IN THE LOW COMPLEXITY TABLE, AND RETURNS A BOOLEAN
isNonComplex <- function(expression) {

	# -------------------------------------------------------------------
	# CHECKS IF THE DIRECTORY WHERE THE TABLES SHOULD BE ACTUALLY EXIST
	directoryCheck <- file.path(getwd(), "1stAnalysisTables")
	fileLocation <- file.path(directoryCheck,file = paste("lowComplexityTable.Rda" ,sep = ""))	


	if (dir.exists(directoryCheck)){		
		slope0DF <- readRDS(fileLocation)
		
	} else {
		generateTables()
		slope0DF <- readRDS(fileLocation)
	}



	# -------------------------------------------------------------------
	# CHECKS IF THE "expression" IS IN THE GROUP OF SLOPE0 DATA FRAME
	isLowComplexity <- expression %in% slope0DF$FunctionName   # subset(modelResults_Primitive, Slope == 0.0 ,)
	#print(isLowComplexity)
	#kable(slope0DF)

	return(isLowComplexity)
}

#generateTables()

#isFirstPhaseComplex("names")