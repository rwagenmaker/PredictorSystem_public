
source("../RExecutor/realArgumentsMiner.R",chdir = TRUE)
source("historyReader.R")
newworkdir<-"../../RExecutor/"



saveEnvironment <- function(envir=.GlobalEnv){
	environment <- list()
	for (var in ls(envir=envir,all.names = TRUE)) {
		environment[[paste(var)]] <- get(paste(var))
	}

	e <- as.environment(environment)
	parent.env(e)<-parent.frame()
	return(e)
}


#-------------------------------------------------------------------
# PREDICTION TYPE:
#				- Predict the size of the returning object ("ReturnObjectSize")
#				- Predict the time the expression is going to take ("ExecutionTime")
# PACKAGE NAME:
#				- Used to identify the 'majorargs' of the specific function
#				- Used by the 'historyReader', searching for the previous records that match this new entry
#
predictor <- function(functCommand, predictionType, packageName, functionName="", printValues = FALSE, memorySize = NULL, workingDir="../") {

	if(printValues) {
		cat("\n\n======================================================================================================",
		"\n========================================= HISTORY READER =============================================",
		"\n======================================================================================================\n" )
		print(functionName)	}

	DF_wrap<-historyReader_majorArgs(paste(functionName), packageName, argsPassed, printValue=FALSE, workingDirectory=workingDir)

	if(!is.null(DF_wrap) && DF_wrap=="NO_HISTORY"){
		return("NO_HISTORY")
	}

	DF <- DF_wrap[["DF"]]
	isDatasetNumeric <- DF_wrap[["isNumericBased"]]
	#attach(DF)
	#print(kable(DF))

	majorArgs <- getMajorArgs(functionName,packageName=packageName,  workingDirectory=workingDir)

	#======================================================================================================
	#============================ generate profile with 'freshExecutionArgs()' ============================
	#======================================================================================================	

	freshAnalysisWrap<-freshExecutionArgs(functCommand, workDir=workingDir)
	if(!exists("debugMode") || debugMode==TRUE){print(str(freshAnalysisWrap))}

	#======================================================================================================
	#============================== READ THE FRESH EXECUTION ANALYSIS =====================================
	#======================================================================================================

	functionName <- freshAnalysisWrap[["FunctionName"]]
	freshAnalysis <- freshAnalysisWrap[["Analysis"]]
	argsPassedStr <- paste(sort(head(names(freshAnalysis),-1)), sep="", collapse=";")
	argsPassed <- sort(head(names(freshAnalysis),-1))
	
	newDF<-DF[DF$ArgsPassed==argsPassedStr,]
	print(kable(newDF))
	cat("\n")
	if(NROW(newDF)>1){
		DF <- newDF
	}

	isNumericBased <- TRUE
	sizeOfFreshEntry <- 0
	totalNumericValue <- NA
	if(majorArgs!="" && (!is.null(majorArgs) || length(majorArgs)>0)){
		argsOfValue <- majorArgs
	} else {
		argsOfValue <- argsPassed#IF DOESN'T HAVE OR COULD NOT BE FOUND ANY MAJOR ARGS. THEN USES THE ARGS WITHOUT ANY RESTRICTION
	}
	#--------------------------------------------------------------------------
	# USING ONLY THE MAJOR ARGS, ESTIMATE THE SIZE OF THE ARGUMENTS
	for(arg in argsOfValue){

		ret <- calculateArgSize(freshAnalysis, arg, isNumericBased, sizeOfFreshEntry, totalNumericValue)
		isNumericBased <- ret[["isNumericBased"]]
		sizeOfFreshEntry <- ret[["sizeOfFreshEntry"]]
		totalNumericValue <- ret[["totalNumericValue"]]

	}

	if(sizeOfFreshEntry==0){
		for(arg in argsPassed){

			ret <- calculateArgSize(freshAnalysis, arg, isNumericBased, sizeOfFreshEntry, totalNumericValue)
			isNumericBased <- ret[["isNumericBased"]]
			sizeOfFreshEntry <- ret[["sizeOfFreshEntry"]]
			totalNumericValue <- ret[["totalNumericValue"]]
			argAnalysis <- freshAnalysis[[paste(arg)]]
		}
	}


	#======================================================================================================
	#=============================== SIZE OF FRESH ENTRY ==================================================
	#======================================================================================================
	

	sizeOfFreshEntry <- round(sizeOfFreshEntry/1000, digits=2)

	#	cat("\n --- Fresh Entry --- ::: MemorySize:",sizeOfFreshEntry,"\n")
	#	cat(" --- Fresh Entry --- ::: NumericValue:",totalNumericValue,"\n")
	#	cat(" --- Fresh Entry --- ::: IsNumericBased:",isNumericBased,"\n")
	#	cat("\n --- DATASET --- ::: IsNumericBased:",isDatasetNumeric,"\n")	}

	#======================================================================================================
	#============================== PERFORM PREDICTION WITH REGRESSION ====================================
	#======================================================================================================
	

	if(isNumericBased && isDatasetNumeric) {
		useNumericValue <- TRUE
	} else {
		useNumericValue <- FALSE
	}

	degree = 1

	rList=list()

	if (predictionType=="TIME_&_SIZE") {
		rList[["r_execTime"]] <- predictTime(sizeOfFreshEntry,totalNumericValue, useNumericValue, DF,degree,printValues)
		rList[["r_returnSize"]] <- predictSize(sizeOfFreshEntry,totalNumericValue, useNumericValue, DF,degree,printValues)
	} 

	if(!exists("debugMode") || debugMode==TRUE){
		cat("\n\n --- prediction final list --- \n\n")
		print(str(rList))
		print(rList[["r_execTime"]][["EstimatedValue"]])
	}

	return(rList)
}

#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#


calculateArgSize <- function(freshAnalysis, arg, isNumericBased, sizeOfFreshEntry, totalNumericValue) {

	argAnalysis <- freshAnalysis[[paste(arg)]]


	if((!is.null(argAnalysis)) && is.null(argAnalysis[["NOTFOUND"]]) ) {
		argTypeof <- argAnalysis[["typeof"]]
		argClass <- argAnalysis[["class"]]
		if(argClass=="TEMP_SIZE_LIST"){isNumericBased<-FALSE}
		
		argNROW <- argAnalysis[["NROW"]]
		argNCOL <- argAnalysis[["NCOL"]]
		argLength <- argAnalysis[["length"]]
		#----------------------------------------------------------------------------
		# OBJECT CLASSES CURRENTLY BEING USED TO RETRIEVE A FINAL VALUE OF MEMORY USE			

		if(argTypeof=="language") {
			
			typeofLang <- argAnalysis[["typeOfLanguage"]]
			innerObjects <- argAnalysis[["objectsInsight"]]

			
			if(typeofLang=="builtin" && length(innerObjects)==1) {
				innerSingleObject <- innerObjects[[1]]
				
				if(innerSingleObject[["class"]]=="numeric" && innerSingleObject[["length"]]==1) {
					innerValue <-  innerSingleObject[["value"]]
					if(!exists("debugMode") || debugMode==TRUE){cat("\n@builtinLang : ",innerValue)}
					totalNumericValue<-addToNumericValue(totalNumericValue, innerValue)
				}
			
			}  else {
					isNumericBased <- FALSE
			}

			sizeObjectsUsed <- argAnalysis[["sizeObjectsUsed"]] 
			if (sizeObjectsUsed==0){
				sizeOfFreshEntry <- sizeOfFreshEntry +  argAnalysis[["objectSize"]] 
			} else {
				sizeOfFreshEntry <- sizeOfFreshEntry + sizeObjectsUsed						
			}

		} else {

			if(argClass=="numeric" && argLength==1) { #THEN IT NEEDS TO BE REPRESENTED BY ITS REAL VALUE AND NOT BY THE SIZE OF THE OBJECT
				argValue <-  argAnalysis[["value"]]
				
				totalNumericValue <- addToNumericValue(totalNumericValue, argValue)
				sizeOfFreshEntry <- sizeOfFreshEntry + argAnalysis[["objectSize"]] 
		
			}else if (argNROW > 1 || argNCOL > 1 || argClass=="TEMP_SIZE_LIST") {
				sizeOfFreshEntry <- sizeOfFreshEntry + argAnalysis[["objectSize"]] 
				isNumericBased <- FALSE
			
			} else if(argClass!="character"){
				isNumericBased <- FALSE
				sizeOfFreshEntry <- sizeOfFreshEntry + argAnalysis[["objectSize"]] 
			}
		}
	}

	return(list("isNumericBased"=isNumericBased, "sizeOfFreshEntry"=sizeOfFreshEntry, "totalNumericValue"=totalNumericValue))

}


predictTime <- function(sizeOfFreshEntry, totalNumericValue, isNumericBased ,DF, degree, printValues){

	mod <-lm(ExecTime ~ polym(MemorySize, degree=degree, raw = TRUE), data=DF)
	if(isNumericBased) {
		mod <-lm(ExecTime ~ polym(NumericValue, degree=degree, raw = TRUE), data=DF)
		predictValue <- predict(mod, data.frame(NumericValue=totalNumericValue)) #prediction comes in miliseconds
	} else {
		mod <-lm(ExecTime ~ polym(MemorySize, degree=degree, raw = TRUE), data=DF)
		predictValue <- predict(mod, data.frame(MemorySize=sizeOfFreshEntry)) #prediction comes in miliseconds	
	}
	if(predictValue<0) {predictValue<-0}
	if(printValues) {
		cat("\n",summary(mod)$sigma,"\n",sep="") #error
		cat("",predictValue[[1]]/1000,"\n",sep="") #estimated value
	}


	returnList <- list("EstimatedValue"=predictValue[[1]], "AverageModelError"=summary(mod)$sigma, "Multiple_R_Squared"=summary(mod)$r.squared)
	return(returnList)
}


predictSize <- function(sizeOfFreshEntry, totalNumericValue, isNumericBased ,DF, degree, printValues){

	if(isNumericBased) {
		mod <-lm(ReturnObjSize ~ polym(NumericValue, degree=degree, raw = TRUE), data=DF)
		predictValue <- predict(mod, data.frame(NumericValue=totalNumericValue))
	} else {
		mod <-lm(ReturnObjSize ~ polym(MemorySize, degree=degree, raw = TRUE), data=DF)
		predictValue <- predict(mod, data.frame(MemorySize=sizeOfFreshEntry))
	}

	if(predictValue<0) {predictValue<-0}
	if(printValues) {
		cat("\n",summary(mod)$sigma,"\n",sep="") #error
		cat("",predictValue[[1]]/1000,"\n",sep="") #estimated value in Mb
	}

	returnList <- list("EstimatedValue"=predictValue[[1]], "AverageModelError"=summary(mod)$sigma, "Multiple_R_Squared"=summary(mod)$r.squared)

	return(returnList)
}

#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#

addToNumericValue <- function(numericValue, newValue) {
	if (is.na(numericValue)) {
		return(newValue)
	} else {
		return(numericValue + newValue)
	}
}
