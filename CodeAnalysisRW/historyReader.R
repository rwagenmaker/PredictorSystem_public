################################################################################################
#
#	> Returns a data frame with all the entries available
#
################################################################################################
library(ggplot2)
suppressWarnings(library(plyr))
suppressWarnings(library(knitr))

#-------------------------------------------------------------------------------------------------------
# USE WITH 'cat()'. 'boldcode' BEFORE THE CODE PRETENDED DO BE BOLD AND 'normalcode' TO RETURN TO NORMAL


boldcode <- system('bold=`tput bold`; echo "${bold}"', intern=TRUE)
normalcode <- system('normal=`tput sgr0`;echo "${normal}"', intern=TRUE)
underlinedcode <- system('underlined=`tput smul`; echo "${underlined}"', intern=TRUE)
bluecode <- system('blue=`tput setaf 4`; echo "${blue}"', intern=TRUE)
whitecode <- system('white=`tput setaf 7`; echo "${white}"', intern=TRUE)
yellowcode <- system('yellow=`tput setaf 3`; echo "${yellow}"', intern=TRUE)
redcode <- system('red=`tput setaf 1`; echo "${red}"', intern=TRUE)
cyancode <- system('cyan=`tput setaf 6`; echo "${cyan}"', intern=TRUE)


dataClasses <- c("glm","lm", "ts","numeric", "integer", "data.frame", "character", "list","matrix", "table", "ftable")


#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#


historyReader_majorArgs <- function(expression, packageName="", freshArguments="", printValue=TRUE, workingDirectory = "") {
	oldenv <- getwd()

	isFileCreated_wrap <- verify_exists(expression, packageName, workingDirectory) 
	isFileCreated <- isFileCreated_wrap[["exists"]]
	fileLocation <- isFileCreated_wrap[["fileLocation"]]
	if(!exists("debugMode") || debugMode==TRUE){cat("\n@historyReader : ",getwd(), fileLocation,"\n\n")}

	if(workingDirectory!="") {
		setwd(workingDirectory)	
	}
	#fileLocation <- file.path("ProfilerDocs/argsProfilerData",file = paste(expression,"_Fresh.Rda", sep = ""))
	DF <- data.frame(MemorySize=double(), NumericValue=double(), ExecTime=double(), ReturnObjSize=double(), ArgsUsed=character(),
					 ClassOfUsedArgs=character(), ArgsPassed=character(), Command=character())

	if(isFileCreated) {

		argsHistoryData <- readRDS(fileLocation)

		majorArgs <- argsHistoryData[["MajorArguments"]]
		argsHistoryData[["MajorArguments"]]<-NULL

		if(!exists("debugMode") || debugMode==TRUE){cat("\n Major Args for <",boldcode, expression, normalcode,"> :", majorArgs, "\n\n" )}

		isNumericBased <- TRUE


		# ----------------------------------------------------------------------
		# FOR EVERY ENTRY EVER RECORDED OF THIS SPECIFIC FUNCTION ("expression")
		for (entry in argsHistoryData) {
			namesEntry <- names(entry)

			#-------------------------------------------------------------------------------------------------------
			# IF EXISTS, REMOVE THE ARGUMENT 'data', SO THAT ITS OBJ.SIZE DOES NOT WRONGLY INFLUENCE THE FINAL VALUE
			namesEntryNoData <- namesEntry[ namesEntry != "data" ]
			#----------------------------------------------------------------------------------------------------------------------------
			# JUST SELECT THE NAMES WHICH TO CORRESPOND TO ARGUMENTS THAT WERE INPUTED. WICH MEANS REMOVING "ExecutionTime" and "Command" and "ReturnObjectSize"
			argsPassed <- paste(sort(head(namesEntry, -3)), sep="", collapse=";")
			argsPassedVec <- sort(head(namesEntry, -3))
			argsPassedWithoutData <- paste(sort(head(namesEntryNoData, -3)), sep="", collapse=";")
			argsPassedVecNoData <- sort(head(namesEntryNoData, -3))
			if(printValue) {print(sort(head(namesEntry, -3)))}

			#-----------------------------------------------------
			# iNITILIZE VARIABLES TO BE USED IN THE READ OPERATION
			totalObjectSize <- 0 # in bytes
			totalNumericValue <- NA
			totalNROW <- 0
			totalNCOL <- 0
			entryExecutionTime <- entry[["ExecutionTime"]]
			classOfArgs <- c()
			argsOfMemory <- c()


			#-------------------------------------------------------------------------------------
			# IF THE FUNCTION DOES NOT HAVE A SINGLE 'major arg', THEN USES ALL THE PASSED VARIABLES
			if(length(majorArgs)==1 && majorArgs=="") {
				if("data" %in% majorArgs){
					argsOfValue <- argsPassedVec
				}else {
					argsOfValue <- argsPassedVecNoData
				}
			}else {
				argsOfValue <- majorArgs
			}


			#-------------------------------------------------------------------------------------
			# IF NO ARGS WERE PASSED AS ARGUMENT, THEN WE CAN CONCLUDE THAT THE INPUT SIZE IS ZERO
			if(length(argsOfValue)==0) {
				entryCommand<-toString(parse(text=entry[["Command"]]))
				returnObjectSize<-as.numeric(entry[["ReturnObjectSize"]])/1000#in bytes -> Kb
				DF <- rbind(DF, data.frame(MemorySize=0,
										   NumericValue=0,
										   ExecTime=entryExecutionTime,
										   ReturnObjSize=round(returnObjectSize, digits=2),
										   ArgsUsed="none",
										   ClassOfUsedArgs="none",
										   ArgsPassed="none",
										   Command=entryCommand))

			#-------------------------------------------------------------------------------------
			# OTHERWISE STARTS THE PROCESS OF SUMMING THE SIZE OF THE ARGS
			} else {
				#-----------------------------------------------------------------------------------------------
				#(AS IT STANDS) USES ONLY THE MAJOR ARGS FOR THE CALCULATION OF THE FINAL 'memory size' VARIABLE
				for(arg in argsOfValue){

				
					argAnalysis <- entry[[paste(arg)]]

					if((!is.null(argAnalysis)) && is.null(argAnalysis[["NOTFOUND"]]) ) {
					
						argTypeof <- argAnalysis[["typeof"]]
						argClass <- argAnalysis[["class"]]
						argNROW <- argAnalysis[["NROW"]]
						argNCOL <- argAnalysis[["NCOL"]]
						argLength <- argAnalysis[["length"]]
					
						#----------------------------------------------------------------------------
						# OBJECT CLASSES CURRENTLY BEING USED TO RETRIEVE A FINAL VALUE OF MEMORY USE
						if(argTypeof=="language") {
								#-------------------------------------------------------------------
								# PREPARE THE STRING ABOUT ARGS' CLASSES TO BE ADDED TO THE FINAL DF
								classOfArgs<-c(classOfArgs,paste(argAnalysis[["typeOfLanguage"]] , sep="", collapse=";"))
								#-------------------------------------------------------------------
								# PREPARE THE STRING ABOUT THE ARGS USED TO BE ADDED TO THE FINAL DF
								argsOfMemory<-c(argsOfMemory,arg)


								typeofLang <- argAnalysis[["typeOfLanguage"]][[1]]
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
									totalObjectSize <- totalObjectSize +  argAnalysis[["objectSize"]] 
								} else {
									totalObjectSize <- totalObjectSize + sizeObjectsUsed						
								}

								#totalObjectSize <- totalObjectSize + argAnalysis[["sizeObjectsUsed"]] 
						} else {

								if (argNROW > 1 || argNCOL > 1) {
									if(argClass=="numeric") {
										argClass <- "numeric_vec"
									}else if(argClass=="integer"){
										argClass <- "integer_vec"
									}else if(argClass=="character"){
										argClass <- "character_vec"
									}
									isNumericBased <- FALSE
								
								} else if(argClass=="numeric" && argLength==1) { #THEN IT NEEDS TO BE REPRESENTED BY ITS REAL VALUE AND NOT BY THE SIZE OF THE OBJECT
									argValue <-  argAnalysis[["value"]]
									totalNumericValue <- addToNumericValue(totalNumericValue, argValue)
									totalObjectSize <- totalObjectSize + argAnalysis[["objectSize"]] 
								} else {
									isNumericBased <- FALSE
								}

								if(argClass!="character"){

									#-------------------------------------------------------------------
									# PREPARE THE STRING ABOUT ARGS' CLASSES TO BE ADDED TO THE FINAL DF
									classOfArgs<-c(classOfArgs,paste(argClass, sep="", collapse=";"))
									#-------------------------------------------------------------------
									# PREPARE THE STRING ABOUT THE ARGS USED TO BE ADDED TO THE FINAL DF
									argsOfMemory<-c(argsOfMemory,arg)
									
									
									totalObjectSize <- totalObjectSize + argAnalysis[["objectSize"]] 
								}

						}
					}

				}

				if(totalObjectSize==0){

					#-----------------------------------------------------------------------------------------------
					#(AS IT STANDS) USES ONLY THE MAJOR ARGS FOR THE CALCULATION OF THE FINAL 'memory size' VARIABLE
					for(arg in argsPassedVec){

					
						argAnalysis <- entry[[paste(arg)]]

						if((!is.null(argAnalysis)) && is.null(argAnalysis[["NOTFOUND"]]) ) {
						
							argTypeof <- argAnalysis[["typeof"]]
							argClass <- argAnalysis[["class"]]
							argNROW <- argAnalysis[["NROW"]]
							argNCOL <- argAnalysis[["NCOL"]]
							argLength <- argAnalysis[["length"]]
						
							#----------------------------------------------------------------------------
							# OBJECT CLASSES CURRENTLY BEING USED TO RETRIEVE A FINAL VALUE OF MEMORY USE
							if(argTypeof=="language") {
									#-------------------------------------------------------------------
									# PREPARE THE STRING ABOUT ARGS' CLASSES TO BE ADDED TO THE FINAL DF
									classOfArgs<-c(classOfArgs,paste(argAnalysis[["typeOfLanguage"]] , sep="", collapse=";"))
									#-------------------------------------------------------------------
									# PREPARE THE STRING ABOUT THE ARGS USED TO BE ADDED TO THE FINAL DF
									argsOfMemory<-c(argsOfMemory,arg)


									typeofLang <- argAnalysis[["typeOfLanguage"]][[1]]
									innerObjects <- argAnalysis[["objectsInsight"]]

									
									if(typeofLang=="builtin" && length(innerObjects)==1) {
										innerSingleObject <- innerObjects[[1]]
										
										if(innerSingleObject[["class"]]=="numeric" && innerSingleObject[["length"]]==1) {
											innerValue <-  innerSingleObject[["value"]]
											cat("\n@builtinLang : ",innerValue)
											totalNumericValue<-addToNumericValue(totalNumericValue, innerValue)
										}
									
									}  else {
											isNumericBased <- FALSE
									}

									sizeObjectsUsed <- argAnalysis[["sizeObjectsUsed"]] 
									if (sizeObjectsUsed==0){
										totalObjectSize <- totalObjectSize +  argAnalysis[["objectSize"]] 
									} else {
										totalObjectSize <- totalObjectSize + sizeObjectsUsed						
									}

									totalObjectSize <- totalObjectSize + argAnalysis[["sizeObjectsUsed"]] 
							} else {

									if (argNROW > 1 || argNCOL > 1) {
										if(argClass=="numeric") {
											argClass <- "numeric_vec"
										}else if(argClass=="integer"){
											argClass <- "integer_vec"
										}else if(argClass=="character"){
											argClass <- "character_vec"
										}
										isNumericBased <- FALSE
									
									} else if(argClass=="numeric" && argLength==1) { #THEN IT NEEDS TO BE REPRESENTED BY ITS REAL VALUE AND NOT BY THE SIZE OF THE OBJECT
										argValue <-  argAnalysis[["value"]]
										totalNumericValue <- addToNumericValue(totalNumericValue, argValue)
										totalObjectSize <- totalObjectSize + argAnalysis[["objectSize"]] 
									} else {
										isNumericBased <- FALSE
									}

									#-------------------------------------------------------------------
									# PREPARE THE STRING ABOUT ARGS' CLASSES TO BE ADDED TO THE FINAL DF
									classOfArgs<-c(classOfArgs,paste(argClass, sep="", collapse=";"))
									#-------------------------------------------------------------------
									# PREPARE THE STRING ABOUT THE ARGS USED TO BE ADDED TO THE FINAL DF
									argsOfMemory<-c(argsOfMemory,arg)
									
									
									totalObjectSize <- totalObjectSize + argAnalysis[["objectSize"]]
							}
						}
					}

				}
			}



			if(totalObjectSize!=0) {
				if(printValue) {cat("\n\n THE ENTRIE'S USED_OBJECTS SIZE IS :", round(totalObjectSize/1000, digits=2), "Kb\n\n" )}
				classOfArgsStr <- paste(sort(classOfArgs), collapse = " / " )
				argsOfMemoryStr <-paste(sort(argsOfMemory), collapse = " / " )

				if(printValue) {
			    	print(str(entry))
				}

				entryCommand<-toString(parse(text=entry[["Command"]]))
				returnObjectSize<-as.numeric(entry[["ReturnObjectSize"]])/1024#in bytes -> Kb
				DF <- rbind(DF, data.frame(MemorySize=round(totalObjectSize/1024, digits=2), #in bytes -> Kb
										   NumericValue=totalNumericValue,
										   ExecTime=entryExecutionTime,
										   ReturnObjSize=round(returnObjectSize, digits=2),
										   ArgsUsed=argsOfMemoryStr,
										   ClassOfUsedArgs=classOfArgsStr,
										   ArgsPassed=argsPassed,
										   Command=entryCommand))

	    		#cat("\n--\n")
	    	} else {
	    		if(printValue)	print("SIZE IS ZERO")
	    	}

		    if(printValue) {cat("\n\n--------============------------\n\n")}
		}

		DF <- DF[rev(order(DF$ExecTime)),]

		setwd(oldenv)
		return(list("DF"=DF, "isNumericBased"=isNumericBased))
	} else {
		#cat("\n---\n THERE IS NO EXECUTION HISTORY FOR FUNCTION <", expression, ">\n\n" )
		setwd(oldenv)
		return("NO_HISTORY")
	}
	setwd(oldenv)
}


#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------
# RETURNS THE ARGS NAMES THAT WERE PASSED WITHOUT ANY CORRESPONDENT VALUE ,i.e, THAT REQUIRED MATCHING
getMajorArgs <- function(expression,packageName="",workingDirectory = ""){

	oldenv <- getwd()

	isFileCreated_wrap <- verify_exists(expression, packageName, workingDirectory)
	isFileCreated <- isFileCreated_wrap[["exists"]]
	fileLocation <- isFileCreated_wrap[["fileLocation"]]

	if(workingDirectory!="") {
		setwd(workingDirectory)
	}
	if(isFileCreated) {

		argsHistoryData <- readRDS(fileLocation)
		majorArgs <- argsHistoryData[["MajorArguments"]]
		setwd(oldenv)
		return(majorArgs)
	} else {

		#THERE IS NO HISTORY FILE FOR FUNCTION < expression >
		cat(bluecode,"\nNO_HISTORY - ",expression,"-",packageName,"\n",normalcode)
		setwd(oldenv)
		return("NO_HISTORY")
	}

}


#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#

getAliases <- function(functionName, package){

	if(typeof(get(functionName))=="builtin"){
		return(c(functionName))
	}

	x <- try(example(paste(functionName), package = package, give.lines=TRUE, character.only=TRUE),silent=TRUE)
	if(class(x)!="try-error") {
		aliasesList <- strsplit( x[3], ": ")[[1]][2]
		aliases <- strsplit( aliasesList, " ")[[1]]
		return(aliases)
	} else {
		
		cat(redcode,"\n",x[1],"\n",normalcode)
		return("ERROR")
	}
}


verify_exists <- function(expression, packageName, workingDirectory = ""){
	oldenv <- getwd()

	if(workingDirectory!="") {
		setwd(workingDirectory)
	}

	packageNameDir <- file.path(packageName)
	if(packageName!="") {
		directoryPath <- paste("ProfilerDocs/argsProfilerData/",packageName, sep="")
	} else {
		directoryPath <- "ProfilerDocs/argsProfilerData"
	}


	fileLocation <- file.path(directoryPath, file = paste(expression,"_Total.Rda", sep = ""))

	if(file.exists(fileLocation)) {
		setwd(oldenv)
		return(list("exists"=TRUE, "fileLocation"=fileLocation))
	} else {

		if(packageName!=".GlobalEnv"){
			aliasList <- getAliases(expression,packageName)
			print(aliasList)
			if(!is.na(aliasList) && aliasList[1]!="ERROR" && length(aliasList>1)){

				for(alias in aliasList){
					fileLocation <- file.path(directoryPath, file = paste(alias,"_Total.Rda", sep = ""))
					if(file.exists(fileLocation)) {
						setwd(oldenv)
						return(list("exists"=TRUE, "fileLocation"=fileLocation))
					}
				}
			}

		}
		print(expression)
		cat("\n@verify_exists : ",boldcode,bluecode,expression,normalcode," : ", getwd(), fileLocation,"\n\n")
		#print("FALSE")
		setwd(oldenv)
		return(list("exists"=FALSE, "fileLocation"=""))
	}

}

addToNumericValue <- function(numericValue, newValue) {
	if (is.na(numericValue)) {
		return(newValue)
	} else {
		return(numericValue + newValue)
	}
}

#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#


readPackage <- function(packageName){
	totalFList <- lsf.str(gsub(" ","",paste("package:",packageName,sep="")))
	flist <- as.vector(unlist(strsplit(totalFList, "\n")))

	for(expression in flist) {
		expression <- gsub("<-","",expression)
		historyReader_majorArgs(expression, packageName=packageName, printValue=FALSE)
		cat("\n-----------------------------------------------------------------------------------------------\n")
	}
}


myArgs <- commandArgs(trailingOnly = TRUE)
expression <- myArgs[1]
packageName <- myArgs[2]
generatePlot <- myArgs[3]

if(!is.na(expression)) {

	if(expression=="TOTAL") {

		if (is.na(packageName)) {
			print("Should receive a package name as 2nd argument")
		} else {
			readPackage(packageName)
		}

	} else if(expression=="") {

	} else {
		x <- historyReader_majorArgs(expression,packageName)
		if(!is.na(generatePlot) && generatePlot=="genplot"){


			mypath <- file.path(getwd(),paste("historyReaderPlot_", expression, ".png", sep = ""))
			png(file=mypath)
			    mytitle = paste("my title is", expression)
			    plot(x$ExecTime~x$MemorySize, main = mytitle)
			    abline(lm(x$ExecTime~x$MemorySize))
			dev.off()
		}
	}
}



