
source("../CodeAnalysisRW/mutualMinerFunctions.R")


freshExecutionArgs <- function(functionCommand, workDir="") {
	performingFreshAnalysis <<-TRUE
	if (typeof(functionCommand)=="language"){
		elements <- decomposeLanguageArg(functionCommand)
		args <- getArgs(elements) 
		mainFunctName <- getMainFunct(elements)
	}
	else  {
		elements <- decomposeFunctionCall(functionCommand)
		args <- getArgs(elements) 
		mainFunctName <- getMainFunct(elements)
	} 

	namesInputed <- names(args)
	namesFormals <- names(getFormals(mainFunctName))
    
    #if(!exists("debugMode") || debugMode==TRUE){print(namesFormals)}

	if(length(args$data)>0){
		dataset <- args[["data"]]
		hasDatasetArg <- TRUE
		#if(!exists("debugMode") || debugMode==TRUE){cat("\nDataset:", dataset,"\n")}

		eval(parse(text=paste("currentDataset <-", dataset)))
		
		eval(parse(text= paste("attach(", dataset, ",warn.conflicts = FALSE)")))
	} else { hasDatasetArg <- FALSE }

	#---

	# WORK DIR MUST FINISH WITH A SLASH. BUT HAS TO BE PASSED THOUGH THE 'workDir' VAR, BECAUSE OTHERWISE IF IT DOESN'T RECEIVE ANYTHING THEN 
	directoryLocation <- paste(workDir, "FreshExecutionProfileData/", sep="")
	fileLocation <- paste(workDir, "FreshExecutionProfileData/freshArgsAnalysis.Rda", sep="")
	
	if(!file.exists(directoryLocation)) {
		dir.create(paste(workDir, "FreshExecutionProfileData", sep=""))
	}

	fullInfoArgsList <- list()

	# ----------------------------------------------------------------------
	# FIRST HANDLE ALL THE ARGS WHICH WERE PASSED WITH THE CORRESPONENT NAME
	for (argName in namesInputed) {

		# ----------------------------------------------------------------------
		# IF THE ARGUMENT HAS A NAME DIRECTLY ATTRIBUTED

		if (argName!="") {
			
			arg <- args[[paste(argName)]]  # get the arg from the argName
			args[[paste(argName)]]<- NULL  # remove the arg from the total list of inputed args to analyse
			namesFormals <- setdiff(namesFormals, paste(argName))

			# --------------------------------------------------------------------------------------------------------
			#IF THE ARGUMENT PASSED IS A VARIABLE/SYMBOL TO A DATA TYPE IN THE ENVIRONMENT,
			#THEN CHECKS IF THERE IS ANY INDICATION IN THE OTHER ARGUMENTS ABOUT WHERE TO LOOK FOR THE DATA,
			#AND IFprint(characteristicsList) IT IS TRUE, TRIES TO FIND IN THE DATASET, THE DATA REQUESTED
			is_symbol <- eval(typeof(arg)=="symbol")
			if(is_symbol){ 
				symbol <- arg
				symbolSize<-temporarySizeList[[paste(symbol)]]
				
				if(typeof(symbolSize)!="NULL"){

					arg <- "VAR_WAS_UPDATED::USING_SIZE_PREDICTION"
					class(arg) <- "NOTFOUND"

				} else if(hasDatasetArg) {
					realArgValue <- try(eval(parse(text= paste(dataset, "$", arg))), silent=TRUE)
					if(class(realArgValue) == "try-error" || is.null(realArgValue)) {
						# --------------------------------------------------------------------------------------------------------
						#IF CANT FIND THE VARIABLE IN THE DATASET, THEN TRIES TO FIND IT IN THE GLOBAL ENVIRONMENT
						arg <- getRealArgValue(arg)
					} else {
						arg <- realArgValue
					}

				} else {
					arg <- getRealArgValue(arg)
				}
			} 

			if (class(arg)!="NOTFOUND"){ 

				if(typeof(arg)=="list" || typeof(arg)=="environment"){
					try(attach(arg,warn.conflicts = FALSE),silent=TRUE)
				}

				if(is_symbol){
					characteristicsList <- characteriseArgument(arg, isSymbol=TRUE, symbolName=symbol)
				} else {
					characteristicsList <- characteriseArgument(arg)
				}
				
				fullInfoArgsList[[paste(argName)]] <- characteristicsList
			} else {
				#if(!exists("debugMode") || debugMode==TRUE){cat(boldcode,"\n <<1>> FRESH ANALYSIS -- PRINTING VAR FROM 'TEMPORARY_SIZE_LIST':  ",normalcode );	print(symbol)}
				symbolSize<-temporarySizeList[[paste(symbol)]]
				#if(!exists("debugMode") || debugMode==TRUE){print(symbolSize)}

				if(typeof(symbolSize)=="NULL"){
				  	fullInfoArgsList[[paste(argName)]] <- list("symbolName"=symbol, "value"="", "class"="NOT_FOUND", "typeof"="", "length"=1, "NCOL"= 1, "NROW"= 1,  "objectSize"=symbolSize, "Description"="FROM_TEMPORARY_SIZE_LIST")
				} else if( !is.na(symbolSize)) {
					fullInfoArgsList[[paste(argName)]] <- list("symbolName"=symbol, "value"="", "class"="TEMP_SIZE_LIST", "typeof"="", "length"=1, "NCOL"= 1, "NROW"= 1,  "objectSize"=symbolSize, "Description"="FROM_TEMPORARY_SIZE_LIST")
				} else {
					fullInfoArgsList[[paste(argName)]] <- list("NOTFOUND"="INPUT_WAS_NOT_FOUND_IN_ENVIRONMENT")
				}
			}
		}
	}


	# -----------------------------------------------------------------------------------------------------------------------------------------------
	# THEN HANDLE THE REST OF THE ARGS. WHICH INCLUDES MATCHING THE ARGUMENT WITH ITS CORRESPONDANT NAME USING THE ORDER FROM WHICH THEY WERE INPUTED

	if (length(args)>0) {

		for(i in 1:length(args)){

			correspondentName <- names(args[1])
			arg <- args[[i]]

			# --------------------------------------------------------------------------------------------------------
			#IF THE ARGUMENT PASSED IS A VARIABLE/SYMBOL TO A DATA TYPE IN THE ENVIRONMENT,
			#THEN CHECKS IF THERE IS ANY INDICATION IN THE OTHER ARGUMENTS ABOUT WHERE TO LOOK FOR THE DATA,
			#AND IF IT IS TRUE, TRIES TO FIND IN THE DATASET, THE DATA REQUESTED
			is_symbol <- eval(typeof(arg)=="symbol")#is.symbol(typeof(arg))#typeof(arg)=="symbol")
			if(is_symbol){ 
				symbol <- arg

				symbolSize<-temporarySizeList[[paste(symbol)]]

				if(typeof(symbolSize)!="NULL"){

					arg <- "VAR_WAS_UPDATED::USING_SIZE_PREDICTION"
					class(arg) <- "NOTFOUND"

				} else if(hasDatasetArg) {
					realArgValue <- try(eval(parse(text= paste(dataset, "$", arg))), silent=TRUE)
					if(class(realArgValue) == "try-error" || is.null(realArgValue)) {
						# --------------------------------------------------------------------------------------------------------
						#IF CANT FIND THE VARIABLE IN THE DATASET, THEN TRIES TO FIND IT IN THE GLOBAL ENVIRONMENT
						arg <- getRealArgValue(arg)
					} else {
						arg <- realArgValue
					}

				} else {
					arg <- getRealArgValue(arg)
				}
			}
			
			if(typeof(arg)=="list" || typeof(arg)=="environment"){
				try(attach(arg,warn.conflicts = FALSE),silent=TRUE)
			}

			if(is_symbol){
				characteristicsList <- characteriseArgument(arg, isSymbol=TRUE, symbolName=symbol)
			} else {
				characteristicsList <- characteriseArgument(arg)
			}

			# --------------------------------------------------------------------------------------------
			# FIND THE ARGNAME OF THESE ARGUMENTS THAT WERE PASSED WITHOUT A DIRECT ASSIGNMENT

			argName <- namesFormals[1]

			if(!is.null(argName) && argName != "...") {
				namesFormals <- namesFormals[-1]

				#----------------------------------------------------------------------------------------------------------------------------------------
				# IF THE ARGUMENT WAS FOUND IN THE ENVIRONMENT AND EVERYTHING IS AS NORMAL THEN SAVES THE MINING RESULT ASSOCIATED WITH THE ARGUMENT NAME
				if (class(arg)!="NOTFOUND"){ 
					fullInfoArgsList[[paste(argName)]] <- characteristicsList
				} else {

					#if(!exists("debugMode") || debugMode==TRUE){cat(boldcode,"\n <<2>> FRESH ANALYSIS -- PRINTING VAR FROM 'TEMPORARY_SIZE_LIST':  ",normalcode );	print(symbol)}
					symbolSize<-temporarySizeList[[paste(symbol)]]
					#if(!exists("debugMode") || debugMode==TRUE){print(symbolSize)}

					if(typeof(symbolSize)=="NULL"){
					  	fullInfoArgsList[[paste(argName)]] <- list("symbolName"=symbol, "value"="", "class"="NOT_FOUND", "typeof"="", "length"=1, "NCOL"= 1, "NROW"= 1,  "objectSize"=symbolSize, "Description"="FROM_TEMPORARY_SIZE_LIST")
					} else if( !is.na(symbolSize)) {
						fullInfoArgsList[[paste(argName)]] <- list("symbolName"=symbol, "value"="", "class"="TEMP_SIZE_LIST", "typeof"="", "length"=1, "NCOL"= 1, "NROW"= 1,  "objectSize"=symbolSize, "Description"="FROM_TEMPORARY_SIZE_LIST")
					} else {
						fullInfoArgsList[[paste(argName)]] <- list("symbolName"=symbol, "NOTFOUND"="INPUT_WAS_NOT_FOUND_IN_ENVIRONMENT")
					}
				}	

			} else {

				newVar <- paste("d_",length(fullInfoArgsList)+1,sep="")
								#----------------------------------------------------------------------------------------------------------------------------------------
				# IF THE ARGUMENT WAS FOUND IN THE ENVIRONMENT AND EVERYTHING IS AS NORMAL THEN SAVES THE MINING RESULT ASSOCIATED WITH THE ARGUMENT NAME
				if (class(arg)!="NOTFOUND"){ 
					fullInfoArgsList[[paste(newVar)]] <- characteristicsList
				} else {

					#if(!exists("debugMode") || debugMode==TRUE){cat(boldcode,"\n <<2>> FRESH ANALYSIS -- PRINTING VAR FROM 'TEMPORARY_SIZE_LIST':  ",normalcode );	print(symbol)}
					symbolSize<-temporarySizeList[[paste(symbol)]]
					#if(!exists("debugMode") || debugMode==TRUE){print(symbolSize)}

					if(typeof(symbolSize)=="NULL"){
					  	fullInfoArgsList[[paste(newVar)]] <- list("symbolName"=symbol, "value"="", "class"="NOT_FOUND", "typeof"="", "length"=1, "NCOL"= 1, "NROW"= 1,  "objectSize"=symbolSize, "Description"="FROM_TEMPORARY_SIZE_LIST")
					} else if( !is.na(symbolSize)) {
						fullInfoArgsList[[paste(newVar)]] <- list("symbolName"=symbol, "value"="", "class"="TEMP_SIZE_LIST", "typeof"="", "length"=1, "NCOL"= 1, "NROW"= 1,  "objectSize"=symbolSize, "Description"="FROM_TEMPORARY_SIZE_LIST")
					} else {
						fullInfoArgsList[[paste(newVar)]] <- list("symbolName"=symbol, "NOTFOUND"="INPUT_WAS_NOT_FOUND_IN_ENVIRONMENT")
					}
				}	

			}

			cat("\n\n")
	
		}
	}

	#if(!exists("debugMode") || debugMode==TRUE){cat("\n----\n\n")}

	fullInfoArgsList[["Command"]] <- functionCommand

	freshArgsAnalysis <- list("FunctionName" = mainFunctName,"Analysis" = fullInfoArgsList)

	saveRDS(freshArgsAnalysis ,file=paste(fileLocation))
	
	return(freshArgsAnalysis)			
}


readFreshExecutionAnalysis <- function(workingDirectory = "") {
	oldenv <- getwd()

	if(workingDirectory!="") {
		setwd(workingDirectory)	
	}
	
	fileLocation <- file.path("FreshExecutionProfileData/freshArgsAnalysis.Rda")

	if(file.exists(fileLocation)) {
		freshArgsAnalysis <- readRDS(fileLocation)
	} else {
		cat("\nCannot find the analysis of the latest command")
	}

	setwd(oldenv)
	return(freshArgsAnalysis)
}



getExamplesFromExpression <- function (expression,package) {
	cat(c("\033[2J","\033[0;0H"))

	x <- example(paste(expression), package = package, give.lines=TRUE, character.only=TRUE)

	x<- as.character(parse(text=paste(x)))
	cat("\n######## Example of function: <<", expression, ">> , from package: <<", package,">> #########\n")


	#cat("\n####################################################\n\n\n")

	for(line in x) {
		if (!isComment(line)) {
			print(line)
		} else {
			cat(line,"\n")
		}
	}

	cat("\n####################################################\n\n\n")

	for(line in x) {

		if (!isComment(line) && hasExpression(expression,line) ) {
			
			cat("\n>", line, "\n--\n")

			elements <- decomposeFunctionCall(line)
			
			args <- getArgs(elements) 

			mainFunctName <- getMainFunct(elements)
			
			if (mainFunctName==expression) {
				argumentsMiner(elements, mainFunctName, args)

			}

			#break ##  to print only the first example that appears
			cat("\n\n\n\n\n--------------------------------------------------------------------------------------------\n")
			cat("\n--------------------------------------------------------------------------------------------\n\n\n\n\n")
		}	
	}

	
	fileLocation <- file.path("ProfilerDocs/argsProfilerData",file = paste(expression,".Rda", sep = ""))

	if(file.exists(fileLocation)) {

		argsHistoryData <- readRDS(fileLocation)
		for (entry in argsHistoryData) {
	    	print(str(entry))
	    	cat("\n--\n")
		}
	}
}