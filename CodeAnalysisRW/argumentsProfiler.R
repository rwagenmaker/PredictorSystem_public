
source("mutualMinerFunctions.R")
source("draw-tree.R")





argumentsMiner <- function(line, elements="", mainFunctName="", args="", packageName="", fromExample = FALSE, lineAsLanguage="", environment=parent.frame()) {

	performingFreshAnalysis <<-FALSE
	if (lineAsLanguage!=""){
		elements <- decomposeLanguageArg(line)

		args <- getArgs(elements)
		mainFunctName <- getMainFunct(elements)
		line <- paste(deparse(line))
	}
	else if (!fromExample) {
		elements <- decomposeFunctionCall(line)

		args <- getArgs(elements)
		mainFunctName <- getMainFunct(elements)
	}

	namesInputed <- names(args)
	namesFormals <- names(getFormals(mainFunctName, environment=environment))
	if(!exists("debugMode") || debugMode==TRUE){print(namesFormals)}

	#---
	#Load the Dataset given, if given

	if(length(args$data)>0){
		dataset <- args[["data"]]
		hasDatasetArg <- TRUE
		cat("\nDataset:", dataset,"\n")

		eval(parse(text=paste("currentDataset <-", dataset)), envir=environment)

		eval(parse(text= paste("attach(", dataset, ",warn.conflicts = FALSE)")),envir=environment)


	}else { hasDatasetArg <- FALSE }


	#---

	packageNameDir <- file.path(packageName)
	if(packageName!="") {
		directoryPath <- paste("ProfilerDocs/argsProfilerData/",packageName, sep="")
	} else {
		directoryPath <- "ProfilerDocs/argsProfilerData"
	}


	fileLocation <- file.path(directoryPath, file = paste(mainFunctName,"_Total.Rda", sep = ""))
	fileLocationFresh <- file.path(directoryPath,file = paste(mainFunctName,"_Fresh.Rda", sep = ""))

	# ----------------------------------------------------------------------------------------------------------
	# FIRST DOES THE VERIFICATION/LOAD PROCESS FOR THE FILE WITH THE TOTAL DATA (WITH EVERY ENTRY EVER RECORDED)
	if(file.exists(fileLocation)) {
		argsHistoryData <- readRDS(fileLocation)
	} else {
		dir.create(directoryPath, showWarnings = FALSE)
		argsHistoryData <- list("MajorArguments" = c())
	}

	# -------------------------------------------------------------------------------------------------------------------
	# DOES THE VERIFICATION/LOAD PROCESS FOR THE FILE WITH THE ENTRIES STILL NOT INCLUDED IN THE DATA FRAME FOR THE MODEL
	if(file.exists(fileLocationFresh)) {
		entriesOnStandby <- readRDS(fileLocationFresh)
	} else {
		dir.create(directoryPath, showWarnings = FALSE)
		entriesOnStandby <- list("MajorArguments" = c())
	}


	fullInfoArgsList <- list()


	# ----------------------------------------------------------------------
	# FIRST HANDLE ALL THE ARGS WHICH WERE PASSED WITH THE CORRESPODENT NAME
	for (argName in namesInputed) {


		# ----------------------------------------------------------------------
		# IF THE ARGUMENT HAS A NAME DIRECTLY ATTRIBUTED
		if (argName!="") {

			arg <- args[[paste(argName)]]  # get the arg from the argName
			args[[paste(argName)]]<- NULL  # remove the arg from the total list of inputed args to analyse
			namesFormals <- setdiff(namesFormals, paste(argName))


			if( missing(arg) || is.null(arg) || is.na(arg)) {
				fullInfoArgsList[[paste(argName)]] <- list("MISSING"="ARGUMENT IS MISSING")
			} else {

				# --------------------------------------------------------------------------------------------------------
				#IF THE ARGUMENT PASSED IS A VARIABLE/SYMBOL TO A DATA TYPE IN THE ENVIRONMENT,
				#THEN CHECKS IF THERE IS ANY INDICATION IN THE OTHER ARGUMENTS ABOUT WHERE TO LOOK FOR THE DATA,
				#AND IFprint(characteristicsList) IT IS TRUE, TRIES TO FIND IN THE DATASET, THE DATA REQUESTED
				is_symbol <- eval(typeof(arg)=="symbol",envir=environment)
				if(is_symbol){ 
					symbol <- arg

					if(hasDatasetArg) {
						realArgValue <- try(eval(parse(text= paste(dataset, "$", arg)),envir=environment), silent=TRUE)
						if(class(realArgValue) == "try-error" || is.null(realArgValue)) {
							# --------------------------------------------------------------------------------------------------------
							#IF CANT FIND THE VARIABLE IN THE DATASET, THEN TRIES TO FIND IT IN THE GLOBAL ENVIRONMENT
							arg <- getRealArgValue(arg,environment=environment)
						} else {
							arg <- realArgValue
						}

					} else {
						arg <- getRealArgValue(arg,environment=environment)
					}

				} else if (typeof(arg)=="language"){
					elements <- decomposeLanguageArg(arg)
					typeOfLang <- typeOfLanguage(elements[[1]],envir=environment)
					if(typeOfLang=="closure" || typeOfLang=="builtin" || elements[[1]]=="expression") {
						arg <- getRealArgValue(arg,environment=environment)
					}
				} else {
					cat("\nelse: ", argName, "\n")
				}


				if (class(arg)!="NOTFOUND"){

					if(typeof(arg)=="list" || typeof(arg)=="environment"){
						#cat("\nadicionou ao ambiente a variavel:\n", argName)
						try(attach(arg,warn.conflicts = FALSE),silent=TRUE)
					}


					if(is_symbol){
						characteristicsList <- characteriseArgument(arg, isSymbol=TRUE, symbolName=symbol, environment=environment)
					} else {
						characteristicsList <- characteriseArgument(arg, environment=environment)
					}

					fullInfoArgsList[[paste(argName)]] <- characteristicsList
				} else {

					fullInfoArgsList[[paste(argName)]] <- list("NOTFOUND"="INPUT_WAS_NOT_FOUND_IN-ENVIRONMENT")
				}
			}
		}
	}








	listArgsWithoutName <- c()


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
			is_symbol <- eval(typeof(arg)=="symbol",envir=environment)
			if(is_symbol){
				symbol <- arg
				if(hasDatasetArg) {
					realArgValue <- try(eval(parse(text= paste(dataset, "$", arg)),envir=environment), silent=TRUE)
					if(class(realArgValue) == "try-error" || is.null(realArgValue)) {
						# --------------------------------------------------------------------------------------------------------
						#IF CANT FIND THE VARIABLE IN THE DATASET, THEN TRIES TO FIND IT IN THE GLOBAL ENVIRONMENT
						arg <- getRealArgValue(arg,environment=environment)
					} else {
						arg <- realArgValue
					}

				} else {
					#print(arg)
					arg <- getRealArgValue(arg,environment=environment)
				}

			} else if (typeof(arg)=="language"){


				elements <- decomposeLanguageArg(arg)
				#print(elements)
				typeOfLang <- typeOfLanguage(elements[[1]], envir=environment)
				#print(typeOfLang)
				if(typeOfLang=="closure" || typeOfLang=="builtin" ){
					arg <- getRealArgValue(arg,environment=environment)
				#print(arg)
				}

			} else {
			}


			if(typeof(arg)=="list" || typeof(arg)=="environment"){
					try(attach(arg,warn.conflicts = FALSE),silent=TRUE)
			}

			if(is_symbol){
				characteristicsList <- characteriseArgument(arg, isSymbol=TRUE, symbolName=symbol, environment=environment)
			} else {
				characteristicsList <- characteriseArgument(arg, environment=environment)
			}

			# --------------------------------------------------------------------------------------------
			# FIND THE ARGNAME OF THESE ARGUMENTS THAT WERE PASSED WITHOUT A DIRECT ASSIGNMENT
			argName <- namesFormals[1]
			if (is.null(argName)){ #if the function does not have "formals". most likely a builin function which sintax is mandatory
				newVar <- paste("x_",length(fullInfoArgsList)+1, sep="")
				fullInfoArgsList[[paste(newVar)]] <- characteristicsList
			} else {
				if(argName == "...") {
					#namesFormals <- namesFormals[-1]
					newVar <- paste("d_",length(fullInfoArgsList)+1,sep="")
					listArgsWithoutName <- c(listArgsWithoutName,newVar)
					if (class(arg)!="NOTFOUND"){ 
						fullInfoArgsList[[paste(newVar)]] <- characteristicsList
					} else {

						fullInfoArgsList[[paste(newVar)]] <- list("NOTFOUND"="INPUT_WAS_NOT_FOUND_IN_ENVIRONMENT")
					}

				} else {
					namesFormals <- namesFormals[-1]
					listArgsWithoutName <- c(listArgsWithoutName,argName)
					if (class(arg)!="NOTFOUND"){ 
						fullInfoArgsList[[paste(argName)]] <- characteristicsList
					} else {

						fullInfoArgsList[[paste(argName)]] <- list("NOTFOUND"="INPUT_WAS_NOT_FOUND_IN_ENVIRONMENT")
					}
				}
			}
		}
	}


	cat("\n----\n\n")

	print(line)

	p_overhead <- microbenchmark(parse(text = line), times=10, unit="ns")
	parseOverhead<-mean(p_overhead[[2]]) # Average amount of nanoseconds which the "parse" requires to convert the text into execution

	try(line <- stripCommand(line,environment=environment),silent=TRUE)

	print(line)

	m <- microbenchmark(eval(parse(text = line),envir=environment), times=1, unit="ns")
	returnObj <- eval(parse(text = line),envir=environment)

	timeMean <- mean((m[[2]]))-parseOverhead
	timeMean <- round(timeMean/1000000000, digits=6) # in seconds. (from nanoseconds to seconds) | rounded to microseconds

	fullInfoArgsList[["ExecutionTime"]] <- timeMean#time.taken[[1]]
	fullInfoArgsList[["ReturnObjectSize"]] <- as.numeric(object.size(returnObj))

	fullInfoArgsList[["Command"]] <- line


	# --
	lengthHistoryList <- length(argsHistoryData)
	oldListArgsWithoutName <- argsHistoryData[["MajorArguments"]]
	oldListArgsWithoutName <- c(oldListArgsWithoutName, listArgsWithoutName)

	#print(str(argsHistoryData))
	newMajorArgs <- unique(oldListArgsWithoutName)
	if(is.null(newMajorArgs)){
		newMajorArgs <- c("")
	}
	argsHistoryData[["MajorArguments"]] <- newMajorArgs
	argsHistoryData[[paste(lengthHistoryList+1)]] <- fullInfoArgsList


	#print(str(argsHistoryData))
	#entriesOnStandby[["MajorArguments"]] <- unique(oldListArgsWithoutName)
	#entriesOnStandby[[paste(lengthHistoryList+1)]] <- fullInfoArgsList

	cat("\n@argumentsProfiler : Just saved entry at ,",getwd(),fileLocation,"\n\n")


	saveRDS(argsHistoryData ,file=paste(fileLocation))

	performingFreshAnalysis <<-TRUE
	return(returnObj)
}




stripCommand <- function(line,environment=parent.frame()) {
	line <- paste(line, collapse = "")
	line<- gsub(" ", "", line)# no spaces, so it correctly identifies the pattern

	newcall <- as.list(parse(text = line))[[1]]
	elements <- as.list(newcall)
	args <- elements[-1]

	numbersGen <- 1:length(args)

	for(arg in args) {
		if( !missing(arg) && !is.null(arg) && !is.na(arg)) {
			if(class(arg)=="call"){

				newVar <- paste("a_",numbersGen[1],sep="")
				numbersGen <- tail(numbersGen, -1)
				rValues<-eval(parse(text = paste(newVar," <- ",deparse(arg),sep="")),environment)

				line<- gsub(" ", "", line)# no spaces, so it correctly identifies the pattern

				argAsText <- paste(paste(deparse(arg)), collapse = "")

				pattern <- gsub(" ", "", argAsText)
				line <- sub(pattern, get("newVar"), line,fixed=TRUE) # Changes only the 1st pattern match per string
			}
		}
	}
	return(line)

}


