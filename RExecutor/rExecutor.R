suppressWarnings(library("rPython"))
suppressWarnings(library(parallel))
suppressWarnings(library(knitr))
library("bannerCommenter")
source("client.R")
source("realArgumentsMiner.R")


source("../CodeAnalysisRW/callTree_Analysys.r", chdir = TRUE)
source("../CodeAnalysisRW/predictorFunct.R", chdir = TRUE)

boldcode <- system('bold=`tput bold`; echo "${bold}"', intern=TRUE)
normalcode <- system('normal=`tput sgr0`;echo "${normal}"', intern=TRUE)
underlinedcode <- system('underlined=`tput smul`; echo "${underlined}"', intern=TRUE)
bluecode <- system('blue=`tput setaf 4`; echo "${blue}"', intern=TRUE)
whitecode <- system('white=`tput setaf 7`; echo "${white}"', intern=TRUE)
yellowcode <- system('yellow=`tput setaf 3`; echo "${yellow}"', intern=TRUE)
redcode <- system('red=`tput setaf 1`; echo "${red}"', intern=TRUE)
cyancode <- system('cyan=`tput setaf 6`; echo "${cyan}"', intern=TRUE)


executeList <- c("require", "library")
OFFLOAD_THRESHOLD <- 60 #seconds
safeList <- c("cat", "remove", "gc", "flush.console")


##-----------------------------------------------------------------------------
## -> THIS FUNCTION IS THE ONE THAT INITIATES ALL THE PREDECTION AND DECISION
## PROCESS AND IS CALLED BY THE USER HIMSELF (LIKE HE WOULD FOR A REGULAR FUNCTION)
predictorSystemHandler <- function(mainexpression, userEnvironment=parent.frame(), mode="live" ){
   	performingFreshAnalysis <<-TRUE

	userEnvir <-  saveEnvironment(userEnvironment)
	p <- mcparallel(printEnvirObjectsSize(userEnvironment))


	exp <- substitute(mainexpression)
	exp_charlist <- as.character(exp)
	exp_languageList <- as.list(exp)
	if(exp_languageList[[1]]!="{"){
		stop("FIRST ARGUMENT OF THE 'predictorSystemHandler' INPUT MUST BE AN R EXPRESSION")
	}

	isOk <- verifyFunctions(majorElement,exp_charlist)

	isOk <- TRUE # EVENTUALY, THIS NEEDS TO GO OUT AND USE THE REAL VALUES
	if(isOk) {

		r <- try(
		{

			# CONNECT THE PREDICTOR SYSTEM'S COMPONENT "MARKET INFORMATION" TO REQUEST FOR THE BW AND CREDITS INFO
			q <- mcparallel(
					(function(x) {
						python.load("../../RExecutor/connectionModule.py")
						userInfo <- python.get("userInfo")
						volunteersInfo <- python.get("volunteersInfo")
						usercredits <- userInfo[[1]]
						print(str(volunteersInfo))
						return(volunteersInfo)
					})()
			)

			start.time_realprediction <- Sys.time()

			prediction <- handleExpression_main(exp, mode=mode)
			#prediction <- handleExpression_main(exp, mode="debug")

			end.time_realprediction <- Sys.time()
			time.taken_realprediction <- end.time_realprediction - start.time_realprediction

			cat("\n-----------------------------------------------------------------------------------------------------------------------------\n")
			cat("-------------------------------------------------------------------------------------------------------------------------------\n")

		}
	    ,silent = TRUE)


		if(class(r)=="try-error"){
			cat(boldcode,"\n\t<<<<<<<<<<<<<<<<<<<<<< EXECUTE CODE LOCALLY. THERE WAS AN ERROR >>>>>>>>>>>>>>>>>>>>>>>\n",normalcode)
			cat(redcode,"\n",r[1],"\n",normalcode)
			decision = FALSE
			error <- gsub(" |[\r\n]", "", r[1])
			errorCause <- strsplit( error, ":")
			errorCause <- errorCause[[1]][2]

			#print(errorCause)
			if(errorCause=="NO_HISTORY"){
				#-------------------------------------------------------------------------------------------------------------------#
				#saveExecutionForIncubator(exp, userEnvir)
				#-------------------------------------------------------------------------------------------------------------------#
			}

		} else {

			timeprediction <- prediction$timeprediction
			sizeprediction <- prediction$sizeprediction
			res <- mccollect(list(p, q))

		 	cat(boldcode,"\n-----------------------------------------------------------------------------------------------------------",normalcode)
		 	cat(boldcode, "\nTIME PREDICTION VALUE:", timeprediction, "seconds", normalcode,"\n\n\n")
			cat(boldcode, "\nSIZE PREDICTION VALUE:", sizeprediction, "", normalcode, "\n\n\n")

			totalEnvirSize <- as.numeric(res[[1]])/1000000 # convert form bytes to Mb
			volunteersInfo <- res[[2]]

			#-----------------------------------------------------------------------------------------------------------------------------------
			# HERE IS WHERE THE ACTUAL DECISION IS MADE

			if (timeprediction > OFFLOAD_THRESHOLD) {
				decision <- FALSE
			} else {

				chosen_one <- "local"
				time_chosen_one <- timeprediction

				for (volunteer in volunteersInfo) {
					if (volunteer[["availableBW"]]!="unable to connect to server: Connection refused"){
						remote_time <- (totalEnvirSize / volunteer[["availableBW"]]) + (timeprediction * volunteer[["score"]]) + (sizeprediction / volunteer[["availableBW"]])
						if (remote_time < time_chosen_one)
						{
							chosen_one <- volunteer
							time_chosen_one <- remote_time
						}

					}
				}

			}

			print(chosen_one)
			print(time_chosen_one)


			# the decision right now is always false, so that it always execute locally, since the remote execution part is not integrated
			decision <- FALSE


			#-------------------------------------------------------------------------------------------------------------------#
			#saveExecutionForIncubator(exp, userEnvir)
			#-------------------------------------------------------------------------------------------------------------------#

		}


	} else {
		cat(boldcode,"\n\t<<<<<<<<<<<<<<<<<<<<<< EXECUTE CODE LOCALLY. THERE IS AN UNKNOWN FUNCTION >>>>>>>>>>>>>>>>>>>>>>>\n",normalcode)
		decision <- FALSE

		#-------------------------------------------------------------------------------------------------------------------#
		#saveExecutionForIncubator(exp, userEnvir)
		#-------------------------------------------------------------------------------------------------------------------#

	}

	performingFreshAnalysis <<-NULL
	if(decision) {

		cat(paste("\n Execute Remotely \n"))

		start.time <- Sys.time()

		#m <- paste("returnValue <-", functCommand)
		#f(c(paste("returnValue <-", functCommand)),functCommand) # <------- Código do Francisco Banha

		returnValue <- eval(parse(text = mainexpression))

		end.time <- Sys.time()
		time.taken <- end.time - start.time
		cat("\ntime taken: ", time.taken, "\n")

		python.assign("execTime", time.taken)
		python.load("saveModule.py")


		return(returnValue)

	} else {


		cat(paste("\n Execute Locally \n"))
		#--------------------------------------------------------------------------------------------------------------
		# HERE THE MAIN EXPRESSION SHOULD BE EXECUTED LIKE IT NORMALLY WOULD, SINCE THE DECISION WAS TO EXECUTE LOCALLY
		#returnValue <- eval(mainexpression, envir = environment)
		#return(returnValue)

	}



   #return summary(returnValue)
   #print(summary(returnValue))
}



##-----------------------------------------------------------------------------
## -> IT STARTS THE RECURSION, MEANING THAT IT CAN ONLY BE CALLED FROM THE OUTSIDE,
## i.e, FROM THE predictorSystemHandler
##
## -> RETURNS A PREDICTION TIME VALUE FOR THE TOTAL CODE BEING ANALYSED
handleExpression_main <- function(exp, mode="live"){
	if(mode=="live"){ debugMode <<- FALSE}
	else {debugMode <<- TRUE}

	innerPredictionsValue <<- 0
	temporarySizeList <<- list()
	assign("temporarySizeList", temporarySizeList, envir=.GlobalEnv)

	exp_charlist <- as.character(exp)
	exp_languageList <- as.list(exp)


	#-------------------------------------------------------------------------
	# PUT UP A BANNER INDICATING THAT THE PREDICTOR ANALYSIS IS GOING TO START
	print(banner("predictor", emph = TRUE))

	#-------------------------------------------------------------------------
	# CREATE A NEW ENVIRONMENT SO WE CAN KNOW WHICH VARIABLES WERE ADDED TO 
	#THE GLOBAL ENVIRONMENT
	my.env <- new.env()

	expressionPrediction <- 0

	#-------------------------------------------------------------------------
	# FOR EACH OF THE ELEMENTS IN THE TOP OF THE EXECUTION TREE, INITATE A NEW
	#PREDICTION PROCESS. (THIS IS NOT DONE IN PARALLEL BECAUSE WE MAY NEED THE
	#RESULT FROM A PREVIOUS PREDICTION TO BE ABLE TO PREDICT THE NEXT)
	for (majorElement in exp_languageList[-1]) {
		start.time_localoverhead <- Sys.time()

		predictionList <- handleInnerExpression(majorElement, 0, my.env=my.env)

		end.time_localoverhead <- Sys.time()
		time.taken_localoverhead <- end.time_localoverhead - start.time_localoverhead
		predVal <- predictionList[["r_execTime"]]
		sizeVal <- predictionList[["r_returnSize"]]
		expressionPrediction <- expressionPrediction + predVal
		cat(boldcode,cyancode,"\n-------------------", predVal ,"s -----------------------",normalcode)
		cat(cyancode,"p_time:", time.taken_localoverhead[[1]], "s\n",normalcode); print(majorElement)

		#---------------------------------------------------------------------
		# IF THE PREDICTION VALUE IS LESS THAN 0.01 & THE OPERATION IS AN
		#ASSIGNMENT, THEN THE ALGORITHM EXECUTES IT. SUCH EXECUTION MAY
		#PROVIDE VALUABLE INFORMATION FOR THE FOLLOWING PREDICTIONS
		if(predVal < 0.01) {
			newcall <- as.list(majorElement)
			mainFunctName<-as.list(newcall)[[1]]

			if(mainFunctName=="<-"){
				capture.output(
					try({
						eval(majorElement, envir = .GlobalEnv)
					}, silent = TRUE))
				cat(yellowcode,boldcode, "EXECUTE", normalcode)

			}
		}
 	}


 	#-------------------------------------------------------------------------
 	#  CLEAN, FROM THE GLOBAL ENVIRONMENT, ALL THE VARIABLES THAT WERE CREATED
 	# AS AN AID FOR THE ANALYSIS. THIS IS IMPORTANT TO ENSURE THERE ARE NO
 	# TRACES LEFT ON THE USERS'S ENVIRONMENT
 	mcparallel({
 		print(str(temporarySizeList))	
 		x <- 0
 		for(element in temporarySizeList){
 			if(length(element) == 0){
 				element <- 48
 			}
 			x <- x + element
 		}
 		print(x)
 		predictedSizeNewEnv <<- x
 	})

	temporarySizeList <<- NULL
	remove(list = names(my.env), envir = .GlobalEnv)
	gc()
 	print(length(exp_languageList[-1]))
 	return(list("timeprediction"=expressionPrediction, "sizeprediction"=0))

}



handleInnerExpression <- function(majorElement, recursionLevel, previousMain="",my.env =parent.frame()){
	newcall <- as.list(majorElement)
	elements <- as.list(newcall)
	mainFunctName <- elements[[1]]
	args <- elements[-1]

	if(mainFunctName=="for") {
		#------------------------------------------------------------
		# TYPICALLY GETTING THE ELEMENTS FROM A LANGUAGE EXPRESSION RETURNS 3 ELEMENTS.
		# IN THE CASE OF 'for', IT RETURNS 4 ELEMENTS.
		#		1 - for
		#		2 - letf hand side of the for's argument
		#		3 - right hand side of the for's argument
		#		4 - body of the for
		# IN THIS SITUATION ONLY THE THIRD ELEMENT EXECUTED AND ITS LENGTH IS MEASURED
		# & THE FOURTH ELEMENT IS GIVEN TO ANOTHER 'soStuff' FUNCTION, AS IF IT WERE A NEW PIECE OF CODE

		iteratorVector <- args[[2]]
		iteratorObj <- try(eval(iteratorVector),silent=TRUE)
		if(class(iteratorObj)[[1]] == "try-error"){ 
			iteratorObj<-"cannot find iterator obj"
		}

		forbody <- args[[3]]

		forBodyCall <- as.list(forbody)
		elementsBodyCall <- as.list(forBodyCall)
		new_mainFunctName <- elementsBodyCall[[1]]

		if(new_mainFunctName=="{") {
			execTime_body <- handleExpression(forbody, recursionLevel+1, my.env=my.env)
		} else {
			value <- handleInnerExpression(forbody, recursionLevel+1, my.env=my.env)
			execTime_body <- value[["r_execTime"]]
		}

		execTime_body <- execTime_body * length(iteratorObj)
		value <- list("r_execTime"=execTime_body)

	} else if(mainFunctName=="if") {

		ifbody <- args[[2]]
		ifBodyCall <- as.list(ifbody)
		elementsBodyCall <- as.list(ifBodyCall)
		new_mainFunctName <- elementsBodyCall[[1]]

		if(new_mainFunctName=="{") {
			execTime_body <- handleExpression(ifbody, recursionLevel+1, my.env=my.env)
		} else {
			value <- handleInnerExpression(ifbody, recursionLevel+1,my.env=my.env)
			execTime_body <- value[["r_execTime"]]
		}

		execTime_body <- execTime_body / 2
		value <- list("r_execTime"=execTime_body)

	} else if(mainFunctName=="{") {

		value <- handleExpression(majorElement, recursionLevel+1,my.env=my.env)

	} else if(mainFunctName=="[" || mainFunctName=="[[" || mainFunctName=="$") {


		codeForMining <- args[[1]]
		value <- handleInnerExpression(codeForMining, recursionLevel+1,my.env=my.env)

	} else if(mainFunctName=="<-" || mainFunctName=="<<-") {
		#-----------------------------------------------------------------------------------------------------------------------------------
		# WHICH HAPPENS ALWAYS THAT THERE IS AN ASSIGNMENT OPERATION. IN THIS SITUATION ONLY THE SECOND ELEMENT OF THE ASSIGNMENT IS MINED
		argToAssign <- deparse(args[[1]])
		codeForMining <- args[[2]]

		if(class(codeForMining)=="call"){

			value <- handleInnerExpression(codeForMining, recursionLevel+1, previousMain="<-", my.env=my.env)
			time <- value[["r_execTime"]]
			size <- value[["r_returnSize"]]

			temporarySizeList[[paste(argToAssign)]]<<-size*1000 # multiplica por 1000 para acertar as unidades

		} else if(class(codeForMining)=="name"){

			if(!exists(paste(codeForMining),temporarySizeList)) {
				eval(majorElement, envir = .GlobalEnv)
				size <- object.size(get(paste(codeForMining)))
				value <- list("r_execTime"=0, "r_returnSize"=as.numeric(size))

			} else {
				size <- temporarySizeList[[paste(codeForMining)]]
				temporarySizeList[[paste(argToAssign)]]<<-size

				value <- list("r_execTime"=0, "r_returnSize"=size)
			}

		} else {
			#------------------------------------------------------------------------------------------------------------------------------
			# IF IT REACHES THIS STAGE IT MEANS THAT THE VARIABLE ASSIGNMENT IS DONE USING A RAW OBJECT, AND IS NOT ONLY SAFE TO SAY THAT IT
			#IS GOING TO BE A QUICK OPERATION, BUT ALSO SAFE TO ASSUME THE VALUE IS GOING TO BE 100% CORRECT
			eval(majorElement, envir = .GlobalEnv)
			eval(majorElement,envir = my.env)

			size <- as.numeric(object.size(get("codeForMining")))

			temporarySizeList[[paste(argToAssign)]]<<-size#*1000 # multiplica por 1000 para acertar as unidades

			value <- list("r_execTime"=0, "r_returnSize"=size)
		}


		print(value[["r_returnSize"]])

	} else if(mainFunctName=="function" && previousMain=="<-") {

		eval(majorElement, envir = .GlobalEnv)
		value <- list("r_execTime"=0)

	} else if (is.element(paste(mainFunctName),executeList)) {

		start.time <- Sys.time()

		realArgValue <- try(eval(majorElement,envir=.GlobalEnv), silent=TRUE)

		end.time <- Sys.time()
		time.taken <- end.time - start.time
		time.taken <- time.taken[[1]]

		size <- as.numeric(object.size(realArgValue))

		if(class(realArgValue)[[1]] == "try-error"){
			stop(paste("ERROR ", mainFunctName,sep = "" ) )
		}

		innerPredictionsValue <<- innerPredictionsValue + time.taken
		value <- list("r_execTime"=time.taken, "r_returnSize"=as.numeric(size))

	} else if (is.element(paste(mainFunctName),safeList)) {

		value <- list("r_execTime"=0, "r_returnSize"=0)

	} else {


			#---------------------------------------------------------------------
			# VERIFY IF THE FUNCTION CALL HAS ANY GROUP OF EXPRESSIONS AS ARGUMENT (like "system.time()")
			done<-FALSE; r<-""
			for (arg in args) {
				local_newcall <- as.list(arg)
				local_elements <- as.list(local_newcall)
				local_mainFunctName <- local_elements[[1]]

				if(local_mainFunctName=="{"){
					value <-  handleExpression(arg, recursionLevel+1,my.env=my.env)
					value <- list("r_execTime"=value, "r_returnSize"=0)
					return (value)
					done<-TRUE
				}
			}

			if(!done) {

				#-------------------------------------------------------------------------------
				# PROCEED WITH THE NORMAL PREDICTION PROCESS
				fNamespace <- findNamespaceLocal(mainFunctName)
				for(namespace in rev(fNamespace)) {
					if(namespace!=".GlobalEnv") {
						fNamespace <- strsplit( namespace[[1]], ":")
						fNamespace <- fNamespace[[1]][2]
					}
					innerPredictionsValue <<- 0

					#capture.output(r <- predictor(majorElement, "TIME_&_SIZE", fNamespace, functionName=mainFunctName))
					r <- predictor(majorElement, "TIME_&_SIZE", fNamespace, functionName=mainFunctName)

					if(r!="NO_HISTORY"){ # IF THE PREDICTION OCCURRED WITHOUT ANY ERROR

						r_execTime <- r[["r_execTime"]]
						r_returnSize <- r[["r_returnSize"]]

						value<-list("r_execTime"=innerPredictionsValue + r_execTime$EstimatedValue, "r_returnSize"=r_returnSize$EstimatedValue)
						innerPredictionsValue <<- 0
						break
					}
				}

				if (r=="NO_HISTORY"){

					cat(boldcode, "\n\nDEU CABUM NO handleInnerExpression. CAN'T FIND HISTORY\n\n", normalcode)
					stop(paste("NO_HISTORY: ", mainFunctName,sep = "" ) )
				}
			}
	}

	return (value)

}


##----------------------------------------------------------------
## -> SIMILAR TO THE "handleExpressionMain" PROCESS, EXCEPT HERE
## THE OPERATION IS CALLED FROM THE INNER PART OF THE RECURSION.
##
## -> IT SIMULATES A NEW USER INVOCATION, TREATING THE RECEIVED
## EXPRESSION/EXECUTION_TREE AS IF IT WAS THE MAIN ONE.
handleExpression <- function(exp, recursionLevel = 0, my.env =parent.frame()){
	exp_charlist <- as.character(exp)
	exp_languageList <- as.list(exp)
	if(exp_languageList[[1]]!="{"){	stop("FIRST ELEMENT OF THE 'handleExpression' INPUT MUST BE AN EXPRESSION") 	}

	expressionPrediction <- 0

	for (majorElement in exp_languageList[-1]) {

		predictionList <- handleInnerExpression(majorElement, recursionLevel, my.env=my.env)
		predVal <- predictionList[["r_execTime"]]
		expressionPrediction <- expressionPrediction + predVal
 	}
 	return(expressionPrediction)

}



#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

verifyFunctions <- function(majorElement,exp_charlist){
	cat("\n-------------------------------------------------------------------------------------------")
	DF <- data.frame(FunctionName=character(), TreeAspect=character(), Namespace=character(), ComposedTree = character())
	#-----------------------------------------------------------------
	# IF LESS THAN 8 MAJOR ELEMENTS DOES SEQUENTIAL SYNTACTIC ANALYSIS
	if(length(exp_charlist) > 8) {
		x <- foreach(majorElement = exp_charlist) %dopar% {
			if(majorElement!="{" ){
				sintaxTree <- call_tree_mod(parse(text=majorElement))
				outputAST <- capture.output(cat(sintaxTree, "\n"))
				callTreeAnalysis("", DF, outputAST, FALSE)
			}
		}

		for(df in x) {
			if(is.data.frame(df)){
				DF <- rbind(DF, df)
			}
		}

	} else { #IF NOT, ANALYSE EACH OF THE MAJOR ELEMENTS IN PARALEL

		for (majorElement in exp_charlist) {
			if(majorElement!="{" ){
				try(
					{sintaxTree <- call_tree_mod(parse(text=majorElement))
					outputAST <- capture.output(cat(sintaxTree, "\n"))
					updatedDF <- callTreeAnalysis("", DF, outputAST, FALSE)
					DF <- updatedDF}, silent=TRUE)
			}
	 	}
	}

	# DF ORDERED BY FUNCTION NAME
	DF<- DF[rev(order(DF$FunctionName)),]

	print(kable(DF))
	print(NROW(DF))
	#orderedDF <- DF[rev(order(DF$FunctionName)),]

	#----------------------------------------------------------
	# WHEN IN LIVE MODE, VERIFY IF THERE IS ANY FUNCTION IN THE SCRIPT WHICH WE DONT HAVE ANY HISTORY FROM
	f_IDs <- unique(DF$FunctID)
	print(length(f_IDs))

	isGoodToGo <- TRUE
	for(f_ID in f_IDs) {
		ids <- strsplit( f_ID, "_")
		f_name <- ids[[1]][1]
		f_namespace <- ids[[1]][2]

		exists_wrap <- verify_exists(f_name, f_namespace, workingDirectory="..")
		exists <- exists_wrap[["exists"]]
		fileLocation <- exists_wrap[["fileLocation"]]

		if(!exists){

			isGoodToGo <- FALSE
		}
	}

	cat("\n-----------------------------------------------------------------------\n\n\n")
	return(isGoodToGo)
}


saveExecutionForIncubator <- function(exp, userEnvir) {

	#-------------------------------------------------------------------------------------------------------------------#
	#-------------------------------------------------------------------------------------------------------------------#
	# SAVE THE ENVIRONMENT ALONG WITH THE EXPRESSION ("exp"). THIS TUPLE IS THEN USED WHILE OFFLINE FOR POPULATING THE DB

	new_populate_list <- list("mainExpression"=exp, "envir"=userEnvir)
	directoryPath <- "/home/rwagenmaker/Documents/IST/PredictorSystem/CodeAnalysisRW/ProfilerDocs/forPopulate"
	dir.create(directoryPath, showWarnings = FALSE)
	fileLocation <- file.path(directoryPath, file = paste("execution1_new.Rda", sep = ""))
	#print(system.time({

		#mcparallel(
			saveRDS(new_populate_list ,file=paste(fileLocation))
		#)
	#}))
}
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

saveEnvironment <- function(envir=.GlobalEnv){
	environment <- list()
	for (var in ls(envir=envir,all.names = TRUE)) {
		#print(var)
		environment[[paste(var)]] <- get(paste(var))
	}

	#print(str(environment))

	e <- as.environment(environment)
	parent.env(e)<-parent.frame()
	return(e)
}

printPurpose <- function(purpose,majorElement){
	cat(yellowcode,boldcode,"\n<---",purpose,"--->\n", normalcode)	
	print(majorElement)
	cat(yellowcode,boldcode,"\n--\n\n", normalcode)	
}

findNamespaceLocal <- function(mainFunctName) {
	fNamespace <- find(paste(mainFunctName))
	#fNamespace <- strsplit( fNamespace[[1]], ":")
	#return(fNamespace[[1]][2])
	return(fNamespace)
}

printEnvirObjectsSize <- function(envir=parent.frame()){
	# USE : 'environment()' TO GET THE CURRENT ENVIRONMENT

	totalEnvirSize <- 0

	for (obj in ls(envir)) {
	    objSize<- object.size(get(obj))
	    totalEnvirSize <- totalEnvirSize + objSize
	    #  UNCOMMENT NEXT LINE TO PRINT THE SIZES OF EVERY OBJECT IN THE ENVIRONMENT
	    #  message(obj, " = ", appendLF = F); print(objSize, units='auto')

    }
    message("\n\ntotal workspace is ",appendLF = F); print(totalEnvirSize, units='auto')

	return(totalEnvirSize)
}
