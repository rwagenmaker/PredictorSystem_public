library(ggplot2)
library(profvis)
library(microbenchmark)

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

getFormals <- function(expression, environment=parent.frame()) {


	returnval <-try(eval(parse(text = paste("formals(args(", expression, "))", sep="")),envir=environment),silent=TRUE)
	if(class(returnval)=="try-error" ){
		returnval <- eval(parse(text = paste("formals(args(`", expression, "`))", sep="")),envir=environment)
	}
	return(returnval)
}

hasExpression <- function(expression,line) {
	hasExpressionTotal <- grepl(expression, line)
	isInternal <- grepl('TRUE', toString(hasExpressionTotal))
}

isComment <- function(line) {
	hasExpressionTotal <- grepl('#', paste(line))
	is_commentary <- grepl('TRUE', toString(hasExpressionTotal))
	return(is_commentary)
}

typeOfLanguage <- function(firstValue, envir=parent.frame()) {
    isclosure <- eval(parse(text=paste("typeof(`", firstValue ,"`)" ,sep="")),envir=envir)
    return(isclosure)
}

getArgs <- function(listOfElements) {
	args <- listOfElements[-1]
	return(args)
}

getMainFunct <-function(listOfElements) {
	mainFunctName <- listOfElements[[1]]
	return(mainFunctName)
}

decomposeFunctionCall <- function(line) { #must receive a call
	newcall <- as.list(parse(text = paste(line)))[[1]]
	elements <- as.list(newcall)
	#print(str(elements))
	return(elements)
}

decomposeLanguageArg <- function(language) { #must receive an expression of type "language"
	newcall <- as.list(language)
	elements <- as.list(newcall)
	return(elements)
}

################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################


getTime_SizePredictions <- function(majorElement, environment=parent.frame()){			
	hasError=FALSE	
	newcall <- as.list(majorElement)
	elements <- as.list(newcall)
	mainFunctName <- elements[[1]]

	fNamespace <- rev(find(paste(mainFunctName)))
	if(fNamespace==".GlobalEnv") { #if is a user created function

			#if(!exists("debugMode") || debugMode==TRUE){cat(cyancode,"\n>> ==================== FINDING INNER PREDICTION FOR: ",mainFunctName,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: ", paste(deparse(fNamespace)) , "\n",normalcode)}
			
			r <- predictor(majorElement, "TIME_&_SIZE", fNamespace, functionName=mainFunctName)	
			
			#if(!exists("debugMode") || debugMode==TRUE){cat(cyancode,"\n<< ==================== FINISH INNER PREDICTION FOR: ",mainFunctName,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: ", paste(deparse(fNamespace)) , "\n",normalcode)}
		
			if(r!="NO_HISTORY"){
				r_execTime <- r[["r_execTime"]]
				r_returnSize <- r[["r_returnSize"]] 
				value<-list("r_execTime"=r_execTime$EstimatedValue, "r_returnSize"=r_returnSize$EstimatedValue*1000, "description"="SUCCESS", "error"=FALSE)
				return(value)
			
			} else {
	
				stop(paste("NO_HISTORY: INNER_PREDICTION :", mainFunctName,sep = "" ) )
			}

	} else {
		for(namespace in fNamespace) {
			fNamespace <- strsplit( namespace[[1]], ":")
			fNamespace <- fNamespace[[1]][2]

			#if(!exists("debugMode") || debugMode==TRUE){cat(cyancode,"\n>> ==================== FINDING INNER PREDICTION FOR: ",mainFunctName,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: ", paste(deparse(fNamespace)) , "\n",normalcode)}

			r <- predictor(majorElement, "TIME_&_SIZE", fNamespace, functionName=mainFunctName)			

			
			#if(!exists("debugMode") || debugMode==TRUE){cat(cyancode,"\n<< ==================== FINISH INNER PREDICTION FOR: ",mainFunctName,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: ", paste(deparse(fNamespace)) , "\n",normalcode)}
			
			if(r!="NO_HISTORY"){
				r_execTime <- r[["r_execTime"]]; r_returnSize <- r[["r_returnSize"]] 
				value<-list("r_execTime"=r_execTime$EstimatedValue, "r_returnSize"=r_returnSize$EstimatedValue*1000, "description"="SUCCESS", "error"=FALSE)
				return(value)
			}

		}
		if (r=="NO_HISTORY"){
			stop(paste("NO_HISTORY: INNER_PREDICTION: ", mainFunctName,sep = "" ) )
		}		

		#-------------------------------------------------------------
		# IF IT REACHES THIS POINT THEN IT MEANS THAT IT COULD NOT FIND THE HISTORY FILE FOR ANY OF THE NAMESPACES
		start.time <- Sys.time()
		
		realArgValue <- try(eval(majorElement,envir=environment), silent=TRUE)
		
		end.time <- Sys.time()
		time.taken <- end.time - start.time
		time.taken <- time.taken[[1]]

		objectSize <- as.numeric(object.size(realArgValue))	

		if(class(realArgValue)[[1]] == "try-error"){ 
			cat(boldcode,"\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< THERE WAS AN ERROR WHILE GETTING TIME & SIZE PREDICTIONS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>2s> \n", realArgValue[[1]], "\n\n\n",normalcode)
			hasError=TRUE
		}

		returnValue <- list("r_execTime"=time.taken, "r_returnSize"=objectSize, "description"="NO_HISTORY", "error"=hasError)
		return(returnValue)

	}	

	return("NO_HISTORY")		
}


getSizeUsedObjects <- function (language, environment=parent.frame(), previousMainFunct="") { #must receive an exprression of type "language"

	elements <- decomposeLanguageArg(language)
	typeOfLanguage <- typeOfLanguage(elements[[1]],environment)
	fullInfoArgsList <- list()
	
	if (typeOfLanguage == "special") {
		args <- getArgs(elements)
	
		if (elements[[1]]=="[[" || elements[[1]]=="[" || elements[[1]]=="$") {

			real <- getRealArgValue(language, environment=environment)
	
			characteristicsList <- characteriseArgument(real, environment=environment)
			fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))

		}else if(elements[[1]]=="{"){
			
			return(list("sizeObjectsUsed"=0, "objectsInsight"="OBJECTS' SIZE IS IRRELEVANT TO THE EXECUTION TIME BECAUSE IS ARGUMENT IS A GROUP OF EXPRESSIONS. EXECUTION TIME WILL DIRECTLY DEPEND ON WHICH EXPRESSIONS ARE INSERTED."))
				
		}else if (elements[[1]]=="~"){	
			for (index in length(args):1) {
				arg <- args[[index]]

				if(!missing(arg)) {

					typeofValue<-try(typeof(arg),silent)
					print(arg)

					if(arg=="."){

						real <- try(environment$currentDataset, silent=TRUE)
						if(class(real) == "try-error"){
							print("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
							real <- getRealArgValue(arg, environment=environment)
						} 
						
						characteristicsList <- characteriseArgument(real, isSymbol=TRUE, symbolName=arg, environment=environment)


					}else if(typeof(arg)=="symbol") {
												
						real <- getRealArgValue(arg, environment=environment)
						
						characteristicsList <- characteriseArgument(real, isSymbol=TRUE, symbolName=arg, environment=environment)
					
					} else if (typeof(arg)=="language" && class(arg)=="call") {
						
						characteristicsList <- characteriseArgument(arg, previousMainFunct=elements[[1]], environment=environment)
					}else {
						characteristicsList <- characteriseArgument(arg, environment=environment)
					}
				
					fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))

				}else {
					print("argument is missing ||'is.na(arg)'||	")
				}
			}

		}else if (exists("performingPopulateFromIncubator") && !isTRUE(performingPopulateFromIncubator)){

			real <- getRealArgValue(language, environment=environment)
	
			characteristicsList <- characteriseArgument(real, environment=environment)
			fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))


		}else { 

			for (index in length(args):1) {
				arg <- args[[index]]

				if(!missing(arg)) {

					typeofValue<-try(typeof(arg),silent)
					print(arg)

					if(arg=="."){

						real <- try(environment$currentDataset, silent=TRUE)
						if(class(real) == "try-error"){
							print("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
							real <- getRealArgValue(arg, environment=environment)
						} 
						
						characteristicsList <- characteriseArgument(real, isSymbol=TRUE, symbolName=arg, environment=environment)


					}else if(typeof(arg)=="symbol") {
												
						real <- getRealArgValue(arg, environment=environment)
						
						characteristicsList <- characteriseArgument(real, isSymbol=TRUE, symbolName=arg, environment=environment)
					
					} else if ( typeof(arg)=="language" && class(arg)=="call") {
						
						#--------------------
						# NOT YET IMPLEMENTED
						characteristicsList <- characteriseArgument(arg, environment=environment)
					}else {
						characteristicsList <- characteriseArgument(arg, environment=environment)
					}
				
					#print(str(characteristicsList))
					fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))

				}else {
					print("argument is missing ||'is.na(arg)'||	")
				}
			}

		}	

	} else if (typeOfLanguage == "closure") {
		
		if(performingFreshAnalysis) {

			#if(!exists("debugMode") || debugMode==TRUE){cat(boldcode,"\n PREDICTING closure language:  ", normalcode );	print(language)}

			predictions <- getTime_SizePredictions(language, environment=environment)

			innerPredictionsValue <<- innerPredictionsValue + predictions[["r_execTime"]]
			returnList <- list("sizeObjectsUsed"=predictions[["r_returnSize"]], "objectsInsight"=predictions)
			#if(!exists("debugMode") || debugMode==TRUE){
			#	cat(boldcode,cyancode,"------------------------------------------> ",normalcode)
			#	cat(boldcode, predictions[["r_execTime"]] ,"s\n\n",normalcode)}
			
			return(returnList)


		} else{
			#if(!exists("debugMode") || debugMode==TRUE){
				cat(boldcode,"\n Executing closure language:  ", normalcode );	print(language)
			#}

			real <- getRealArgValue(language, environment=environment)
			characteristicsList <- characteriseArgument(real, environment=environment)
			fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))
		}

	} else if (typeOfLanguage == "builtin") {
		args <- getArgs(elements)
		
		if ((elements[[1]]=="+" || elements[[1]]=="-")&& previousMainFunct=="~") { #To handle formulas
			for (index in length(args):1) {
				arg <- args[[index]]
				
				if(as.list(arg)[[1]]=="+" || as.list(arg)[[1]]=="-") {
					characteristicsList <- characteriseArgument(arg,  previousMainFunct="~", environment=environment)
				} else {
					real <- getRealArgValue(arg, environment=environment)
						
					characteristicsList <- characteriseArgument(real, isSymbol=TRUE, symbolName=arg, environment=environment)
				}

				fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))
			}

		} else {	
			

			#-------------------------------------------------------------------------------------------------------
			# DETERMINES WHETHER THE BUILTIN INVOCATION IS SAFE TO PERFORM. BY BEING SAFE IT IS MEANT THAT THE
			# ARGUMENTS INVOLVED IN THE OPERATION ARE MANDATORY TO BE RAW OBJECTS
			executionSafe <- TRUE
			for(arg in args){
				if(typeof(arg)=="language"){
					executionSafe <- FALSE
				}
			}


			if(performingFreshAnalysis && !executionSafe) {
		
					#if(!exists("debugMode") || debugMode==TRUE){cat(boldcode,"\n Predicting builtin language:  ", normalcode );	print(language)}
			
					predictions <- getTime_SizePredictions(language, environment=environment)

					innerPredictionsValue <<- innerPredictionsValue + predictions[["r_execTime"]]
					returnList <- list("sizeObjectsUsed"=predictions[["r_returnSize"]], "objectsInsight"=predictions)
					
					#if(!exists("debugMode") || debugMode==TRUE){
					#	cat(boldcode,cyancode,"------------------------------------------> ",normalcode)
					#	cat(boldcode, predictions[["r_execTime"]] ,"s\n\n",normalcode)}

					return(returnList)
				#	}
			} else {
				#if(!exists("debugMode") || debugMode==TRUE){
					cat(boldcode,"\n Executing bultin language:  ", normalcode );	print(language)
				#}
		
				real <- getRealArgValue(language, environment=environment)
				characteristicsList <- characteriseArgument(real, environment=environment)
				fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))
			}
		}

	} else if (elements[[1]]=="expression"){

		real <- getRealArgValue(deparse(elements[[2]]), environment=environment)
					
		characteristicsList <- characteriseArgument(real, environment=environment)
		fullInfoArgsList <- append(fullInfoArgsList, list(characteristicsList))
	}


	#if(!exists("debugMode") || debugMode==TRUE){print(str(fullInfoArgsList))}

	sizeObjectsUsed <- 0
	for (arg in fullInfoArgsList) {
		if (arg$typeof=="language"){
			sizeObjectsUsed<- sizeObjectsUsed + arg$sizeObjectsUsed
		}else{
			sizeObjectsUsed<- sizeObjectsUsed + arg$objectSize
		}
	}

	return(list("sizeObjectsUsed"=sizeObjectsUsed, "objectsInsight"=fullInfoArgsList))

}


characteriseArgument <- function(argument, isSymbol = FALSE, symbolName = "", previousMainFunct="", environment=parent.frame()) {
	
	class <- class(argument)
	typeof <- typeof(argument)
	length <- length(argument)
	NCOL <- NCOL(argument)
	NROW <- NROW(argument)


	if(class=="lm"){
		objectSize <- as.numeric(object.size(argument$model))	
	} else {
		objectSize <- as.numeric(object.size(argument))	
	}

	if(typeof=="language") {
		
		elements <- decomposeLanguageArg(argument)
		typeOfLanguage <- try(typeOfLanguage(elements[[1]],environment),silent=TRUE)
		if(class(typeOfLanguage)=="try-error" ){
			print("ERROR IN FINDING OBJECTS' SIZE")
			cat(redcode,"\n",typeOfLanguage[1],"\n",normalcode)
			return(list("symbolName"=paste(symbolName), "value"=argument, "class"=class, "typeof"=typeof, "typeOfLanguage"="", "length"=length, "NCOL"=NCOL, "NROW"=NROW,  "objectSize"=objectSize, "sizeObjectsUsed"=0, "objectsInsight"="CANNOT IDENTIFY HEAD OF LANGUAGE EXPRESSION"))
		}	
		result <- getSizeUsedObjects(argument, environment=environment, previousMainFunct=previousMainFunct)
		return(list("symbolName"=paste(symbolName), "value"=argument, "class"=class, "typeof"=typeof, "typeOfLanguage"=typeOfLanguage, "length"=length, "NCOL"= NCOL, "NROW"= NROW,  "objectSize"=objectSize, "sizeObjectsUsed"=result[["sizeObjectsUsed"]] , "objectsInsight"=result[["objectsInsight"]] ))
	} else if(length==1) {
		return(list("symbolName"=paste(symbolName), "value"=argument, "class"=class, "typeof"=typeof, "length"=length, "NCOL"= NCOL, "NROW"= NROW,  "objectSize"=objectSize))
	} else {
		return(list("symbolName"=paste(symbolName), "class"=class, "typeof"=typeof, "length"=length, "NCOL"= NCOL, "NROW"= NROW,"objectSize"=objectSize))	
	}
}


getRealArgValue <- function(arg, environment=parent.frame()) {

	if(typeof(arg)=="language"){
		realArgValue <- try(eval(arg,envir=environment), silent=TRUE)
	} else {
		realArgValue <- try(eval(parse(text= paste(arg)),envir=environment), silent=TRUE)
	}

	if(class(realArgValue)[[1]] == "try-error"){ 
		cat("\n>>> Could not find name : '")
		print(arg)
		cat("'\n")
		arg <- "NOTFOUND"
		class(arg) <- "NOTFOUND"
	} else {
		arg <- realArgValue
	}
	return(arg)
}

################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################

isFunction_X <- function(expression) {

	is_function <- try(eval(parse(text = paste("is.function(", expression, ")"))), silent=TRUE ) 
	if (typeof(is_function) == "character"){ # If the try clause above returns an error, then the return value is of type: 'Character'
		res <- try(eval(parse(text = paste("getAnywhere(", expression, ")",sep=""))),silent=TRUE) #if it returns error then it will do a wider search to see if the 		
		is_function <- try(is.function(res[1]),silent=TRUE)

		if(class(is_function)=="try-error" || is_function==FALSE) { #if it still returns error, then assumes que expression is not a function
			return(FALSE)		
		} 

		newobjList <- res$dups

		lengthnewobjlist <- length(which(newobjList==FALSE))
		if (is_function==TRUE && lengthnewobjlist==1){
			assign(paste(res$name), res[1], envir=.GlobalEnv)
			
			return(TRUE)
		} else {
			return(FALSE)
		}
	} 

	return(is_function)
}


isFunction <- function(expression) {
	is_function <- try(eval(parse(text = paste("is.function(`", expression, "`)",sep=""))), silent=TRUE ) 
	if(class(is_function)=="try-error") { #if it still returns error, then assumes que expression is not a function

			return(FALSE)		
	} 
	
	return(is_function)
}

isMeaningful <- function(expression) {
	type <- try(eval(parse(text = paste("typeof(`", expression, "`)",sep=""))), silent=TRUE ) 
	if(class(type)=="try-error") { #if it still returns error, then assumes que expression is not a function
			cat(redcode,"\n",is_function[1],"\n",normalcode)
			return(FALSE)		
	} 
	
	if(type=="special") {
		return(FALSE)
	} else {
		return(TRUE)
	}

}