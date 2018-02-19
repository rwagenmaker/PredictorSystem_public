source("argumentsProfiler.R")

library(ggplot2)



myArgs <- commandArgs(trailingOnly = TRUE)
receivedArgs <- FALSE
if (length(myArgs)==0){
	stop("Must receive at least one argument. 1st: name of package; 2nd: name of function ")
	#packageName <- "stats"#"utils"#"base"#"graphics"#"ggplot2"#"Matrix"#

} else if (length(myArgs)==1){
	packageName <- myArgs[1]
	receivedArgs <- 1

} else if (length(myArgs)==2){
	expression <- myArgs[2]
	packageName <- myArgs[1]
	receivedArgs <- 2
} else {
	stop("Max 2 arguments")

}



# ----------------------------------------------------------------------------------------------
# FOR A GIVEN 'expression' INSPECTS EVERY ELEMENT FROM THE CORRESPONDENT EXAMPLE FILE 

populateFunctionFromExamples <- function(originalExpression, packageName) {
	performingPopulateFromIncubator <<- FALSE
	#cat(c("\033[2J","\033[0;0H")) # clears the terminal

	errorList <- c()
	exampleLines_wrap <-getExamplesFromFunction(originalExpression,packageName)
	if(exampleLines_wrap=="NO_EXAMPLE") {return(errorList)}
	exampleLines <- exampleLines_wrap[["exampleLines"]]
	expression <- exampleLines_wrap[["correctAlias"]]

	err <- try(namesFormals <- names(getFormals(expression)),silent=TRUE)
	if(class(err)=="try-error" ){

		expression <- originalExpression
		err <- try(namesFormals <- names(getFormals(expression)),silent=TRUE)
		if(class(err)=="try-error" ){
			print("ERROR_FORMALS")
			errorList <- c(errorList, list(err[[1]],paste("FORMALS    ||  " ,expression,"   ||   ", err[[1]], sep="")))
			return(errorList)
			break
		}
	}

	cat("\n## FORMALS ## : \n")
	print(namesFormals)
	cat("\n -- Analysis --\n\n")

	my.env <- new.env()

	for (element in exampleLines) {

		elements <- decomposeFunctionCall(element)
		mainFunctName <- getMainFunct(elements)

		hasExpressionValue <- try(hasExpression(expression,element),silent=TRUE)
		if(class(hasExpressionValue)=="try-error" ){
			print("ERROR_'hasExpression()'")
			errorList <- c(errorList, list(err[[1]],paste("'hasExpression()'||  " ,expression,"   ||   ", err[[1]], sep="")))
			return(errorList)
			break
		}#

		if(hasExpressionValue && mainFunctName==expression) { 
			cat("\n ============================================== \n >>", boldcode , element, normalcode, "\n")

			err <- try(argumentsMiner(element,packageName=packageName,environment=my.env),silent=TRUE)
			if(class(err)=="try-error" ){
				print("ERROR_MAIN EXP")
				print(err)
				errorList <- c(errorList, list(paste("MAIN EXP   ||  " ,element, "   ||   ", err[[1]],sep="")))
			}

			err<-try(eval(parse(text=element), my.env),silent=TRUE)
			if(class(err)=="try-error" ){
					print("ERROR EVAL_MAIN")
					print(err)
					errorList <- c(errorList, list(paste("EVAL MAIN  ||  " ,element, "   ||   ", err[[1]],sep="")))
			}	
			cat("\n ============================================== \n\n")
		
		# -----------------------------------------------------------------------------
		# IF THE ELEMENT HAS THE GIVEN FUNCT NAME BUT ISN'T THE MAIN FUNCT(as it stands)
		} else if (hasExpression(expression,element) && mainFunctName!=expression) { #should be '==', 
		#it is currently '!=' so that we can specify the cases where the expression appears but isn't the mainFunct
			cat("\n ------------------------------------------------------------------------------------------------------- \n >>" , element, "\n")
			err<-try(eval(parse(text=element), my.env),silent=TRUE)
			if(class(err)=="try-error" ){
					print("ERROR EVAL_INSIDE EXP")
					print(err)
					errorList <- c(errorList, list(paste("EVAL INSIDE||  " ,deparse(element),"   ||   ", err[[1]],sep="")))
			}	

			args <- getArgs(elements) 
			realExpression <- getCommandFromLanguage(args,expression)
			if(realExpression != "falso positivo"){
				cat("\n\n>>>>", boldcode, paste(deparse(realExpression)), normalcode, "\n")
				
				err <- try(argumentsMiner(realExpression, packageName=packageName,lineAsLanguage=realExpression,environment=my.env ),silent=TRUE)
				if(class(err)=="try-error" ){
					cat("\n|||||||| ERROR_INSIDE EXP: ")
					print(err[[1]])
					errorList <- c(errorList, list(paste("INSIDE EXP ||  " ,deparse(realExpression), "   ||   ", err[[1]],sep="")))
				}	
			} 
			cat("\n ------------------------------------------------------------------------------------------------------- \n\n")
		} else {

			err<-try(eval(parse(text=element), my.env),silent=TRUE)
			if(class(err)=="try-error" ){
					print("ERROR EVAL_FINAL")
					print(err)
					errorList <- c(errorList, list(paste("EVALUATE   ||  " ,deparse(element),"   ||   ", err[[1]], sep="")))
			}	

			cat("\n --\n")
		}
	}
	return(errorList)
}

#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#

getExamplesFromFunction <- function (expression,package) {
	x <- try(example(paste(expression), package = package, give.lines=TRUE, character.only=TRUE),silent=TRUE)
	if(class(x)!="try-error" && !is.null(x)) {
		
		aliasesList <- strsplit( x[3], ": ")[[1]][2]
		aliases <- strsplit( aliasesList, " ")[[1]][1]
		print(aliases)
		#fNamespace <- fNamespace[[1]][2]
		print(x)

		x<- as.character(parse(text=paste(x)))
		cat("\n######## Example of function: <<", expression, ">> , from package: <<", package,">> #########\n")


		for(line in x) {
			if (!isComment(line)) {
				cat("<>",line,"\n")
				#print(line)
			} else {
				cat(line,"\n")
			}
		}
		return(list("exampleLines"=x, "correctAlias"=aliases))

	} else {

		return("NO_EXAMPLE")
	} 
}

getCommandFromLanguage <- function(args, expression) {

	if(length(args)>0){
		for (index in 1:length(args[])) {
			arg <- args[[index]]
	
			if(!missing(arg)){
				if(hasExpression(expression,arg) && typeof(arg)=="language") {
					elements <- decomposeLanguageArg(arg)
					mainFunctName <- getMainFunct(elements)

					if(mainFunctName==expression){
						return(arg)
					} else {

						newArgs <- getArgs(elements) 

						result <- getCommandFromLanguage(newArgs ,expression)
						if(result!="falso positivo") {
							return(result)
						}
					}
				}
			}
		}
	}
	return ("falso positivo")
}



#---------------------------------------------------------------------------------------------------------------#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#---------------------------------------------------------------------------------------------------------------#

totalerrorList <- c()
namesError <- c()

if(receivedArgs==2) {
	cat("\n\n===================================== POPULATING FUNCTION: ", expression, " ======================================\n\n")
	eval(parse(text=paste("require(",packageName,")",sep = "")))
	r<-populateFunctionFromExamples(expression,packageName)
	namesError <- c(namesError, expression)
	totalerrorList <- c(totalerrorList, paste("\n\n===================================== ERROR LIST: ", expression, " ======================================\n",sep=""), r)

} else {

	# ----------------------------------------------------------------------------------------------
	# THIS PART LISTS ALL THE ELEMENTS FROM A PACKAGE

	r<-try(totalFList <- lsf.str(gsub(" ","",paste("package:",packageName,sep=""))),silent = TRUE)
	if(class(r)=="try-error"){
		print("could not find package")
		eval(parse(text=paste("require(",packageName,")",sep = "")))
		totalFList <- lsf.str(gsub(" ","",paste("package:",packageName,sep="")))
	}
	flist <- as.vector(unlist(strsplit(totalFList, "\n")))

	print(flist)

	for (expression in flist) {

		typeOfL <- eval(parse(text=paste("typeof(`", expression ,"`)" ,sep="")))

			expression <- gsub("<-","",expression)
			if(expression!="") {
				cat("\n\n\n\n======================================================================================================================",
					"\n===================================== POPULATING FUNCTION: ", expression, " ======================================\n\n")
				r <- populateFunctionFromExamples(expression, packageName)
				if(length(r)!=0) {
					namesError <- c(namesError, expression)
					totalerrorList <- c(totalerrorList, paste("\n\n===================================== ERROR LIST: ", expression, " ======================================\n",sep=""), r)
				}
			}
	}
}

cat("\n\n===================================== PRINT GLOBAL ERROR LIST (", length(totalerrorList)-length(namesError), ") ======================================\n")
cat("\n\n===================================== ", length(namesError), "Functions =================================================\n")
print(sort(namesError))
cat("\n\n===================================================================================================================\n")
previous <- ""

#for(error in totalerrorList){
#	errID<-strsplit(error, " \\|\\|")
#	typeError <- errID[[1]][1]
#	errID <- errID[[1]][2]
#	if(typeError!="INSIDE EXP") {
#		if (is.na(errID)){
#			cat(error,"\n")
#			previous <- error
#		} else if(previous==errID) {
#			cat(error,"\n")
#			previous <- errID
#		} else {
#			cat("\n--\n",error,sep = "")
#			previous <- errID
#		}
#	}
#}








