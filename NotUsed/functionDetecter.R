 suppressWarnings(library(pryr))
 suppressWarnings(library(knitr))
 
 #library(ggplot2)
 #library(mvbutils)
 source("listFunctions.R")
 source("callTree_Analysys.r")
 #source("callTree_AnalysysSequential.r")
 source("firstSeparation.R")

MAXRECURSIONLEVEL <- 2




assignFunction <- function(functionDefNumbers, totalSrcCode) {
	
	#cat("\n\n---------------- Assign Function ------------------------------------------------------------------------------\n")
	#cat(length(functionDefNumbers),"\n")

	for(index in functionDefNumbers) {
		functionCode <- character()
		openCounter <- 0
		closeCounter <- 0
		innerIndex <- index

		functionName <- head(as.vector(unlist(strsplit(totalSrcCode[index], " <-"))), 1)
		#cat(functionName, ":\t")
		#print(totalSrcCode[index])
		
		while(openCounter != closeCounter || openCounter == 0){

			# print(totalSrcCode[innerIndex])

			numberOfOpen <- length(grep('\\{', totalSrcCode[innerIndex]))
 			numberOfClose <- length(grep('\\}', totalSrcCode[innerIndex]))
 			
 			openCounter <- openCounter + numberOfOpen
 			closeCounter <- closeCounter + numberOfClose

 			if(openCounter == 0){
 				break
 			}

 			functionCode <- c(functionCode,totalSrcCode[innerIndex])


 			innerIndex <- innerIndex + 1
		}

		if (openCounter == 0) { # For the cases which the function is defined without '{' and '}', i.e, in the same line
			#print("Open counter was zero")
			functionCode <- totalSrcCode[index]
		} else {
			functionCode <- paste(functionCode, collapse = '\n')	
		}

		wtv <- try(eval(parse(text = functionCode)), silent=TRUE)	
		if (typeof(wtv) == "character") {
			#cat("Unsucessful\n")
		} else{
			#cat("Sucessful\n")
			assign(functionName, wtv, envir=globalenv())		
		}
		

	}
	#cat("\n---------------- --------------- ------------------------------------------------------------------------------\n")
}

listGenericFunctionMethods <- function(generic.function) {

	f<- getAnywhere(generic.function)[1]
	listF <- list()
	listF[[paste(generic.function)]] <- f
	e <- list2env(listF)
	s3 <- .S3methods(generic.function, envir=e)

	return(s3)
}

functDiscovery <- function(srcCode, f.expression) {

	##############################################################################################################

	functionDefNumbers <- grep(' function\\(', srcCode)

	if (length(functionDefNumbers) != 0){ # If there is any function definition within the regular function's code
			assignFunction(functionDefNumbers, srcCode)
	}


	treeDF <- structureTreeDF(f.expression)#, enablePrint=TRUE)
	#allsubfuncs <- treeDF$FunctionName 
	#if (length(which(allsubfuncs=='inherits'))> 0) {print(f.expression)}

	return(treeDF)
}


# ==========================================================================================================
# ==========================================================================================================
# ==========================================================================================================
# ==========================================================================================================
 


analysisStarter <- function(expression){


	# -------------------------------------------------------------------
	# LOADS THE CURRENT AVAILABLE ANALYSIS DATA TO MEMORY

	if(file.exists("totalAnalysisList.Rda")) {
		global_list <<- readRDS("totalAnalysisList.Rda")
		print(class(global_list))
		print(names(global_list))
		cat("\n")
		assign("global_list", global_list, envir=.GlobalEnv)
	} else {
		global_list <<- list()
		assign("global_list", global_list, envir=.GlobalEnv)
	}

	
	# -------------------------------------------------------------------
	# CHECKS IF THE EXPRESSION RECEIVED IS A FUNCTION OR A SCRIPT

	if(isFunction(expression)){

		###############################################################################################################################
		# ---------------------------------------------------------------------------
		# DOES A FIRST VERIFICATION TO SEE IF IT IS NECESSARY TO PERFORM THE ANALYSIS
		if(isNonComplex(expression)) {
			return(0.0)
		}

		###############################################################################################################################


		cat("\n\n======================================================================================================================",
		"\n===================================== ANALYSING INPUTED FUNCTION: ", expression, " ======================================\n")
		
		# --------------------------------------------------------------------------------------------
		# IF IS A FUNCTION THEN TRIES TO LOAD THE LIBRARY WHERE THE FUNCTION IS FROM

		fString <- paste("environment(", expression, ")")
		
		fEnvironment <- try(eval(parse(text = fString)),silent=TRUE)
		fNamespace <- trimNamespace(fEnvironment)
		try(library(fNamespace,character.only=TRUE), silent=TRUE)
		functionIdentifier <- paste(expression,fNamespace,0,sep = "_")


		# --------------------------------------------------------------------------------------------
		# THEN CONSULTS THE ANALYSIS DATA STORED AND CHECKS IF THE GIVEN FUNCTION WAS ALREADY ANALYSED

		if(exists(paste(functionIdentifier), where = global_list)){
			analysisResult <- global_list[[paste(functionIdentifier)]]
		} else {
			analysisResult <- analyseFunction(expression, fNamespace=fNamespace)
			global_list[[paste(functionIdentifier)]] <<- analysisResult
		}

		#saveRDS(global_list,file="totalAnalysisList.Rda")
		return(analysisResult)

	} else {
		cat("\nNot a function:", expression, "\n")
	}
	# -------------------------------------------------------------------

}


analyseFunction <- function(masterFunct, functionToAnalyse=masterFunct, recursiveLevel = 0, parentFunct = masterFunct, fNamespace="") {

	#cat("\n\n ==================== NEW FUNCTION ANALYSIS: ", functionToAnalyse, "==================================================\n")
	names_global_list <- names(global_list)   
	#cat("\n--------\n number of saved names:", length(names_global_list), "\t recursionLevel:",recursiveLevel,"\t From parent function:",parentFunct,"\n")
	#print(names_global_list)


	primitivenumber <- 0
	internalnumber <- 0
	regularnumber <- 0
	genericnumber <- 0

	is_function <- isFunction(functionToAnalyse)

	if (!is_function) {
		#showinfo <- addRecursionLevel(paste("-- The expression < ", functionToAnalyse, " > is not a function -----------\n"), recursiveLevel)
		return (list("functionType" = "NOFUNCTION", "numLines" = 0, "ifnumber" = 0, "fornumber" = 0, "whilenumber" = 0))
	} 
	

	if (isPrimitive(functionToAnalyse)) {

		#showinfo <- addRecursionLevel(paste("----------- PRIMITIVE --------------------------------------- FUNCTION <<< ", functionToAnalyse, " >>>  ------------------ \n"), recursiveLevel)
		#cat(showinfo)
		
		functionType <- "PRIMITIVE"
		primitivenumber <- 1 + primitivenumber
		returnList <- list("functionType" = "PRIMITIVE", "numLines" = 0, "ifnumber" = 0, "fornumber" = 0, "whilenumber" = 0, "primitivenumber"= 1, "internalnumber" = 0, "regularnumber" = 0)
		return (returnList)

	} else {

		fInformation <- getSourceCode(functionToAnalyse)

		if (fInformation$isInternal) {

			#showinfo <- addRecursionLevel(paste("----------- INTERNAL ---(numLines:", fInformation$numLines, ")----------------------- FUNCTION <<< ", functionToAnalyse, " >>> --------------- \n" ), recursiveLevel)
			#cat(showinfo)
			
			functionType <- "INTERNAL"
			internalnumber <- 1 + internalnumber
			returnList <- list("functionType" = "INTERNAL", "numLines" = 0, "ifnumber" = 0, "fornumber" = 0, "whilenumber" = 0, "primitivenumber"=0, "internalnumber" = 1, "genericnumber"= 0 ,"regularnumber" = 0)
			return (returnList)

		} else if (fInformation$isGeneric) {

			functionType <- "GENERIC"
			genericnumber <- 1 + genericnumber

							#hasDefault <- FALSE
							#showinfo <- addRecursionLevel(paste("----------- GENERIC  ---(numLines:", fInformation$numLines, ")----------------------- FUNCTION <<< ", functionToAnalyse, " >>> --------------- \n" ), recursiveLevel)
							#cat(showinfo)

			generic.function <- paste(functionToAnalyse)
			s3 <- listGenericFunctionMethods(generic.function)

							#print(s3)
			hasDefault <- isTRUE(length(which(s3==paste(generic.function,".default", sep=""))) == 1)
			if(hasDefault) {
				res <- getAnywhere(paste(generic.function,".default", sep=""))
				f_default <- res[1]
				f_default_name <- res$name
				assign(paste(f_default_name), f_default, envir=.GlobalEnv)
				
				result_generic <- analyseFunction(masterFunct, f_default_name, recursiveLevel, paste("GENERIC PARENT: ", functionToAnalyse, sep = ""))
				result_generic[["functionType"]] <- "GENERIC"
				result_generic[["hasDefault"]] <- hasDefault
							#cat("\n?????????????????????? RETURN VALUE FROM :  ", functionToAnalyse, "\n")
							#str(result_generic)
				return(result_generic)
				

							#returnList <- list("functionType" = "GENERIC", "numLines" = 0, "ifnumber" = 0, "fornumber" = 0, "whilenumber" = 0, "primitivenumber"=0, "internalnumber" = 0, "genericnumber"= 1, "regularnumber" = 0, "hasDefault" = hasDefault)
							#return (returnList)

							#cat("\n")
							#print(as.character(body(f_default)))
			} else {
				returnList <- list("functionType" = "GENERIC", "numLines" = 0, "ifnumber" = 0, "fornumber" = 0, "whilenumber" = 0, "primitivenumber"=0, "internalnumber" = 0, "genericnumber"= 1, "regularnumber" = 0, "hasDefault" = hasDefault)
				return (returnList)
			}
			
		} else {

			#showinfo <- addRecursionLevel(paste("=========== REGULAR  ===(numLines:", fInformation$numLines, ")======================= FUNCTION <<< ", functionToAnalyse, " >>> =============== \n"), recursiveLevel)
			#cat(showinfo)

			hasDefault <- TRUE
			functionType <- "REGULAR" 
			regularnumber <- 1 + regularnumber
		}

		numLines <- fInformation$numLines
		ifnumber <- getExpressionNumber(fInformation$fcode, 'if \\(|if\\(')
		fornumber <- getExpressionNumber(fInformation$fcode, 'for \\(')
		whilenumber <- getExpressionNumber(fInformation$fcode, 'while \\(')

		functionNum <- 0
		nonFunctionNum <- 0


		###############################################################################################################################

		realFunctions <- character()
		primitiveFunctions <- character()
		internalFunctions <- character()
		genericFunctions <- character()
		regularFunctions <- character()
		allSubFunctions <- character()
		totalTrees <- c()
		
		#if (TRUE){ 
		if ((masterFunct != functionToAnalyse || recursiveLevel == 0) && recursiveLevel<=MAXRECURSIONLEVEL){ 
		#if (recursiveLevel <= 10) {
			
			treeDF <- functDiscovery(fInformation$fcode, functionToAnalyse)
			allSubFunctions <- treeDF$FunctionName

			

			if(nrow(treeDF)>0){

				for(rowID in 1:nrow(treeDF)){
					

					# --------------------------------------------------------------------------------------------------------
					# THIS WAY OF DOING THE DATA FRAME ACCESS RETURN A "FACTOR" TYPE OBJECT, SO IT MUST BE CONVERTED TO STRING
					f <- as.character(treeDF[rowID,"FunctionName"])

					# --------------------------------------------------------------------------------------------------------
					# IF THE CODE OF FUNCTION f CONTAINS THE SAME FUNCTION f (RECURSIVELY), THEN IT DOES NOT ANALYSE IT
					if(f!=functionToAnalyse){ 
	
						composedTree <- as.character(treeDF[rowID,"ComposedTree"])
						namespace <- as.character(treeDF[rowID,"Namespace"])

						if ((recursiveLevel==1 && fNamespace==namespace) || recursiveLevel!=1) {
							oldFunctionAndLevel <- FALSE
							for(level in 0:MAXRECURSIONLEVEL) {
								functionIdentifier <- paste(f,namespace,level,sep = "_")

								if(exists(paste(functionIdentifier), where = global_list) && level <= recursiveLevel) {
									#cat("\n\n ==================== OLD FUNCTION ANALYSIS: ", f, "==================================================\n")

									result <- global_list[[paste(functionIdentifier)]]

									oldFunctionAndLevel <- TRUE
									break
								} 
							}

							if (!oldFunctionAndLevel) {

								result <- analyseFunction(masterFunct, f, recursiveLevel + 1, functionToAnalyse)
								
								functionIdentifier <- paste(f,namespace,recursiveLevel,sep = "_")
								global_list[[paste(functionIdentifier)]] <<- result	
							}


							#--------------------------------------------------------------------------------------------------------------------
							# AGGREGATES THE ANALYSIS RESULTS FOR EVERY SINGLE ONE OF IT'S CHILDS (AND THEY'RE RESPECTIVE ANALYSIS)

							if(result$functionType == "PRIMITIVE"){
								realFunctions <- c(realFunctions,f)
								primitiveFunctions <- c(primitiveFunctions,f)
								
								functionNum <- 1 + functionNum
								primitivenumber <- result$primitivenumber + primitivenumber
								totalTrees <- c(totalTrees, composedTree)
							
							} else if(result$functionType == "INTERNAL"){
								realFunctions <- c(realFunctions,f)
								internalFunctions <- c(internalFunctions,f)

								functionNum <- 1 + functionNum
								internalnumber <- result$internalnumber + internalnumber
								totalTrees <- c(totalTrees, composedTree)

							} else if(result$functionType == "GENERIC" && result$hasDefault==FALSE){
								realFunctions <- c(realFunctions,f)
								genericFunctions <- c(genericFunctions,f)

								functionNum <- 1 + functionNum
								genericnumber <- result$genericnumber + genericnumber
								totalTrees <- c(totalTrees, composedTree)

							} else if(result$functionType == "GENERIC" && result$hasDefault==TRUE){
								#str(result)
								for(tree in result$totalTrees){
									totalTrees <- c(totalTrees, paste(composedTree, tree, sep = " // "))
								}

								realFunctions <- c(realFunctions,f,result$realFunctions)
								genericFunctions <- c(genericFunctions,f,result$genericFunctions)

								regularFunctions <- c(regularFunctions,result$regularFunctions)
								primitiveFunctions <- c(primitiveFunctions,result$primitiveFunctions)
								internalFunctions <- c(internalFunctions,result$internalFunctions)


								functionNum <- 1 + functionNum + result$functionNum


								nonFunctionNum <- nonFunctionNum + result$nonFunctionNum
								primitivenumber <- primitivenumber + result$primitivenumber
								internalnumber <- internalnumber + result$internalnumber
								genericnumber <- genericnumber + result$genericnumber
								regularnumber <- regularnumber + result$regularnumber
								allSubFunctions <- c(allSubFunctions, result$allSubFunctions)

							} else if(result$functionType == "REGULAR"){

								for(tree in result$totalTrees){
									totalTrees <- c(totalTrees, paste(composedTree, tree, sep = " // "))
								}

								realFunctions <- c(realFunctions,f,result$realFunctions)
								regularFunctions <- c(regularFunctions,f,result$regularFunctions)

								genericFunctions <- c(genericFunctions,result$genericFunctions)
								primitiveFunctions <- c(primitiveFunctions,result$primitiveFunctions)
								internalFunctions <- c(internalFunctions,result$internalFunctions)


								functionNum <- 1 + functionNum + result$functionNum


								nonFunctionNum <- nonFunctionNum + result$nonFunctionNum
								primitivenumber <- primitivenumber + result$primitivenumber
								internalnumber <- internalnumber + result$internalnumber
								genericnumber <- genericnumber + result$genericnumber
								regularnumber <- regularnumber + result$regularnumber
								allSubFunctions <- c(allSubFunctions, result$allSubFunctions)

							} else{
								
								nonFunctionNum <- 1 + nonFunctionNum 
							
							}

							numLines <- result$numLines + numLines
							ifnumber <- result$ifnumber + ifnumber
							fornumber <- result$fornumber + fornumber
							whilenumber <- result$whilenumber + whilenumber
						}
					} else {
						nonFunctionNum <- 1 + nonFunctionNum 	
					}


				}
			}			
		} 
		
		x <- 0
		returnList <- list("functionType" = functionType, "numLines" = numLines,  "ifnumber" = ifnumber, "fornumber" = fornumber, "whilenumber" = whilenumber, "primitivenumber"= primitivenumber, "internalnumber" = internalnumber, "regularnumber" = regularnumber, "genericnumber"=genericnumber, "allSubFunctions" = allSubFunctions, "realFunctions" = realFunctions, "primitiveFunctions" = primitiveFunctions, "internalFunctions" = internalFunctions, "genericFunctions" = genericFunctions, "regularFunctions" = regularFunctions, "functionNum" = functionNum, "nonFunctionNum" = nonFunctionNum, "totalTrees" = totalTrees ,"hasDefault" = hasDefault)	
		
		return (returnList)
	}

}

# ==========================================================================================================
# ==========================================================================================================
# ==============================     VERIFICATION FUNCTIONS + GETS      ====================================
# ==========================================================================================================
# ==========================================================================================================

getSourceCode <- function(f.expression) {
	function.body <- eval(parse(text = paste("body(", f.expression, ")", sep="")))	

	#print(function.body)
	f.code <- as.character(function.body)

	
	isInternal <- getIsInternalValue(f.code)
	isGeneric <- getIsGenericValue(f.code)
	#if (isTRUE(getIsInheritsValue(f.code))) {
	#	print("IS INHERITS")
	#	print(f.code)
	#}

	#cat("\n", f.expression, ":  ", isGeneric,"\n")

	listed.f.code <- as.vector(unlist(strsplit(f.code, "\n")))

	#print(listed.f.code)
	#Sys.sleep(0.05)
	number_of_lines <- length(listed.f.code)


	return (list("fcode" = listed.f.code, "numLines" = number_of_lines, "isInternal" = isInternal, "isGeneric" = isGeneric))
}

getIsInternalValue <- function(f.code) {
	isInternalTotal <- grepl('.Internal', f.code)
	isInternal <- grepl('TRUE', toString(isInternalTotal))
}

getIsDeprecatedValue <- function(f.expression) {
	function.body <- eval(parse(text = paste("body(`", f.expression, "`)",sep="")))	
	f.code <- as.character(function.body)
	isDeprecatedTotal <- grepl('.Deprecated', f.code)
	isDeprecated <- grepl('TRUE', toString(isDeprecatedTotal))
}

getIsInheritsValue <- function(f.code) {
	isInheritsTotal <- grepl('inherits', f.code)
	isInherits <- grepl('TRUE', toString(isInheritsTotal))
	return(isInherits)
}

getIsGenericValue <- function(f.code) {
	#cat("\nenter getIsGeneric ----------- \n")
	#print(f.code)
	isGenericTotal <- grepl('UseMethod|standardGeneric', f.code)
	#print(isGenericTotal)
	isGeneric <- grepl('TRUE', toString(isGenericTotal))
	#cat("\nexit getIsGeneric ----------- \n")

	return(isGeneric)
}

isFunction <- function(expression) {

	is_function <- try(eval(parse(text = paste("is.function(", expression, ")"))), silent=TRUE ) 
	#print(is_function)
	if (typeof(is_function) == "character"){ # If the try clause above returns an error, then the return value is of type: 'Character'
		res <- try(eval(parse(text = paste("getAnywhere(", expression, ")",sep=""))),silent=TRUE) #if it returns error then it will do a wider search to see if the 		
		is_function <- try(is.function(res[1]),silent=TRUE)

		if(class(is_function)=="try-error" || is_function==FALSE) { #if it still returns error, then assumes que expression is not a function
			return(FALSE)		
		} 

		newobjList <- res$dups

		#print(res$visible)

		lengthnewobjlist <- length(which(newobjList==FALSE))
		if (is_function==TRUE && lengthnewobjlist==1){
			#print(expression)
			#print("................................................")
			assign(paste(res$name), res[1], envir=.GlobalEnv)
			


			#namespace <- findFunctionNamespace(expression)
			#library(namespace,character.only=TRUE)
			#print(namespace)

			return(TRUE)
		} else {
			return(FALSE)
		}
	} 

	#cat("\n", expression, "// is.function :", is_function,"\n")
	#print(is_function)
	return(is_function)
}

isPrimitive <- function(expression) {

	#print("----------------------------------------")
	#print(ls())

	is_primitive <- try(eval(parse(text = paste("is.primitive(", res[1], ")"))), silent=TRUE)
	if(class(is_primitive)=="try-error") {
		res <- eval(parse(text = paste("getAnywhere(", expression, ")",sep="")))
		return(is.primitive(res[1]))	
	} else {
		return(is_primitive)
	}
	

	return(eval(parse(text = paste("is.primitive(", res[1], ")"))))	
}

getFormals <- function(expression) {
	return(eval(parse(text = paste("formals(args(`", expression, "`))",sep=""))))	
}

getExpressionNumber <- function(listed.f.code, expression) {

   pos = gregexpr(expression, listed.f.code) # Returns positions of every match in the function's code (f.code)
   readable.pos <- toString(pos) #

   #print(readable.pos)
   newz <- gsub("-1|-1, |, -1|c\\(|\\)", "",readable.pos)

   #print(newz)

   matchesVector <- as.vector(unlist(strsplit( newz, ", "))) # Converts the string to 
   number_of_pattern <- length(matchesVector)
   
   return(number_of_pattern)
}	





# ==========================================================================================================
# ==========================================================================================================
# ===============================     TEXT STRUCTURE FUNCTIONS       =======================================
# ==========================================================================================================
# ==========================================================================================================



trimNamespace <- function(environmentOutput) {
	fNamespace <- capture.output(environmentOutput)

	fNamespace <- gsub("<|>| ", "", fNamespace)
	fNamespace <- tail(as.vector(unlist(strsplit( fNamespace, ":"))),1)

	return(fNamespace)
}

addNamespaceColumn <- function(analysisResult, groupFunct, fList){

	for (f in fList){
		



		fString <- paste("environment(", f, ")")
		
		fEnvironment <- try(eval(parse(text = fString)),silent=TRUE)

		if(class(fEnvironment)=="try-error" ) { 
			res <- try(eval(parse(text = paste("getAnywhere(", f, ")",sep=""))),silent=TRUE) 
			if(class(res)=="try-error" || length(res$objs)==0) { #which will happen for the functions that are assigned dynamically in the code being analysed
				fEnvironment <- "InvisibleNamespace_DynamicFunction"
			} else {
				#print(res)
				fEnvironment <- environment(res[1])
			}	
		} 

		fNamespace <- trimNamespace(fEnvironment)
		

		rowID <- which(groupFunct$FunctionName == f)


		#cat("\n",f,":\t", fNamespace)

		groupFunct[rowID,"Namespace"] <- fNamespace

	}

	return(groupFunct)

}

addRecursionLevel <- function(showinfo, recursiveLevel) {

	if (recursiveLevel==0)
		return(showinfo)
	else if (recursiveLevel==1)
		cat("\n\t",showinfo)#, "\n")

	else if (recursiveLevel==2)
		cat("\n\t\t",showinfo)
	else if (recursiveLevel==3)
		cat("\n\t\t\t",showinfo)
	else if (recursiveLevel==4)
		cat("\n\t\t\t   ",showinfo)
	else if (recursiveLevel==5)
		cat("\n\t\t\t       ",showinfo)
	else if (recursiveLevel==6)
		cat("\n\t\t\t          ",showinfo)		
	else 
		cat("\n\t\t\t\t     ",showinfo)
	
}

analysisReport <- function(expression, analysisResult) {
	cat("\n\n======================================================================================================",
	"\n===================================== FUNCTION ANALYSIS REPORT =======================================",
	"\n======================================================================================================\n" )
	cat("Function << ", expression, " >> :\n" )
	cat("   This function executes aproximately <<< ", analysisResult$numLines, " >>> lines\n" )
	cat("   ----------------------------------------------------\n")
	cat("   Total identified functions < ", length(analysisResult$allSubFunctions) , " >  |  Total actual functions < ", analysisResult$functionNum , " >  |  Nº miss Identified functions < ", analysisResult$nonFunctionNum , " > \n" )
	cat("   There are <<< ", analysisResult$primitivenumber , " >>> 'PRIMITIVE' functions\n" )
	cat("   There are <<< ", analysisResult$internalnumber , " >>> 'INTERNAL' functions\n" )
	cat("   There are <<< ", analysisResult$genericnumber , " >>> 'GENERIC' functions\n" )
	cat("   There are <<< ", analysisResult$regularnumber , " >>> 'REGULAR' functions\n" )
	cat("   ----------------------------------------------------\n")
	cat("   There are <<< ", analysisResult$ifnumber , " >>> 'IF' invocations\n" )
	cat("   There are <<< ", analysisResult$fornumber , " >>> 'FOR' invocations\n" )
	cat("   There are <<< ", analysisResult$whilenumber, " >>> 'WHILE' invocations\n\n" )
}

analysisReport2 <- function(analysisResult, returnValue = "primitives") {
	groupFunctions <- as.data.frame(table(analysisResult$realFunctions))

	#cat("\n======================= Primitive Functions =====================\n")

	#print(orderedPrimitiveFunct)

	#cat("\n======================= Internal Functions =====================\n")

	#print(orderedInternalFunct)

	#cat("\n======================= Generic Functions =====================\n")

	#print(orderedGenericFunct)

	#cat("\n======================= Regular Functions =====================\n")

	#print(orderedRegularFunct)

	#cat("\n======================= Total Functions =====================\n")

	#totalGroupFunctions <- rbind(orderedPrimitiveFunct, orderedInternalFunct, orderedRegularFunct)
	#orderedGroupFunctions <- totalGroupFunctions[rev(order(totalGroupFunctions$Freq)),]
	#print(orderedGroupFunctions)


	if(returnValue=="primitive") {

		#=========================
		if(length(analysisResult$primitiveFunctions)>0){
			groupPrimitiveFunct <- as.data.frame(table(analysisResult$primitiveFunctions))
			colnames(groupPrimitiveFunct)[1] <- "FunctionName"
			groupPrimitiveFunct$Namespace <- c("NULL")
			groupPrimitiveFunct$Type <- c("primitive") 

			orderedPrimitiveFunct <- groupPrimitiveFunct[rev(order(groupPrimitiveFunct$Freq)),]

			return(orderedPrimitiveFunct)
		} else {
			emptyDF <-  data.frame(FunctionName = "NONE")
			return(emptyDF)
		}
	} else if(returnValue=="internal") {

		#=========================
		if(length(analysisResult$internalFunctions)>0){
			groupInternalFunct <- as.data.frame(table(analysisResult$internalFunctions))
			colnames(groupInternalFunct)[1] <- "FunctionName"
			groupInternalFunct$Namespace <- c("") 
			groupInternalFunct$Type <- c("internal") 
			groupInternalFunct <- addNamespaceColumn(analysisResult, groupInternalFunct, analysisResult$internalFunctions)
			orderedInternalFunct <- groupInternalFunct[rev(order(groupInternalFunct$Freq)),]

			return(orderedInternalFunct)
		} else {
			emptyDF <-  data.frame(FunctionName = "NONE")
			return(emptyDF)
		}
	} else if(returnValue=="generic") {

		#=========================
		if(length(analysisResult$genericFunctions)>0){
			groupGenericFunct <- as.data.frame(table(analysisResult$genericFunctions))
			colnames(groupGenericFunct)[1] <- "FunctionName"
			groupGenericFunct$Namespace <- c("") 
			groupGenericFunct$Type <- c("generic") 
			groupGenericFunct <- addNamespaceColumn(analysisResult, groupGenericFunct, analysisResult$genericFunctions)
			orderedGenericFunct <- groupGenericFunct[rev(order(groupGenericFunct$Freq)),]
			return(orderedGenericFunct)
		} else {
			emptyDF <-  data.frame(FunctionName = "NONE")
			return(emptyDF)
		}
	} else if(returnValue=="regular") {
		#=========================
		if(length(analysisResult$regularFunctions)>0){
			groupRegularFunct <- as.data.frame(table(analysisResult$regularFunctions))
			colnames(groupRegularFunct)[1] <- "FunctionName"
			groupRegularFunct$Namespace <- c("")
			groupRegularFunct$Type <- c("regular")
			groupRegularFunct <- addNamespaceColumn(analysisResult, groupRegularFunct, analysisResult$regularFunctions)
			orderedRegularFunct <- groupRegularFunct[rev(order(groupRegularFunct$Freq)),]
			return(orderedRegularFunct)
		} else {
			emptyDF <-  data.frame(FunctionName = "NONE")
			return(emptyDF)
		}
	}
}





