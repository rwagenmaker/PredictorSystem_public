#suppressWarnings(require(SuppDists))  # Optimized random number generators
library(pryr)
source("argumentsProfiler.R")
source("../RExecutor/rExecutor.R", chdir = TRUE)
options( warn = -1 )


boldcode <- system('bold=`tput bold`; echo "${bold}"', intern=TRUE)
normalcode <- system('normal=`tput sgr0`;echo "${normal}"', intern=TRUE)
underlinedcode <- system('underlined=`tput smul`; echo "${underlined}"', intern=TRUE)
bluecode <- system('blue=`tput setaf 4`; echo "${blue}"', intern=TRUE)
whitecode <- system('white=`tput setaf 7`; echo "${white}"', intern=TRUE)
yellowcode <- system('yellow=`tput setaf 3`; echo "${yellow}"', intern=TRUE)
redcode <- system('red=`tput setaf 1`; echo "${red}"', intern=TRUE)
cyancode <- system('cyan=`tput setaf 6`; echo "${cyan}"', intern=TRUE)

require(Matrix)   # Optimized matrix operations
require(SuppDists)
clear <- function() cat(c("\033[2J","\033[0;0H"))



profileWithSavedEntries_VERSION_2 <- function(){
	performingPopulateFromIncubator <<-TRUE
	directoryPath <- "/home/rwagenmaker/Documents/IST/PredictorSystem/CodeAnalysisRW/ProfilerDocs/forPopulate"
	oldenv <- getwd()
	setwd(directoryPath)

	allfiles <- list.files(pattern = "\\.Rda$")
	for (file in allfiles) {
		cat(boldcode,"\n->> File name: " , file , "\n", normalcode)
		new_populate_list <- readRDS(file)
		
		cat("\n---\n\n")
		mainExpression <- new_populate_list[["mainExpression"]]
		e <<- new_populate_list[["envir"]]
		parent.env(e)<-parent.frame()
		envir <<- e
		#attach(envir)

		print(ls.str(envir))
		clear()
		print(typeof(mainExpression))
		cat("\n--\n")
		print(mainExpression)
		cat("\n---\n\n\n\n")
		setwd(oldenv)


		handleExpression_mainINCUBATOR(mainExpression)

		#r <- try(eval(cv<-"", envir = envir),silent=TRUE)
		#if(class(r)=="try-error"){
		#	cat("\nERROR IN MANUAL PROFILER - ",deparse(functionCommand),"\n")
		#}
		setwd(directoryPath)
		


		#file.remove(file)


	}
	setwd(oldenv)
}

argumentMinerIterator <- function(mainFunctName, majorElement, fNamespace){
	#print(typeof(majorElement))
	#print(ls.str(envir))
	#print(majorElement)

	#if(mainFunctName=="crossprod"){
		err <- try(argumentsMiner(majorElement,packageName=fNamespace, lineAsLanguage=majorElement, environment=envir),silent=TRUE)
		if(class(err)=="try-error" ){
			cat("\n@argumentMinerIterator : \n\n");print(fNamespace)
			print(err)
			cat("\n____________________________________________________\n\n")
		}	


		newcall <- as.list(majorElement)
		elements <- as.list(newcall)
		args <- elements[-1]
		for(arg in args){
			if(!missing(arg) && typeof(arg)=="language"){

				inner_newcall <- as.list(arg)
				inner_elements <- as.list(inner_newcall)
				inner_mainFunctName <- inner_elements[[1]]
				
				inner_fNamespace <- rev(findNamespaceLocal(inner_mainFunctName))
				if(length(inner_fNamespace)==0){
					if(exists(paste(inner_mainFunctName), envir)){
						inner_fNamespace <- ".GlobalEnv"
						cat(redcode,"\ninner iterator:",normalcode);print(inner_fNamespace)
						cat(cyancode,"\n>> ==================== INIATE .GlobalEnv HANDLING: ",normalcode, boldcode, inner_mainFunctName, normalcode, cyancode,"  <<","",">> ============================>", paste(deparse(arg))," |||  package: ", paste(deparse(inner_fNamespace)) , "\n",normalcode)
						argumentMinerIterator(inner_mainFunctName, arg	, inner_fNamespace)
						cat(cyancode,"\n>> ==================== FINISH .GlobalEnv HANDLING: ",normalcode, boldcode, inner_mainFunctName, normalcode, cyancode,"  <<","",">> ============================>", paste(deparse(arg))," |||  package: ", paste(deparse(inner_fNamespace)) , "\n",normalcode)
					}
				} else {

					inner_fNamespace <- strsplit(inner_fNamespace[[1]][1], ":")
					#print(inner_fNamespace)
					inner_fNamespace <- inner_fNamespace[[1]][2]
					cat(redcode,"\ninner iterator:",normalcode);print(inner_fNamespace)
					cat(cyancode,"\n>> ==================== INIATE INNER HANDLING: ",normalcode, boldcode, inner_mainFunctName, normalcode, cyancode,"  <<","",">> ============================>", paste(deparse(arg))," |||  package: ", paste(deparse(inner_fNamespace)) , "\n",normalcode)
					argumentMinerIterator(inner_mainFunctName, arg	, inner_fNamespace)
					cat(cyancode,"\n>> ==================== FINISH INNER HANDLING: ",normalcode, boldcode, inner_mainFunctName, normalcode, cyancode,"  <<","",">> ============================>", paste(deparse(arg))," |||  package: ", paste(deparse(inner_fNamespace)) , "\n",normalcode)
				}


			}
		}
	#}
}


handleInnerExpressionINCUBATOR <- function(majorElement, recursionLevel, previousMain="",my.env =parent.frame()){

	newcall <- as.list(majorElement)
	elements <- as.list(newcall)
	mainFunctName <- elements[[1]]
	args <- elements[-1]

	if(mainFunctName=="for") {
		printPurpose("For", majorElement)
		#-----------------------------------------------------

		iteratorVector <- args[[2]]

		if(class(iteratorVector)=="call"){
			value <- handleInnerExpressionINCUBATOR(iteratorVector, recursionLevel+1, previousMain="<-", my.env=my.env)
		}
		
		forbody <- args[[3]]
		cat("\n- The 'for' body: \n"); print(forbody)	
		

		forBodyCall <- as.list(forbody)
		elementsBodyCall <- as.list(forBodyCall)
		new_mainFunctName <- elementsBodyCall[[1]]
		#cat("\n@for:  NEW MAIN FUNCT NAME:"); print(new_mainFunctName)

		if(new_mainFunctName=="{") {
			execTime_body <- handleExpressionINCUBATOR(forbody, recursionLevel+1, my.env=my.env)
		} else {
			value <- handleInnerExpressionINCUBATOR(forbody, recursionLevel+1, my.env=my.env)
		}

		value <- 1

		

		#final
	
	} else if(mainFunctName=="if") {
		printPurpose("If", majorElement)
		#################################

		print(args)
		ifbody <- args[[2]]
		ifBodyCall <- as.list(ifbody)
		elementsBodyCall <- as.list(ifBodyCall)
		new_mainFunctName <- elementsBodyCall[[1]]



		if(class(args[[1]])=="call"){
			value <- handleInnerExpressionINCUBATOR(args[[1]], recursionLevel+1, previousMain="<-", my.env=my.env)
		}

		if(new_mainFunctName=="{") {
			execTime_body <- handleExpressionINCUBATOR(ifbody, recursionLevel+1, my.env=my.env)
		} else {
			value <- handleInnerExpressionINCUBATOR(ifbody, recursionLevel+1,my.env=my.env)			
		}

		value <- 1

	} else if(mainFunctName=="{") {

		value <- handleExpressionINCUBATOR(majorElement, recursionLevel+1,my.env=my.env)

	} else if(mainFunctName=="[" || mainFunctName=="[[" || mainFunctName=="$") {

		#----------------------------------------------------------------------------------------------------------------------------------
		# WHICH HAPPENS ALWAYS THATE THERE IS AN ASSIGNMENT OPERATION. IN THIS SITUATION ONLY THE SECOND ELEMENT OF THE ASSIGNMENT IS MINED
		codeForMining <- args[[1]]
		#print("CODE FOR MINING:"); 	print(codeForMining)
		value <- handleInnerExpressionINCUBATOR(codeForMining, recursionLevel+1,my.env=my.env)
	
	} else if(mainFunctName=="<-" || mainFunctName=="<<-") {
		printPurpose("Assignment", majorElement)
		#-----------------------------------------------------------------------------------------------------------------------------------
		# WHICH HAPPENS ALWAYS THATE THERE IS AN ASSIGNMENT OPERATION. IN THIS SITUATION ONLY THE SECOND ELEMENT OF THE ASSIGNMENT IS MINED
		
		leftSide <- args[[1]]
		rightSide <- args[[2]]

		if(class(leftSide)=="call"){
			value <- handleInnerExpressionINCUBATOR(leftSide, recursionLevel+1, previousMain="<-", my.env=my.env)
		}
		if(class(rightSide)=="call"){
			value <- handleInnerExpressionINCUBATOR(rightSide, recursionLevel+1, previousMain="<-", my.env=my.env)
		}
		

	} else if(mainFunctName=="function" && previousMain=="<-") {
		value <- 1

	} else if(eval(typeof(paste(mainFunctName)),envir=envir)=="special"){
		printPurpose("Special Character", majorElement)
		#--------------------------------------------------------

		cat(bluecode,"\n>> ==================== INIATE HANDLING: ",normalcode, boldcode, mainFunctName, normalcode, bluecode,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: base", "\n",normalcode)
		
		argumentMinerIterator(mainFunctName, majorElement, "base")
		
		cat(bluecode,"\n<< ==================== FINISH HANDLING ",normalcode, boldcode, mainFunctName, normalcode, bluecode,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: base", "\n",normalcode)
		#for(arg in args) {
		#	if(class(arg)=="call"){
		#		value <- handleInnerExpressionINCUBATOR(arg, recursionLevel+1, previousMain="<-", my.env=my.env)
		#	}
		#}



	} else {
		cat("(zxz)\n")


			#---------------------------------------------------------------------
			# VERIFY IF THE FUNCTION CALL HAS ANY GROUP OF EXPRESSIONS AS ARGUMENT (like "system.time()")
			done<-FALSE; r<-""
			for (arg in args) {
			
				local_newcall <- as.list(arg)
				local_elements <- as.list(local_newcall)
				local_mainFunctName <- local_elements[[1]]
				
				if(local_mainFunctName=="{"){
					value <-  handleExpressionINCUBATOR(arg, recursionLevel+1,my.env=my.env)
					return (1)	
				}
			}

			if(!done) {

				#-------------------------------------------------------------------------------
				# PROCEED WITH THE NORMAL PREDICTION PROCESS

					fNamespace <- rev(findNamespaceLocal(mainFunctName))
					if(length(fNamespace)==0){
						if(exists(paste(mainFunctName), envir)){
							fNamespace <- ".GlobalEnv"
						}
					} else {
						fNamespace <- strsplit( fNamespace[1][[1]], ":")
						fNamespace <- fNamespace[[1]][2]
					}
					#print(fNamespace)
					cat(bluecode,"\n>> ==================== INIATE HANDLING: ",normalcode, boldcode, mainFunctName, normalcode, bluecode,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: ", paste(deparse(fNamespace)) , "\n",normalcode)
					
					#freshAnalysisWrap<-freshExecutionArgs(majorElement, workDir="../RExecutor/")
					argumentMinerIterator(mainFunctName, majorElement, fNamespace)


					#print(str(freshAnalysisWrap))
					
					cat(bluecode,"\n<< ==================== FINISH HANDLING ",normalcode, boldcode, mainFunctName, normalcode, bluecode,"  <<","TIME_&_SIZE",">> ============================>", paste(deparse(majorElement))," |||  package: ", paste(deparse(fNamespace)) , "\n",normalcode)

					#if(r!="NO_HISTORY"){

					#	break

					#}	
				#}	

				if (r=="NO_HISTORY"){

					cat(boldcode, "\n\nDEU CABUM NO handleInnerExpressionINCUBATOR. CAN'T FIND HISTORY\n\n", normalcode)
				}		
			}
	}
	
	#cat("\n--end--",value[["r_execTime"]],"\n")

	return (1)
	cat("\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<< finish handling expression\n")
	
}


#----------------------------------------------------------------
# RETURNS A PREDICTION VALUE FOR THE PIECE OF CODE BEING ANALYSED
handleExpressionINCUBATOR <- function(exp, recursionLevel = 0, my.env =parent.frame()){
	cat(yellowcode,"\n\n\n\n==========================================================================================",
			"\n======================================== START :", recursionLevel, " ======================================\n\n",normalcode)

	stuffsx <- as.character(exp)
	stuffs_lang <- as.list(exp)
	
	for (majorElement in stuffs_lang[-1]) {
		cat("------------------------------------==================-------------------------",recursionLevel,"------- \n")	

		prediction <- handleInnerExpressionINCUBATOR(majorElement, recursionLevel, my.env=my.env)
		try(eval(majorElement, envir = envir), silent=TRUE)
 	}

	cat(yellowcode,"\n\n\n\n==========================================================================================",
	"\n======================================= FINISH :", recursionLevel, " ======================================\n::",normalcode)
 	
 	return(1)

}

#----------------------------------------------------------------
# RETURNS A PREDICTION VALUE FOR THE PIECE OF CODE BEING ANALYSED
handleExpression_mainINCUBATOR <- function(exp, mode="live"){
	performingPopulateFromIncubator <<-TRUE
	stuffsx <- as.character(exp)
	stuffs_lang <- as.list(exp)

	#---------------------------------------------------------------------------------------------
	# CREATE A NEW ENVIRONMENT SO WE CAN KNOW WHICH VARIABLES WERE ADDED TO THE GLOBAL ENVIRONMENT
	my.env <- new.env()
	
	for (majorElement in stuffs_lang[-1]) {
		cat(yellowcode,boldcode,"\n\n-----------------------------==================----------------------------- ", normalcode)	
		cat(yellowcode,boldcode,"\n|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| ", normalcode)	
		cat(yellowcode,boldcode,"\n-----------------------------==================----------------------------- \n", normalcode)	

		predictionList <- handleInnerExpressionINCUBATOR(majorElement, 0, my.env=my.env)
		
		try(eval(majorElement, envir = envir), silent=TRUE)

 	}

	gc()
 
 	return(1)


	
}




banner<-system('banner "Incubator : manual profiler"', intern=TRUE)
for(b in banner){
	cat(b,"\n")
}
cat("\n--\n\n\n"); 


#printSavedEntries()

cat("\n\n\n\n=========================================\n\n\n")

#profileWithSavedEntries()
profileWithSavedEntries_VERSION_2()


cat("\n________________________________________________________________________________________________________________\n")

#Rnorm <- rziggurat  # The fast normal number generator

#smallVector <- Rnorm(100) #840 bytes
#mediumVector <- Rnorm(100000) # 800 Kbytes


#element <- "t(mediumVector)"
#element <- "Rnorm(1000*1000)"
#element <- "system.time(for(i in 1:100) mad(runif(1000)))"
element <- "flush.console()"

#namespace <- find("Rnorm")
#print(namespace)
#argumentsMiner(element,packageName=namespace)
#element <- "t(matrix(smallVector))"
#argumentsMiner(element,packageName="utils")


#printSavedEntries()

#----------------------------------------------------
# GENERATE A VECTOR WITH 100*100 NUMBERS
#x<-Rnorm(200*200)/10
#z <- c(1:10)
#w <- c(11:20)

#element <- "z/w"#"matrix(x, ncol=200, nrow=200)"
#argumentsMiner(element,packageName="base")

#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------- NOT USED IN THIS OPERATION OF PROFILING ----------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------#


stripCommand <- function(line,environment=parent.frame()) {

	newcall <- as.list(parse(text = paste(line)))[[1]]
	elements <- as.list(newcall)
	args <- elements[-1]
	print(args)

	numbersGen <- 1:length(args)
	print(numbersGen[1])
	for(arg in args) {
		if(class(arg)=="call"){

			#print(paste("",2,"_ <- ", sep=""))
			newVar <- paste("a_",numbersGen[1],sep="")
			numbersGen <- tail(numbersGen, -1)
			eval(parse(text = paste(newVar," <- ",deparse(arg),sep="")),environment)

			cat("\n---\n")

			line<- gsub(" ", "", line)# no spaces, so it correctly identifies the pattern
			pattern <- gsub(" ", "", paste(deparse(arg)))
			line <- sub(pattern, get("newVar"), line,fixed=TRUE) # Changes only the 1st pattern match per string

			#line_new <- sub("table(t1, t2)", "a_1", line)

			print(newVar)
			print(line)
		}
	}
	return(line)

}