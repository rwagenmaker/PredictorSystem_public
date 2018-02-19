


isInternalFunction <- function(f.expression) {
	function.body <- eval(parse(text = paste("body(`", f.expression, "`)", sep="")))	
	#print(function.body)
	f.code <- as.character(function.body)
	isInternalTotal <- grepl('.Internal', f.code)
	isInternal <- grepl('TRUE', toString(isInternalTotal))

	return(isInternal)
}

listPackageFunctions <- function(packageName){

	totalFList <- lsf.str(gsub(" ","",paste("package:",packageName,sep="")))
	flist <- as.vector(unlist(strsplit(totalFList, "\n")))

	flist_clean <- character()
	flist_error <- character()
	numPrimitives <- 0
	for (f in flist) {
		#f <- gsub("<-","",f)
		if (isFunction(f)){
			isP <- try(isPrimitive(f), silent=TRUE)   
			if(typeof(isP) == "logical") {

				if (isP) {
					#print(f)
					numPrimitives <- 1 + numPrimitives
				} else	 {
					flist_clean <- c(flist_clean, f) 	
				}

			}
		} else	{ flist_error <- c(flist_error, f) }	
	}

	print(flist_error)
	cat("\nthere were < ", numPrimitives, " >  primitives in the package with name << ", packageName, " >>  out of a total of < ", length(flist) ," > functions \n\n")

	return (flist_clean)
}


simpleListPackageTotalFunctions <- function(packageName) {

	objs <- mget(ls(paste("package:",packageName,sep = "")), inherits = TRUE)
	funs <- Filter(is.function, objs)
	names(funs)
}


listPackagePrimitiveFunctions <- function(packageName) {

	objs <- mget(ls(paste("package:",packageName,sep = "")), inherits = TRUE)
	funs <- Filter(is.function, objs)
	primitives <- Filter(is.primitive, objs)
	names(primitives)
}

listPackageNonPrimitiveFunctions <- function(packageName) {

	objs <- mget(ls(paste("package:",packageName,sep = "")), inherits = TRUE)
	funs <- Filter(is.function, objs)
	primitives <- Filter(is.primitive, objs)

	primitivesNames <- names(primitives)
	funsNames <- names(funs)

	nonPrimitiveNames <- c()
	for(name in funsNames) {
		if(!(name %in% primitivesNames)){
			nonPrimitiveNames <- c(nonPrimitiveNames,name)
		}
	}

	#print(nonPrimitiveNames)

	#sw <- funs$`^`
	#print(sw)
	#cat("\n")
	#sw[1]

	return(nonPrimitiveNames)

}

listPackageInternalFunctions <- function(packageName) {

	internalNames <- c()
	nonPrimitiveNames <- listPackageNonPrimitiveFunctions(packageName)
	#funs <- Filter(is.function, objs)
	#primitives <- Filter(is.primitive, objs)
	#names(primitives)

	for (name in nonPrimitiveNames) {
		if(isInternalFunction(name)) {
			internalNames <- c(internalNames, name)
		}
	}

	print(internalNames)
}



#isNotPrimitiveFunction <- function(expression) {

#	is_primitive <- eval(parse(text = paste("is.primitive(`", expression, "`)", sep="")))
#	cat("\n expression", expression, "::", is_primitive )
#	return(!is_primitive)
#}

#cat("\n======================= Primitive Functions =====================\n")
#listPackagePrimitiveFunctions("base")

#cat("\n\n\n")
#cat("\n======================= Internal Functions =====================\n")


#listPackageInternalFunctions("base")




#cat("\n======================= Non Primitive Functions =====================\n")
#listPackageNonPrimitiveFunctions("base")