source("../../RExecutor/rExecutor.R", chdir=TRUE)
options( warn = -1 )
clear <- function() cat(c("\033[2J","\033[0;0H"))
clear()

require(Matrix)   # Optimized matrix operations
require(SuppDists)  # Optimized random number generators
library(ggplot2)
library(car)

#timeX<-system.time({

	runs <- 3     # Number of times the tests are executed
	times <- rep(0, 15); dim(times) <- c(5,3)

	Runif <- rMWC1019 # The fast uniform number generator
	a <- rMWC1019(10, new.start=TRUE, seed=492166)  # Init. the generator
	Rnorm <- rziggurat  # The fast normal number generator
	b <- rziggurat(10, new.start=TRUE)  # Init. the generator
	remove("a", "b")
	options(object.size=100000000)

if(isTRUE(FALSE)){
predictorSystemHandler({
 			cat("   III. Programmation\n")
			cat("   ------------------\n")
			if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

			# (1)
			cumulate <- 0; a <- 0; b <- 0; phi <- 1.6180339887498949
			for (i in 1:runs) {
			  a <- floor(Runif(3500000)*1000)
			  invisible(gc())
			  timing <- system.time({
			    b <- (phi^a - (-phi)^(-a))/sqrt(5)
			  })[3]
			  cumulate <- cumulate + timing
			}
			timing <- cumulate/runs
			times[1, 3] <- timing
			cat(c("3,500,000 Fibonacci numbers calculation (vector calc)(sec): ", timing, "\n"))
			remove("a", "b", "phi")
			if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

			# (2)
			cumulate <- 0; a <- 3000; b <- 0
			for (i in 1:runs) {
			  invisible(gc())
			  timing <- system.time({
			    b <- rep(1:a, a); dim(b) <- c(a, a);
			    b <- 1 / (t(b) + 0:(a-1))
			    # Rem: this is twice as fast as the following code proposed by R programmers
			    # a <- 1:a; b <- 1 / outer(a - 1, a, "+")
			  })[3]
			  cumulate <- cumulate + timing
			}
			timing <- cumulate/runs
			times[2, 3] <- timing
			cat(c("Creation of a 3000x3000 Hilbert matrix (matrix calc) (sec): ", timing, "\n"))
			remove("a", "b")
			if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

			# (3)
			cumulate <- 0; c <- 0
			gcd2 <- function(x, y) {if (sum(y > 1.0E-4) == 0) x else {y[y == 0] <- x[y == 0]; Recall(y, x %% y)}}
			for (i in 1:runs) {
			  a <- ceiling(Runif(400000)*1000)
			  b <- ceiling(Runif(400000)*1000)
			  invisible(gc())
			  timing <- system.time({   
			    c <- gcd2(a, b)                            # gcd2 is a recursive function
			  })[3]
			  cumulate <- cumulate + timing
			}
			timing <- cumulate/runs
			times[3, 3] <- timing
			cat(c("Grand common divisors of 400,000 pairs (recursion)__ (sec): ", timing, "\n"))
			remove("a", "b", "c", "gcd2")
			if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

			# (4)
			cumulate <- 0; b <- 0
			for (i in 1:runs) {
			  b <- rep(0, 500*500); dim(b) <- c(500, 500)
			  invisible(gc())
			  timing <- system.time({
			    # Rem: there are faster ways to do this
			    # but here we want to time loops (220*220 'for' loops)! 
			    for (j in 1:500) {
			      for (k in 1:500) {
			        b[k,j] <- abs(j - k) + 1
			      }
			    }
			  })[3]
			  cumulate <- cumulate + timing
			}
			timing <- cumulate/runs
			times[4, 3] <- timing
			cat(c("Creation of a 500x500 Toeplitz matrix (loops)_______ (sec): ", timing, "\n"))
			remove("b", "j", "k")
			if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

			# (5)
			cumulate <- 0; p <- 0; vt <- 0; vr <- 0; vrt <- 0; rvt <- 0; RV <- 0; j <- 0; k <- 0;
			x2 <- 0; R <- 0; Rxx <- 0; Ryy <- 0; Rxy <- 0; Ryx <- 0; Rvmax <- 0
			# Calculate the trace of a matrix (sum of its diagonal elements)
			Trace <- function(y) {sum(c(y)[1 + 0:(min(dim(y)) - 1) * (dim(y)[1] + 1)], na.rm=FALSE)}
			for (i in 1:runs) {
			  x <- abs(Rnorm(45*45)); dim(x) <- c(45, 45)
			  invisible(gc())
			  timing <- system.time({
			    # Calculation of Escoufier's equivalent vectors
			    p <- ncol(x)
			    vt <- 1:p                                  # Variables to test
			    vr <- NULL                                 # Result: ordered variables
			    RV <- 1:p                                  # Result: correlations
			    vrt <- NULL
			    for (j in 1:p) {                           # loop on the variable number
			      Rvmax <- 0
			      for (k in 1:(p-j+1)) {                   # loop on the variables
			        x2 <- cbind(x, x[,vr], x[,vt[k]])
			        R <- cor(x2)                           # Correlations table
			        Ryy <- R[1:p, 1:p]
			        Rxx <- R[(p+1):(p+j), (p+1):(p+j)]
			        Rxy <- R[(p+1):(p+j), 1:p]
			        Ryx <- t(Rxy)
			        rvt <- Trace(Ryx %*% Rxy) / sqrt(Trace(Ryy %*% Ryy) * Trace(Rxx %*% Rxx)) # RV calculation
			        if (rvt > Rvmax) {
			          Rvmax <- rvt                         # test of RV
			          vrt <- vt[k]                         # temporary held variable
			        }
			      }
			      vr[j] <- vrt                             # Result: variable
			      RV[j] <- Rvmax                           # Result: correlation
			      vt <- vt[vt!=vr[j]]                      # reidentify variables to test
			    }
			  })[3]
			  cumulate <- cumulate + timing
			}
			times[5, 3] <- timing
			cat(c("Escoufier's method on a 45x45 matrix (mixed)________ (sec): ", timing, "\n"))
			remove("x", "p", "vt", "vr", "vrt", "rvt", "RV", "j", "k")
			remove("x2", "R", "Rxx", "Ryy", "Rxy", "Ryx", "Rvmax", "Trace") 
			if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

			times[ , 3] <- sort(times[ , 3])
			cat("                      --------------------------------------------\n")
			cat(c("                Trimmed geom. mean (2 extremes eliminated): ", exp(mean(log(times[2:4, 3]))), "\n\n\n"))

			cat(c("Total time for all 15 tests_________________________ (sec): ", sum(times), "\n"))
			cat(c("Overall mean (sum of I, II and III trimmed means/3)_ (sec): ", exp(mean(log(times[2:4, ]))), "\n"))
			remove("cumulate", "timing", "times", "runs", "i")
			cat("                      --- End of test ---\n\n")
})
# print(timeX)

}

#a <- Rnorm(10*10); dim(a) <- c(10, 10)
#b <- Rnorm(100*100); dim(b) <- c(100, 100)
#c <- Rnorm(1000*1000);dim(c) <- c(1000, 1000)
#d <- Rnorm(5000*5000);dim(d) <- c(5000, 5000)
#e <- Rnorm(10000*10000);dim(e) <- c(10000, 10000)

#x <- Rnorm(2000*2000);dim(x) <- c(2000, 2000)
exampleData <- Prestige
#head(prestige,5)

#str(prestige)

#summary(prestige)
predictorSystemHandler({

	qplot(mpg, carb, data = mtcars)
	qplot(mpg, data = mtcars)
	qplot(drat,qsec, data = mtcars)
	qplot(gear,data = mtcars)

})

#######################################################

