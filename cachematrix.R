## Put comments here that give an overall description of what your
## functions do

## These functions introduce a special matrix object which caches its inverse.
## To create such a special matrix invData you execute \code{invData <- makeCacheMatrix(x)}
## where x is an ordinary matrix. You can then get the value with \code{x$get()}
## and change the value with \code{x$setInverse(y)} where y is an ordinary matrix.
## You can get the inverse with \code{cacheSolve(m)}.


#' Create a special "matrix" object that can cache its inverse.
#' 
#' @param x A matrix
#' 
#' @return A list containing four functions to set and get the value of the
#'     matrix and to set and get the inverse of the matrix
#'     

makeCacheMatrix <- function(x = matrix()) {
	
	invData <- NULL
	# Define function to set the value of the matrix. It also clears the old
	# inverse from the cache
	set <- function(y) {
	       x <<- y          # Set the value
	       invData <<- NULL # Clear cache
	    }
	# Define function to get the value of matrix
	get <- function() x
	# Define function to set inverese.
	setInverse <- function(inverse) invData <<- inverse
	# Define function to get inverse
	getInverse <- function() invData
	
	#Return list with the above functions
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


#' Return inverse of matrix x
#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve retrieves the 
#' inverse from the cache.
#' 
#' @param x a special matrix created with makeCacheMatrix
#' 
#' @return The inverse of the matrix x
#' 

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
		
	invData <- x$getInverse() # This fetches the cached value for the inverse
	if(!is.null(invData)) { # If the cache was not empty, just return it
		     message("getting cached data.")
		    return(invData)
	}
	# The cache was empty. Need to calculate it, cache it, and then return it.
	data <- x$get()         # Get value of matrix
	invData <- solve(data)  # Calculate inverse
	x$setInverse(invData)   # Cache the result
	invData	                # Return the inverse
}

