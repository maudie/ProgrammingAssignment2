## The following pair of functions can be used together to
## cache the inverse of a matrix to avoid unnecessary
## re-computation of the inverse

## makeCacheMatrix() creates a special matrix object that
## can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
	
		## i will store the computed inverse of the matrix
		## initialized to NULL prior to computation
		i <- NULL

		## When set is called on the matrix, the value of
		## y is assigned to the matrix, and any previously
		## computed inverse is discarded by resetting i
		## to NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
	
		## Returns the value of the matrix
		get <- function() x
		
		## Given the computed inverse of the matrix
		## setinverse stores the value in i
		setinverse <- function(inverse) i <<- inverse
		
		## Returns the value of the matrix inverse
		getinverse <- function() i
		
		## Return a list object with tagged arguments
		## that will allow the matrix object's
		## get, set, getinverse, and setinverse
		## functions to be called
		list(get = get, set = set,
				getinverse = getinverse,
				setinverse = setinverse)
	
}


## cacheSolve() calculates the inverse of the matrix stored
## in the object returned by makeCacheMatrix()
## If the inverse has been computed previously, cacheSolve()
## will return the cached value. Otherwise cacheSolve()
## will compute the inverse, cache it, and return it

cacheSolve <- function(x, ...) {
		
		## Check whether the matrix inverse has already by cached
		i <- x$getinverse()
		
		## If the inverse has been cached, just return it
		if(!is.null(i)) {
				message("getting cached data")
				return(i)
		}
		
		## If the inverse hasn't been cached, then compute it,
		## store it, and return it
		
		## First, get the matrix that needs to be solved out of
		## the special matrix object
		data <- x$get()
		
		## Then, perform the actual inverse computation
		i <- solve(data, ...)
		
		## Before returning the inverse, cache it for later
		x$setinverse(i)
		
		## And finally, return the matrix that is the inverse of 'x'
		i
}
