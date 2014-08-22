## The following pair of functions can be used together to
## cache the inverse of a matrix to avoid unnecessary
## re-computation of the inverse

## makeCacheMatrix() creates a special matrix object that
## can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
	
		## 'i' will store the computed inverse of the matrix
		## initialize it to NULL
		i <- NULL

		## set() stores a new matrix value in 'x' and resets
		## the computed inverse to NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
	
		## get() returns the value of the matrix
		get <- function() x
		
		## setinverse() caches a new inverse value in 'i'
		setinverse <- function(inverse) i <<- inverse
		
		## getinverse() returns the value of the inverse
		getinverse <- function() i
		
		## makeCachMatrix() returns a list object
		## whose tagged arguments allow the matrix object's
		## get, set, getinverse, and setinverse
		## functions to be called
		list(get = get, set = set,
				getinverse = getinverse,
				setinverse = setinverse)
	
}


## cacheSolve() returns the inverse of the matrix stored
## in the object returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
		
		## Check whether the matrix inverse has already by cached
		i <- x$getinverse()
		
		## If the inverse has been cached, just return it
		if(!is.null(i)) {
				message("getting cached data")
				return(i)
		}
		
		## If the inverse hasn't been cached, we then compute it,
		## cache it, and return it
		
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
