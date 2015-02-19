## These functions take a matrix and computes for its inverse.
## The first time the inverse of the matrix is required, it computes it and stores the result in a cache.
## Thereafter, every time the inverse is required again, it gets the inverse from the cache.

## The following function, makeCacheMatrix creates the matrix, which is really a list containing a function to:
##    set the value of the matrix (set)
##    get the value of the matrix (get)
##    set the value of the inverse (setinverse)
##    get the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL                                           ## Initially set the inverse to null
	set <- function(y) {                                ## Create the matrix
		x <<- y
		i <<- NULL
	}
	get <-function() x                                  ## Return the matrix
	setinverse <- function(inverse) i <<- inverse       ## set the inverse
	getinverse <- function() i                          ## return the inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with the above function.
## However, it first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value in the cache via the setinverse function.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()                     ## get the inverse from cache, if any
	if(!is.null(i)) {                       ## if there is value in the cache
		message("getting chached data")     ## inform that we're using the cached value of the inverse, and
		return(i)                           ## return what is in the cache
	}
	data <- x$get()                         ## otherwise, compute for the inverse
	i <- solve(data)
	x$setinverse(i)                         ## store the computed inverse into the cache, then
	i                                       ## return the computed inverse
}
