## Together, these functions allow you to calculate the inverses of matrices and cache them,
## so that you can retrieve the inverse from the cache rather than recalculating it if you need it
## again. Use the makeCacheMatrix function to create a "matrix" object that can cache its inverse,
## then use cacheSolve to either calculate the inverse of the matrix object, or retrieve the inverse
## from the cache if it has already been calculated. 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Apply cacheSolve to the object created by running makeCacheMatrix to return the inverse of 
## the original matrix. CacheSolve first checks to see if the inverse has already been calculated. 
## If so, it retrieves the inverse from the cache rather than re-calculating it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
