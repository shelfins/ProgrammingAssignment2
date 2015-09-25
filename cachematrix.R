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


## cacheSolve retruns the inverse of a special "matrix" returned by makeCacheMatrix. If the inverse
## has already been calculated, cacheSolve will retrieve the solution from the cache.

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
