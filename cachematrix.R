# makeCacheMatrix and cacheSolve caches the inverse of a matrix.

# makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
# It retrieves the inverse from the cache if it had already been calculated
# and had not changed.

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
