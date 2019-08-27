## Functions creating caching matrix with inverse
## makeCacheMatrix(matix) create initialized cache
## cache$get - returns stored matrix
## cache$set - changes stored matrix
## cacheSolve - solves inverse


## Function create cache object active matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y)
		{x <<- y
		inverse <- NULL}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function sloved, saved, returns inverse cached matrix or return it if already exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if (!is.null(xi)) 
		{message("getting cached data")
		return(inverse)}
	x2 <- x$get()
	inverse <- solve(x2, ...)
	x$setinverse(inverse)
	return(inverse)
}