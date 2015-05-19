## makeCacheMatrix take a matrix as argument and return a list of function to get/set matrix value and inversion of the matrix
## cacheSolve take a matrix as argument calculating inversion of the matrix, then cache and return the inversion of the matrix

## Usuage:
## Init: x <- makeCacheMatrix()
## set value: x$set(y)
## get value: y <- x$get()
## set mean value: x$setmean(mean)
## get mean value: mean <- getmean()

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
		        x <<- y
		        I <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse) I <<- inverse
		getInverse <- function() I
		list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## First create a cache matrix and then cache the inversion by using cacheSolve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
        		message("getting cached data")
        		return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
