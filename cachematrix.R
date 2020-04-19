## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrixVar = matrix()) {
        matrixInverse <- NULL
        set <- function(y) {
            if (is.null(y)) {
                message("please set a non-null matrix")
                return()
            }
            if (!identical(matrixVar, y)) {
                message("setting new matrix")
                matrixVar <<- y
                matrixInverse <<- NULL
            }
        }
        get <- function() matrixVar
        setMatrixInverse <- function(inverse) matrixInverse <<- inverse
        getMatrixInverse <- function() matrixInverse
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}
cacheSolve <- function(x, ...) {

        if (is.null(x)) {
            message("cache matrix not created")
            return(NULL)
        }

        cachedMatrixInverse <- x$getMatrixInverse()
        if(!is.null(cachedMatrixInverse)) {
            message("getting cached data")
            return(cachedMatrixInverse)
        }
        data <- x$get()
        if (!is.null(data)) {
            newMatrixInverse <- solve(data, ...)
            x$setMatrixInverse(newMatrixInverse)
            newMatrixInverse   
        } else {
            message("please set a non-null matrix")
        }
}
