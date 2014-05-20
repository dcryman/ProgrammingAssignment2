## cachematrix.R
## author: dcryman 5/20/14
## functions to calculate the inverse of a matrix, and cache and retrieve the result


## makeCacheMatrix returns a list of the following functions:
## set: create a matrix
## get: get the matrix
## setinv: set the inverse of the matrix
## getinv: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
        }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve first checks if a matrix inverse is cached
## if no cache value is found, it then calculates and stores the matrix inverse

cacheSolve <- function(x) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}