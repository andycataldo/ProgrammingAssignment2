## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates matrix object that can cache its inverse
## 1 set value of vector, 2 get value of vector
## 3 set value of inverse, 4 get value of inverse

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) mx <<- solve
    getsolve <- function() mx
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## if inverse has already been calculated and matrix is unchanged, then 
## cacheSolve retrieves the inverse matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mx <- x$getsolve()
    if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    data <- x$get()
    mx <- solve(data, ...)
    x$setsolve(mx)
    mx
}
