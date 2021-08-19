## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(makeCacheMatrix.object, ...) {
    ## Return a matrix that is the inverse of 'x'
    m.local <- makeCacheMatrix.object$getinverse()
    if(!is.null(m.local)) {
        message("getting cached data")
        return(m.local)
    }
    data <- makeCacheMatrix.object$get()
    m.local.calculated <- solve(data, ...)
    makeCacheMatrix.object$setinverse(m.local.calculated)
    m.local.calculated
}
