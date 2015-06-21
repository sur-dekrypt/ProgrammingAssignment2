## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function basically makes a list of 4 objects all of whom are functions. Mostly, these are used 
## by the cachesolve function. Precisely 3 of its functions are used by cacheSolve : "get", "getinverse", "setinverse"
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

## This function basically checks if the inverse of a function is already calculated earlier. Otherwise, it 
## calculates it and then caches it so that it does not have to calculate again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
