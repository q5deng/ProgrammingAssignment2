## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  #makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(data = matrix()) {
    inverse <- NULL
    set <- function(x) {
        data <<- x;
        inverse <<- NULL;
    }
    get <- function() 
      return(data)
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function
 #cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(data, ...) {
    inverse <- data$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    dat <- data$get()
    invserse <- solve(dat, ...)
    data$setinv(inverse)
    return(inverse)
   }

# the end
