## Functions can be written to store and cache the inverse of a Matrix
## When computations become too lengthy to repeatedly caculate, it is often 
## beneficial to cache that calculation for later retrieval rather than 
## calculating that variable again. 

## This function creates a special "Matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) { 
        x <<- y
      invs <<- NULL
}

      get <- function() x
      setinvs <- function(inverse) invs <<- inverse
      getinvs <- function() invs
      list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
} 

## This next function computes the inverse of the special "Matrix" created by the function
## above (makeCacheMatrix). If the inverse has already been calculated provided the matrix
## has not changed, then it should retreive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinvs()
        if (!is.null(invs)) {
        message("getting cached data")
        return(invs)

        }
        mdata <- x$get()
        invs <- solve(mdata, ...)
        x$setinvs(invs)
        return (invs)
}