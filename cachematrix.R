## The functions below construct a "matrix" object and return its inverse.
## If the inverse has already been calculated, it is cached. 
## Otherwise, it is calculated.
## -----------------------------------------------------------
## Note: we assume the input matrix is always invertible.
## -----------------------------------------------------------

## The function makeCacheMatrix() constructs a special "matrix" object that 
## caches its inverse. The function receives an invertible matrix as input 
## and outputs an object that contains four functions: 
## get() - outputs the input matrix
## set() - sets the input matrix and resets the inverse value to NULL
## getinverse() - outputs the inverse of the input matrix
## setinverse() - sets the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
      
      im <- NULL
      set <- function(y) {
            x <<- y
            im <<- NULL
      }
      
      get <- function() x
      setinverse <- function(invmat) im <<- invmat
      getinverse <- function() im
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## The function cacheSolve() receives makeCacheMatrix() object as input and 
## returns the inverse of the matrix stored in this object. The function 
## first checks if an inverse already exists, and it has, the inverse is cached. 
## Otherwise, the inverse is calculated and stored.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      im <- x$getinverse()
      if(!is.null(im)) {
            message("Getting cached data")
            return(im)
      }
      
      data <- x$get()
      im <- solve(data, ...)
      x$setinverse(im)
      im
}
