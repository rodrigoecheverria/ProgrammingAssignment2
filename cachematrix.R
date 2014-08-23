## These functions create a wrapper around matrix objects which are
## able to save the result of the otherwise costly matrix inversion
## and return it when requested multiple times.

## makeCacheMatrix defines and returns 4 functions: get, set,
## getinverse and set inverse. The key for it to work is lexical
## scoping: when makeCacheMatrix is called it defines those
## functions with a reference to a matrix in a different scope
## when they are called they act on it.
## The other key aspect is that it can store internally the inverse
## of the matrix, so it trades-off memory for computation.

makeCacheMatrix <- function (x = matrix()) {
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

## cacheSolve uses the "subfunctions" defined previously to access
## the members of the matrix. Specifically, it accesses the inverse
## matrix, if its calculated (not null) it returns it, if not, it
## calculates it, stores it (using setinverse) and returns it.

cacheSolve <- function(x, ...) {
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