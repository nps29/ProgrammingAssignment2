## Put comments here that give an overall description of what your
## functions do

## Data type containing a matrix and its inverse, together with getter and setter 
## functions.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL;
  set <- function( y ) {
    x <<- y;
    inverse <<- NULL;
  }
  get <- function() x;
  setinverse <- function( inverseMatrix ) inverse <<- inverseMatrix;
  getinverse <- function() inverse;
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}

## cacheSolve takes a CacheMatrix as input, and returns the inverse.
## If the inverse has not been set yet, it is calculated first, and then
## set.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
