##  These Functions will cache the inverse of a matrix and retrieve the cached
##  results if already computed and no changes to matrix.
##  This assumes an invertible matrix is supplied to the function.

##  This function creates a special function which is a list of functions
##  that will set the value of the matrix, get the value of the matrix, 
##  set the value of the inverse of the matrix, get the value of the inverse
##  of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This functions returns the inverse of the special matrix created, but first checks
## to see if the matrix has already been solved, if so it used the cached value.
## Otherwise, it calculates the inverse of the matrix and assigns to setinverse.

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if(!is.null(m)) {
      message("Retrieving cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}