## One of these functions creates a matrix
## The other one retrieves its inverse
## user: GerardoDMC
## The next function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set =set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function retrieves the inverse of that matrix
## if it has already been calculated, if not, it computes
## the inverse of the matrix created by using the 
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv<- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
}
