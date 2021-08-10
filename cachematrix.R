## Put comments here that give an overall description of what your
## functions do
## define the argument with default mode of "matrix"
## initialize inv as NULL; will hold value of matrix inverse
## define the set function to assign new
## value of matrix in parent environment
## if there is a new matrix, reset inv to NULL
## define the get fucntion - returns value of the matrix argument
## assigns value of inv in parent environment
## gets the value of inv where called
## you need this in order to refer to the functions with the $ operator

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
}
get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
}
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv  
}
