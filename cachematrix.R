## Put comments here that give an overall description of what your
## functions do: 
## The first function assigns the given matrix
## to a variable in a different environment,
## sets the matrix's inverse to another variable
## in the same environment, and keeps track of them
## (providing access to the alternate environment).
## The second function accesses an inverse, either by getting it
## from the first or by getting it itself, then storing it
## in the first function.


## Write a short comment describing this function: 
## cacheMatrix takes a matrix as an argument and creates an
## empty NULL inverse. It sets the given matrix to a variable in
## a different environment, gets the matrix from that environment,
## calculates the inverse matrix and sets that to a variable in
## the "different" environment used earlier, and gets the
## inverse matrix back from the alternate enviroment.
## It returns everything as a list at the end.

cacheMatrix <- function(matr = matrix()) {
  inv <- NULL
  set <- function(y) {
    matr <<- y
    inv <<- NULL
  }
  get <- function() matr
  setinverse <- function(solve) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Write a short comment describing this function:
## Checks the cache to see if the inverse matrix
## has already been calculated. If it has, returns that.
## If not, it gets the inverse and uses cacheMatrix to
## cache the inverse. Finally, it prints the inverse.

cacheSolve <- function(matr, ...) {
  inv <- matr$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- matr$get()
  inv <- solve(data, ...)
  matr$setinverse(inv)
  inv
}
