## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## setMatrix        set the value of a matrix
## getMatrix        get the value of a matrix
## setInverse       set the cached value (inverse of the matrix)
## getInverse       get the cached value (inverse of the matrix)


makeCacheMatrix <- function(x = numeric()) {
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to list NULL
  cache <- NULL
  # set the matrix in the cache
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, the cache is empty
    cache <<- NULL
  }
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  # set the Inverse of the matrix using solve 
  setInverse <- function(solve) {
    cache <<- solve
  }
  # get the cached value of SetInverse
  getInverse <- function() {
    cache
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


# generate the inverse of the matrix created with function makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value of set inverse
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in the cache and return
  data <- y$getMatrix()
  inverse <- solve(data)
  y$setInverse(inverse)
  inverse
}