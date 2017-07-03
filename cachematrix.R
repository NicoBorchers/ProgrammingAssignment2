## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## This function creates a special matrix, a list that contains a function to: 1) set the value of a matrix, 2) get the value of a matrix,
## 3) set the value of the inverse of a matrix, and 4) get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv_x <- NULL
  set <- function(y) {
    x <<- y
    Inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) Inv_x <<- solve
  getInverse <- function() Inv_x
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of a matrix, created by the above function.
## The cacheSolve function verifies if there is cached data. If there is, inverse from matrix is obtained from here
## If cached data is NULL, then data is obtained, and the inverse is calculated with the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv_x <- x$getInverse()
  if(!is.null(Inv_x)){
    message("getting cached data")
    return(Inv_x)
  }
  data <- x$get()
  Inv_x <- solve(data)
  x$setInverse(Inv_x)
  Inv_x
}\nAnother change.
