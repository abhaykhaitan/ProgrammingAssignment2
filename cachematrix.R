## Create a Matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  # Return Statement
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse for the matrix retured by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  # Return Statement
  inverse     
}
