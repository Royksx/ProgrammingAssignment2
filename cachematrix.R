## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrixInput = matrix()) {
  inverseCache <- NULL
  
  setMatrix <- function(newMatrix) {
    matrixInput <<- newMatrix
    inverseCache <<- NULL
  }
  
  getMatrix <- function() matrixInput
  
  setInverse <- function(inverseMatrix) inverseCache <<- inverseMatrix
  
  getInverse <- function() inverseCache
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(cacheMatrixObject, ...) {
  cachedInverse <- cacheMatrixObject$getInverse()
  
  if (!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  matrixData <- cacheMatrixObject$getMatrix()
  calculatedInverse <- solve(matrixData, ...)
  cacheMatrixObject$setInverse(calculatedInverse)
  
  calculatedInverse
}