## Put comments here that give an overall description of what your
## functions do

## This function builds a matrix object. More info here: https://class.coursera.org/rprog-008/human_grading/view/courses/972581/assessments/3/submissions

makeCacheMatrix <- function(matrix = matrix()) {
  inverseCache <- NULL
  setMatrix <- function(newMatrix) {
    matrix <<- newMatrix
    inverseCache <<- NULL
  }
  getMatrix <- function() matrix
  setCache <- function(inverse) inverseCache <<- inverse
  getCache <- function() inverseCache
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
  
}


## Caches the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache <- x$getCache()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix <- x$getMatrix()
  inverse <- solve(matrix, ...)
  x$setCache(inverse)
  inverse
}
