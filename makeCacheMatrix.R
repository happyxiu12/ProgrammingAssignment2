# Caching the Inverse of a matrix
# write a pair of functions and use "makeXaxheMatrix" and "cacheSolve" to cache the inverse of this martix 

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
        x <<- y
        Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

 ## Return a matrix that is the inverse of "x"
 
cacheSolve <- function(x, ...) {
   Inv <- x$getInverse()
  if (!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
  }
  mat <- x$get()
  Inv <- solve(mat, ...)
  x$setInverse(Inv)
  Inv
}
