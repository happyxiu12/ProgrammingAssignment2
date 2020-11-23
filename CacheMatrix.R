# Caching the Inverse of a matrix
# write a pair of functions and use "makeXaxheMatrix" and "cacheSolve" to cache the inverse of this martix 
## Write a short comment describing this function -- makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {                                ##establishes the function makeCacheMatrix
                    inv <- NULL                                            
                    set <- function(y){                                    #sets the value of the matrix
                            x <<- y 
                            inv <<- NULL
                    }
                    get <- function() x                                    ##pulls the value of the function 
                    setInverse <- function(inverse) inv <<- inverse        ##sets the value of the inverse
                    getInverse <- function() inv                           ##pulls the value of the inverse
                    list(set = set, 
                         get = get, 
                         setInverse = setInverse, 
                         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()                                ##This retrieves the function from makeCacheMatrix and stores the value.
        if (!is.null(inv)) {                                 ##If there is already a value in inv it will return this value, otherwise it will process
                message("getting cached data")             
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)                                
        inv                                                 ##returns the data
}

