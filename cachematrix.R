## The following functions cache the inverse of a matrix.

## makeCacheMatrix 
## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      # get the value of the matrix
      get <- function() x
      # set the value of the inverse of the matrix
      setinverse <- function(inv) inverse <<- inv
      # get the value of the inverse of the matrix
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      # verify the inverse value stored previously with the getinverse function 
      # inside the function makeCacheMatrix IF it exists and is not NULL. 
      # If it exists in memory it returns the inverse value... 
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      # ...ELSE data gets the matrix stored with makeCacheMatrix
      # inverse computes the inverse of the matrix stored in data
      # x$setinverse(inverse) stores it in the object generated assigned 
      # with makeCacheMatrix. Finally it prints the inverse matrix.
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
      
}