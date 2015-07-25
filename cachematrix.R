## makeCacheMatrix creates a special "matrix" object and returns a list of getter and setter functions
## cacheSolve returns the inverse of a matrix and stores it in a cache allowing subsequent calls to used the cached value
## Please note i over-commented intentionally!

## Matrix" object that can cache its inverse
## Returns a list of getter and setter functions
makeCacheMatrix <- function(x = matrix()) {
  # sets variable invX to null as a placeholder
  invX <- NULL
  # setter for x
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  # getter for x
  get <- function() x
  # setter for the inverse of x
  setinverse<- function(inverse) invX <<- inverse
  # getter for the inverse of x
  getinverse <- function() invX
  # return the list of getters and setters
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # assigns the cached inverse if we have it
  invX <- x$getinverse()
  if (!is.null(invX)) {
    # checks if we have the inverse from the cache and if we do returns it
    return(invX)
  } else { 
    # if we don't have the inverse from the cache we calculate it, store it in the cache and then return it
    invX <- solve(x$get())
    x$setinverse(invX)
    return(invX)
  }
}
