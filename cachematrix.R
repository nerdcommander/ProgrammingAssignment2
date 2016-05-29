## The goal of these functions is to calculate the inverse of a matrix 
## only when necessary by caching the results in a variable in the global environment
## 

## makeCacheMatrix makes a list of functions that can set and get the matrix or 
## its inverse which is stored in the variable inv

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve gets the inverse from inv if it has been calculated
## or calculates it, sets it and prints it if inv = NULL

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
