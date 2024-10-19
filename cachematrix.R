## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize cache for inverse
  
  set <- function(y) {
    x <<- y  # Store the matrix
    inv <<- NULL  # Clear cache since the matrix has changed
  }
  
  get <- function() x  # Get the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Store the inverse
  getInverse <- function() inv  # Get the inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get cached inverse
  
  if (!is.null(inv)) {
    message("Getting cached data")  # Message if inverse is already cached
    return(inv)  # Return cached inverse
  }
  
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}

