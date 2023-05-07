# Cachematrix
# Define function to create special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # Create an empty cache
  inv <- NULL
  
  # Define function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Define function to get the matrix
  get <- function() x
  
  # Define function to get the cached inverse
  getinv <- function() inv
  
  # Define function to set the cached inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Return a list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Define function to compute the inverse of the matrix
cacheSolve <- function(x, ...) {
  
  # Get the cached inverse if it exists
  inv <- x$getinv()
  
  # If the cached inverse exists, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the cached inverse does not exist, compute it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return the inverse
  inv
}
