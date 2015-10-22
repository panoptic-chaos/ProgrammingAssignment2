## This file provides an interface for creating and using Matrix-like objects
## which make use of an internal cache to store their inverses on an as-needed basis


# makeCacheMatrix acts as a factory to build CacheMatrix objects
# These object provide a get/set functionality, as well as a get/setinverse, which is used by cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the 
  self_matrix <- x
  # Initialize the cached matrix inverse to null
  self_matrix_inverse <- NULL
  
  # Setter function 
  set <- function(new_matrix) {
    #  Sets a new internal matrix and clears the cached inverse
    self_matrix <<- new_matrix
    self_matrix_inverse <<- NULL
  }
  
  # Getter function
  get <- function() {
    self_matrix
  }
  
  # Setter for the matrix inverse
  setinverse <- function(matrix_inverse) {
    self_matrix_inverse <<- matrix_inverse
  }
  
  # Getter for the matrix inverse
  getinverse <- function() {
    self_matrix_inverse
  }
  
  # Return a list so we can use the result like a pseudo-object
  list(
    set=set,
    get=get,
    setinverse=setinverse,
    getinverse=getinverse
  )
}


# cacheSolve returns the inverse the matrix 'x'
# It expects CacheMatrix objects built using the makeCacheMatrix()
# It uses their internal caches (getting and setting as appropriate)
cacheSolve <- function(x, ...) {
  # get the cached inverse from x
  matrix_inverse <- x$getinverse()
  
  # if the cached inverse is non-null, we have a valid cached inverse,
  #  just use that
  if (!is.null(matrix_inverse)) {
    message("getting cached data")
    return (matrix_inverse)
  }
        
  # if the cached inverse *was* null, in that case:
  
  # Calculate the inverse matrix
  matrix_inverse <- solve(x$get())
  
  # Store the matrix inverse so we can use the cache next time
  x$setinverse(matrix_inverse)
  
  # Return the inverted matrix
  matrix_inverse
}
