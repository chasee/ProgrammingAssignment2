# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix: return a list of function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # 1. Set the value for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 2. Get the value for the matrix
  get <- function() x
  
  # 3. Set the value of the inverse
  setInv <- function(inverse) inv <<- inverse
  
  # 4. Get the value of the inverse
  getInv <- function() inv
  
  # Return the matrix with the newly defined functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## cacheSolve: Returns the inverse of a matrix. Checks to see if the matrix
## is already cached, and returns that if it has been. Otherwise, it computes
## the inverse, sets the value in the cache, and then returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  # Return the inverse if it has already been calculated
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Calculate the inverse if it hasn't been done yet
  data <- x$get()
  inv <- solve(data)
  
  # Cache the inverse
  x$setInv(inv)
  
  # Return the inverse
  return(inv)
}
