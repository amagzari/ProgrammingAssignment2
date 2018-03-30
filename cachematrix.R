# This set of function were directly inspired from the example used to cache the mean
# The logic remained the same
# The only difference is that instead of calculating and caching the mean of a vector, we are calculating and caching the inverse of aq matrix

# Function containing four functions (set a matrix, get its value, sets and gets its inverse) )
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function to calculate the inverse of the matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  i
}