## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function takes in a matrix x
# It sets m to null
# It has 4 functions that are when the function is first called
# 1. "set" is called when we want to directly set x and make m NULL
# 2. "get" is called to get the matrix x that was set
# 3. "setinv" is called when we want to cache the result of inverse
# 4. "getinv" is called when we want to retrieve the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve follows the steps below:
# 1. attempt to retrieve cache
# 2. check if cache is existent
# 3. if existent, return cache
# 4. if not, calculate inverse of matrix
# 5. Cache the inverse of the matrix
# 6. Final step returns inverse calculation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
