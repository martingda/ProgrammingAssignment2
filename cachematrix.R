## The purpose of these functions is to simplify the processing time of 
## performing the calculation of the inverse of a particular matrix.
## Instead of having to calculate the inverse every time, this functions
## allow for the solution to be stored in a cache the first time it's calculated.
## Then, when the operation is requested again, before making the calculation it
## checks the cache in order to determine if the solutions has been determined before.
## In case the solution is in the cache, it gives that solution thus saving valuable
## time on each iteration.

## This function creates a type of "dummy" variable which is used to store a particular 
## matrix and it's inverse whenever it is calculated. The inverse is stored in the function's 
## cache ready to be retrieved when it's called for.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The purpose of this function is to calculate the inverse of the previously defined matrix.
## Nevertheless, instead of the traditional solve function, this one includes an operation were it 
## checks the cache created on the previous function to see if the inverse has been calculated before. 
## In case it has, it brings the value as it is stored in the cache and it displays a message 
## letting the user know the function didn't calculate the inverse one more time, instead it simply used 
## the stored version in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
