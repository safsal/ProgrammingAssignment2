## this is my solution to the matrix inversion caching task.
## makeCacheMatrix() makes a list setters and getters for matrix inversion.
## cacheSolve() uses makeCacheMatrix() output list and activating solve()
## on the matrix only if the matrix solution is not already cached.

## makeCacheMatrix() makes a list setters and getters for matrix inversion.
## gets a matrix.
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


## cacheSolve() uses makeCacheMatrix() output list and activating solve()
## on the matrix only if the matrix solution is not already cached.
## gets makeCacheMatrix() output list.
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
