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
  setmean <- function(inverse) inv <<- inverse
  getmean <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## cacheSolve() uses makeCacheMatrix() output list and activating solve()
## on the matrix only if the matrix solution is not already cached.
## gets makeCacheMatrix() output list.
cacheSolve <- function(x, ...) {
  inv <- x$getmean()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmean(inv)
  inv
}
