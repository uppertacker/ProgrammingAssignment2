## Storing matrix into cache and retrieving it when required

## Special matrix function with four functions to store and retrieve the data and cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
	x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return an inverse of the special matrix vector from cache (if found), else calculate the inverse and store it in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
