## Assuming x is an invertible matrix, this function sets a function to 
## invert x and then uses that function to invert x and stores the function the 
## execution and original matrix in a list.



makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This funcion calls the inverse execution from 'makeCacheMatrix', if NULL then it solves the
## of x data called from the list set in 'makeCacheMatrix' and stores it in the cache. If the inverse
## is already stored in the cache then it returns a message and the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
