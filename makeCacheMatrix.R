# makeCacheMatric gives the inversed matrix through get_inverse. If it is not inversed in the prior execution it gives Null when get_inverse is invoked 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {inver<-ginv(x)
                         inver%%x
  }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Cache function returns the Cached inverse matrix upon repeated execution of cacheSolve function, with a message getting cached data
## If the matrix is not inversed in previous execution, then it will do it as it is doing it for first time
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
 ## Return a matrix that is the inverse of 'x'
  }
