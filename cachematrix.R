## These functions can be used to cache the result of an inversed matrix
## saving time and providing a better performance

## This function saves the value of the inversed matrix in a variable called m.
## Next time that this value is required then it just return its value but avoids
## an additional calculation for this

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Evaluates if there is a value already cached for the inverse. If it is then it just returns
## this value, otherwise the inverse is calculated and cached

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}