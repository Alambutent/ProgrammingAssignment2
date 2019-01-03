## makeCacheMatrix function uses lexical scoping to assign a matrix
## from the global environment to a cached value within the function
## environment. This can then be used in conjunction with cacheSolve
## to store the inverse of a matrix object. 
##
## Cached value can be manipulated using functions defined in the 
## resulting list (set,get,setinverse,getinverse)
## 
## New matrix object inversion must be instantiated with cacheSolve
##

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
