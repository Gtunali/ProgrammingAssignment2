## Put comments here that give an overall description of what your
## functions do

## creates a matrix that can cash it's inverse
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    getInverse <- function() m_inv
    setInverse <- function(inv) m_inv <<- inv
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Returns the inverse of a matrix by either returning the value from the cache
## (if available), or computes it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getInverse()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    ## if came here, the cache is empty, so it will solve
    data <- x$get()
    m_inv <- solve(data, ...)
    x$setInverse(m_inv)
    m_inv
}
