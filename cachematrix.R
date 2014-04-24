## Put comments here that give an overall description of what your
## functions do

# The function returns a new CacheMatrix object, which is a list 
# that contains functions to access the matrix and its cached inverse form

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setsolve <- function(computed_solve) s <<- computed_solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)        
}


# This functions returns a cached inverse matrix of a CacheMatrix object
# If the cached data is not present it is computed and saved

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s    
    
}

# Simple function to test the implementation

testCacheMatrix <- function() {
    
    m <- matrix(c(1:4),2,2)
    
    cm <- makeCacheMatrix(m)

    # Iteration 1: value calculated, Iteration 2&3: cached value returned
    
    for (i in 1:3) {
        print(paste0("Iteration ", i))
        print(cacheSolve(cm))
    }
    
}
