##  makeCacheMatrix will generate list of functions that can be used
##  on the matrix. Using this list, cacheSolve will find the inverse
##  of the matrix in the cache. If not available, it will then solve
##  for the inverse and then store it in the cache.

##  Using the example as a template: this function will have set,
##  get, getinv, and setinv functions.

makeCacheMatrix <- function(x = matrix()) {
    MTX <- NULL
    set <- function(y) {
        x <<- y
        MTX <<- NULL
    }
    get <- function() x
    setinv <- function(INVMX) MTX <<- INVMX
    getinv <- function() MTX
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
}


##  Using the example as a template: first try to get the inverse
##  of the matrix. If available, return. If not, then get the data
##  (matrix) and find the inverse (using the solve() function)
##  now set the inverse matrix. now it is cached (if not already)!

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    MTX <- x$getinv()
    if(!is.null(MTX)) {
        message("getting cached data")
        return(MTX)
    }
    data <- x$get()
    MTX <- solve(data)
    x$setinv(MTX)
    MTX
}
