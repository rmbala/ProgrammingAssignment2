## We follow the methodology for the example provided for the vector example
## The function, `makeCacheMatrix` creates a list containing a function to

##1.  set the value of the Matrix
##2.  get the value of the Matrix
##3.  set the inverse of the Matrix
##4.  get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix(data = NA_real_, nrow = nrow(x), ncol = ncol(x))
        set <- function(y) {
                x <<- y
                m <<- matrix(data = NA_real_, nrow = nrow(x), ncol = ncol(x))
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of teh Matrix refernced 
## in the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse (via getinv)
## from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the inverse via the `setinv`
## function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.na(m[1,1])) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
