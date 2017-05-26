## Very similar to what is done in the "Mean" example. 
## makecacheMatrix() and cacheSolve() are used to read the value of the inverse of a matrix from cache, as long as  the inverse is calculated once. 

## makeCachematrix prepares the matrix to be handed to cachesolve in proper format.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve reads the inverse from cache if it's calculated before, otherwise it calculates and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}
