## These two functions makes sure that the inverse of two identical matrices 
## will not be calculated twice but whatever was previously done is 
## gets stored and called if it comes up again.
## function makeCacheMatrix stores the matrices and their inverses that 
## have already been calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## CasheSolve does the actual inverting of a specific matrix 
## after checking if this inverse had already been done or not.
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
