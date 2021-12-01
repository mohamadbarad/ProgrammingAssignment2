## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # This function creates a special "matrix" object that can cache its inverse
        InvMat <- NULL
        set <- function(y) {
                x <<- y
                InvMat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) InvMat <<- solve
        getinv <- function() InvMat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InvMat <- x$getinv()
        if(!is.null(InvMat)) {
                message("getting cached data")
                return(InvMat)
        }
        data <- x$get()
        InvMat <- solve(data,...)
        x$setinv(InvMat)
        InvMat
        
}

