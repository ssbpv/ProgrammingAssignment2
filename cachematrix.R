## The cachematrix.R script has two functions which when executed sequentially 
##computes the inverse of a matrix and caches it, so that in subsequent iterations
## the cached value can be used rather than recomputing the inverse of the matrix
## as long as the original matrix remains the same.

## The output argument of the 'makeCacheMatrix' function is a list which has
## the values of the orginal matrix (x) and the inverse of the matrix (invx) 
##computed using 'solve'.At first pass invx has been instantiated to 'NULL'.
##This function takes advantage of lexical soping which allows for objects defined within
##the function to be referenced in the parent environmet, where the objectscan be 
##accessed by the 'cacheSolve' function

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invx <<- solve
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The 'cacheSolve' function first gets the inverse of matrix using 'x$getinverse()',
## if the value of the inverse is'NULL'then it computes it using 'solve' otherwise
##it gets the value from the cached data. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data,...)
        x$setinverse(invx)
        invx
}
