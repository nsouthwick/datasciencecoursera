## These functions work together to calculate the inverse value of a matrix
## and cahce them.

## The following function creates a matrix that can cache the inverse 
## value of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) s <<- solve
        getmatrix <- function() s
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The following function calculates the inverse value for the matrix 
## that is passed from the function above if not already calculated
## else it pulls the inverse value from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getmatrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setmatrix(s)
        s
}
