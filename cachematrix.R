##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        solve_inverse <- function(solve) m <<- solve
        get_inverse <- function() m
        list(set = set, get = get,
             solve_inverse = solve_inverse,
             get_inverse = get_inverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$solve_inverse(m)
        m
}
