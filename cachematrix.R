## cachematrix

## A set of functions to remember an input matrix and compute
## the corresponding inverse the first time requested and remember
## it to more efficiently respond for subsequently requests.

## Example Usage:

##     > m <- matrix(c(1.0, 0.5, 0.5, 0.3333333), 2, 2)

##     > cm <- makeCacheMatrix(m)

##     > cacheSolve(cm)
##               [,1]      [,2]
##     [1,]  4.000001 -6.000002
##     [2,] -6.000002 12.000005

##     > cacheSolve(cm)
##     getting cached solution
##               [,1]      [,2]
##     [1,]  4.000001 -6.000002
##     [2,] -6.000002 12.000005


## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {

        # Store input matrix to be solved
        x <<- y

	# Initialize solution cache
        s <<- NULL
    }

    # Retrieve matrix to be solved
    get <- function() x

    # Store inverse solution
    setsolve <- function(solve) s <<- solve

    # Retrieve cached inverse solution
    getsolve <- function() s

    # Return list of custom functions for this matrix
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of the cached matrix
    ## x corresponds to the list of functions returned for the cached matrix
    ## Other options of the solve() function may be optionally provided

    # See whether the solution has already been computed
    s <- x$getsolve()

    # If the inverse solution was previously cached, then return it
    if (!is.null(s)) {
        message("getting cached solution")
        return(s)
    }

    # Otherwise, the solution needs to be computed

    # Retrieve the earlier cached input matrix
    data <- x$get()

    # Compute the inverse using any provided options for the solve() function
    s <- solve(data, ...)

    # Cache the computed solution, so we can return it in the future without
    # the need to compute it again.
    x$setsolve(s)

    # Return the computed inverse solution
    s
}










