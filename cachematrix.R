## Following functions, when used together, computes a matrix inverse, caches it on 1st computaion,
##  then allows user to reuse result without having to recompute inverse when that matrix inverse must be
##  supplied for a computation, say, in a loop  (by calling cacheSolve)

## makeCacheMatrix creates a special "matrix" object that can cache its inverse by creating a special
## "matrix", which is really a list containing a function to set and get value of the matrix as well as
##  set and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Inputs: a matrix x
    ## Outputs:

    ## Usage:
    ##   Create test matrix:
    ##  mdat <- matrix(c(1,2,3, 11,-12,13, 14, 15, -16), nrow = 3, ncol = 3, byrow = TRUE)
    ##
    ##  m1 <- makeCachMatrix(mdat)
    ##  cacheSolve(m1)
    ##
    ##    should give output:
    ##                [,1]         [,2]        [,3]
    ##  [1,] -0.001752336  0.044976636  0.03621495
    ##  [2,]  0.209112150 -0.033878505  0.01168224
    ##  [3,]  0.194509346  0.007593458 -0.01985981

    # initialize result to NULL
    inv <- NULL

    # define setter (from above, m1$set() may be used to reinitialize cache to NULL from any environment)
    set <- function(y) {
        x <<- y       # argument y may be defined in any environment
        inv <<- NULL
    }

    # define getter
    get <- function() x

    # define user access functions
    setinverse <- function(solve) inv <<- solve  # function def to cache 1st time inverse computation
    getinverse <- function() inv                 # function def to get cached (or NULL) inverse data

    # object definition for access to user functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix, It does this by
##  calculating the mean of the special "matrix" created with makeCacheMatrix. However, it first checks
##  to see if the inverse has already been calculated. If the inverse has already been calculated (and
##  the matrix has not changed), then the cachesolve retrieves the inverse from the cache. Otherwise, it
##  calculates the inverse of the matrix data and sets the value of the inverse in the cache via the
##  solve() function.
cacheSolve <- function(x, ...) {
    ## Inputs: result from makeCacheMatrix, i.e. cacheSolve(makeCacheMatrix(mdat)) where mdat must be
    ##          square, invertible and non-singular

    ## Usage (for solve): given X is a square invertible matrix, then solve(X) returns its inverse.

    # get currently cached matrix inverse (may be NULL if inverse has not been run)
    inv <- x$getinverse()

    # check if inverse has been run prior, if so return result
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # if inverse has not been run, then get matrix data (by calling getter function)
    data <- x$get()

    # compute inverse using solve() function
    inv <- solve(data, ...)

    # commit result to cache
    x$setinverse(inv)

    # and return answer
    inv
}

