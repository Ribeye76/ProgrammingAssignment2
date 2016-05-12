## makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse.
## The outcome is a list of four functions:
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## set() set the value of the matrix
        get <- function() x
        ## get() get the value of the matrix        
        setInv <- function(inverse) inv <<- inverse
        ## setInv() set the value of the inverse matrix to cache
        getInv <- function() inv
        ## getInv() get the value of the inverse matrix
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
        ## list() return this four functions to the working environment
        ## This list is used as the input to cacheSolve()
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        ## Returns a matrix inv that is the inverse of 'x' through function getInv()
        if (!is.null(inv)) {
        ## If such matrix is not NULL
                message("getting cached data")
                ## Displays message "getting cached data"
                return(inv)
                ## Returns the inverse
        }
        ## Otherwise
        mat <- x$get()
        inv <- solve(mat, ...)
        ## Calculates the inverse of x
        x$setInv(inv)
        ## Uses setInv() to set its value to cache
        inv
        ## Returns it
}
