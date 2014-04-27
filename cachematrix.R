## Creates an object that expects a response from the cache if it has already done
## calculate the result is displayed by the object created by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {

    invMatrix <- NULL
    set <- function(y) {
            x <<- y
            invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) invMatrix <<- inv
    getInverse <- function() invMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculates the inverse of the matrix if you do not find the result in the cache
cacheSolve <- function(x, ...) {

    if (!is.null(invMatrix)) {
            return(invMatrix)
    } else {
            inv_x <- solve(x$get())
            x$setInverse(invMatrix)
            return(invMatrix)
    }
}
