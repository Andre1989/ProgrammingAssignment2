## This program computes and caches the inverse of a matrix. This avoids
## R to perform potentially time-consuming computations when they are not
## necessary.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of 4 functions: 1 - set(): sets the values of the matrix; 
## 2 - get(): gets the matrix; 3 - setInv(): stores the inverse matrix; 
## 4 - getInv(): gets the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
inv <-x$getInv()
if (!is.null(inv)){
        message("getting cached data")
        return(inv)
}
data <- x$get()
inv <- solve(data,...)
x$setInv(inv)
inv
}
