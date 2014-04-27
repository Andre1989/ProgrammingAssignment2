## This program computes and caches the inverse of a matrix. This avoids
## R to perform potentially time-consuming computations when they are not
## necessary.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of 4 functions: 1 - set(): sets the values of the matrix; 
## 2 - get(): gets the matrix; 3 - setInv(): stores the inverse matrix; 
## 4 - getInv(): gets the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL          ## At the beggining, we have no inverse 
        set <- function(y) {          ## Sets a new original matrix and deletes old inverses
                x <<- y               ## Since the inverse was deleted, this assures no inverses
                inv <<- NULL          ## are cached with non-correponding original matrices
        }
        get <- function() x         ## Returns the original matrix
        setInv <- function(inverse) inv <<- inverse       ## Sets the inverse
        getInv <- function() inv          ## Returns the inverse
        list(set = set, get = get,         ## List with the 4 functions to return
             setInv = setInv,
             getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
inv <-x$getInv()          ## Gets the inverse matrix from the cache
if (!is.null(inv)){          ## If the inverse was taken from the cache, returns it and exits the function
        message("getting cached data")
        return(inv)
}
data <- x$get()          ## If there is no inverse in the cache, gets the original matrix
inv <- solve(data,...)   ## computes its inverse
x$setInv(inv)            ## and stores it in the cache
inv
}
