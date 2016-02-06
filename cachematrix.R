##
##    Programming assignment number 2 from Coursera R Programming course
##
##    Assignment: Caching the Inverse of a Matrix
##    Write a pair of functions that cache the inverse of a matrix.
##


## A function that creates a matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
    ## Private fields
    inv <- NULL

    ## Getters and setters
    get <- function() {
        return(x)
    }

    set <- function(y){
        x <<- y
        inv <<- NULL
    }

    getInverse <- function() inv
    
    setInverse <- function(inverse){
        inv <<- inverse
    }

    ## Return the list
    list(get = get,
         set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}


##  A function that returns the cached inverse of a matrix if such exists;
##  otherwise calculates the inverse and caches it for future use.
cacheSolve <- function(x, ...) {
    ## Check if the inverse is already calculated and cached
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("Returning the cached inverse.")
        return(inv)
    }

    ## At this point the inverse is NULL; calculate and cache it
    m <- x$get()
    inv <- solve(m, ...)
    x$setInverse(inv)

    ## Return the inverse
    inv
}
