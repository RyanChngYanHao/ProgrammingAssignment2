## A pair of functions capable of caching a matrix's inverse.

## makeCacheMatrix is a function.
### When the input is a matrix, it creates an environment of 
### 4 functions and 2 matrices(x and its inverse) capable of caching x inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve is a function.
### When the input is an end-product from makeCacheMatrix,
### it checks whether x's inverse was already stored.
### If x's inverse was already stored, it will return i (which is x's inverse).
### If x's inverse is not yet stored, it will pull x from x$get 
### of makeCacheMatrix and compute the inverse by solve(x), 
### set the inverse in makeCacheMatrix then return i (which is x's inverse).


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
