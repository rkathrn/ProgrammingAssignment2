## Enables user to store a matrix and its inverse in
## a cache - structured as a list object. Then, using second function,
## user can call out the matrix inverse from the cache - if the inverse
## does not already exist in the cache, the second function
## will calculate the inverse and store it in the original cache list object.

## makeCacheMatrix - uses matrix as an input to create a 'cache' as a list of four functions
## that enable user to set and have returned the matrix and its inverse. 
## Store result to a separate variable  to use correctly.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
}
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        }

## cacheSolve - using the variable where you've stored your matrix cache
## cacheSolve will check if there is an inverse stored in the cache already. If there is no inverse 
## set, function will calculate the inverse, store it in the cache, and print it.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
