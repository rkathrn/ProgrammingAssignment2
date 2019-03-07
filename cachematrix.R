## Put comments here that give an overall description of what your
## functions do

## Creates list that can house matrix input and an ALREADY CALCULATED inverse, and return them.

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

## Takes the matrix stored through the previous function, tries to calculate the inverse.
## If the inverse hasn't been set through makeCachematrix, then this function will calculate the inverse and return it.

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
