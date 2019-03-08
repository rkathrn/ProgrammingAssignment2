## Overall -- Enables user to store a matrix and its inverse in a cache, structured as a list object.
## First function creates cache, and sub-functions (list items) allow user to store matrix & inverse in that cache.
## Second function will call the matrix inverse, checking the cache first to see if data already exists.
## If the inverse is not already stored in the cache, the second function will compute it, and then store it in the cache. 

## makeCacheMatrix -- uses matrix as an input to create a 'cache' as a list of four functions
## that enable user to set and have returned the matrix and its inverse. 
## Store result to a separate variable  to use correctly.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ ## sets/stores the matrix
                x <<- y
                inv <<- NULL
}
        get <- function() x ## returns the matrix set in the 'set' subfunction
        setinverse <- function(inverse) inv <<- inverse ## sets/stores the matrix inverse
        getinverse <- function() inv ## returns the matrix inverse stored in 'setinverse' subfunction
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse) 
        }

## cacheSolve - using the variable where you've stored your matrix cache
## cacheSolve will check if there is an inverse stored in the cache already. If there is no inverse 
## set, function will calculate the inverse, store it in the cache, and print it.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## storing the 'getinverse' aspect of the cache in the first function
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) ##if there isn't a stored inverse in the cache, returns a message saying 'getting cached data'
        }
        matrix <- x$get() ##first step of calculation - storing cached standard matrix in a new variable
        inv <- solve(matrix, ...) ##calculating inverse of matrix from the new matrix variable and storing it in 'inv'
        x$setinverse(inv) ##taking the calculated inverse and storing it in the cache - in the 'setinverse' subfunction
        inv ##printing the inverse matrix
}
