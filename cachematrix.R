## The makeCacheMatrix takes as input a matrix and returns a list object which
## contains 4 functions (setters and getters) useful to either initialize or 
## retrieve, a matrix or a cached inverse of an already existing matrix

## The cacheSolve takes as input the list object from the above function and 
## returns an already cached inverse of a matrix or a newly calculated inverse.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set the value of x and initialize the value of inverse to NULL
        set <- function(y) {
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


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # if cached inverse exists, return the same. Otherwise, calculate the 
        # inverse for the new matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
