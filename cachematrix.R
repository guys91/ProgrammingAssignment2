## These functions have the purpose of implementing an efficient way to calculate
## and cache the value of the inverse of a given matrix.

## The makeCacheMatrix function returns an object of type makeCacheMatrix, containing a list of
## the functions set, get, setinverse and getinverse. When called, the value passed
## as an argument is automatically set as x.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function requires as an argument a makeCacheMatric object, checks if the 
## inverse of the matrix x has already been calculated and therefore cached. If
## the value is cached, it returns it, otherwise it executes the solve() function,
## calculates the inverse of matrix x, sets the result in the makeCacheMatrix 
## object and returns it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
