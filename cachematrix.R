## Together these two functions can receive a matrix as an input, calculate the inverse of that
## matrix, and cache that inverse matrix for later use. This enables the function user to avoid computing
## the inverse matrix each time they want to use it


##Description: makeCacheMatrix initializes the following functions: set,get,setinverse, and getinverse
## It also initializes the argument x as a matrix and m as NULL. Once a matrix is entered as the
## argument of makeCacheMatrix, and object of type makeCacheMatrix is formed. ##once the object is passed
## to the cacheSolve function, m is assigned the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Description: cacheSolve takes an object of type makeCacheMatrix as an argument, and checks whether there is
## already a cached inverse matrix in memory. If no inverse is cached, cacheSolve will calculate the inverse
## matrix and store it in memory. If there is already a cached inverse, cacheSolve will return its value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m 
}