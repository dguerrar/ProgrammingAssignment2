## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## takes a matrix in a paramater
## with getter and setter methods
## m is the inverse
## x is the matrix data

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


## Write a short comment describing this function

## takes x as an argument and it is the matrix data
## if matrix is new (change from last or new new), inverse is calculated
## if no changes, return inverse
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m) ) {
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
