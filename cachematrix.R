## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## takes a matrix in a paramater
## with getter and setter methods
## m is the inverse
## x is the matrix data

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		isNew<-FALSE
        set <- function(y) {
                x <<- y
                m <<- NULL
				isNew<<-TRUE
				message("setting new data to matrix")
        }
        get <- function() x

		
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
		
		getisnew <- function() isNew
		setisnew <- function(valor) isNew <<- valor

        list(set = set, get = get,
             setisnew = setisnew,
			 getisnew = getisnew,
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
		isnew<-x$getisnew()
        if(!is.null(m) && isnew==FALSE) {
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
		x$setisnew(FALSE)
        m
}

##in order to make som test
##v<-matrix(nrow=2,ncol=2)
##v[1,]<-c(1,2)
##v[2,]<-c(2,2)
##x<-makeCacheMatrix(v)
##cacheSolve(x)


##change data
##v[2,]<-c(4,4)
##x$set(v)
##cacheSolve(x)