## These two functions take a matrix, and create a matrix-type object that includes
## the matrix, and a copy of the matrix inverse (provided that is has been calculated)


## The first function creates the matrix-type object.

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


## The second function checks to see if the matrix inverse has already been created, and if it
## has not, then it calculates the inverse. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m 
}
