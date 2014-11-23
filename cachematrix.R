##    This program  will compute the inverse of a square matrix. In case the inverse
##    has already been calculated, the inverse of the matrix will be retrived from 
##    the cache. It consists of two functions: 

##    1.  `makeCacheMatrix`: This function creates a special "matrix" object
##         that can cache its inverse.
##    2.  `cacheSolve`: This function computes the inverse of the special
##         "matrix" returned by `makeCacheMatrix` above. If the inverse has
##         already been calculated (and the matrix has not changed), then
##         `cacheSolve` should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        myinverse <- NULL
        set <- function(y) {
                x <<- y
                myinverse <<- NULL
        }
        get <- function() x
        setiv <- function(inverse) myinverse <<- inverse
        getiv <- function() myinverse
        list(set = set, get = get,
             setiv = setiv,
             getiv = getiv)
}


cacheSolve <- function(x, ...) {
        myinverse <- x$getiv()
        if(!is.null(myinverse)) {
                message("getting cached data")
                return(myinverse)
        }
        data <- x$get()
        myinverse <- solve(data, ...)
        x$setiv(myinverse)
        myinverse
}
