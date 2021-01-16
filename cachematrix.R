##overall goal is to cache the inverse of a matrix

##arguments to create and set up the matrix and then extract its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ##get and set for the normal matrix before inverse
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ##return a list of the get and set inverse
}



## To solve for a matrix (the inverse) if it hasn't already been cached to save time and processing

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        ##loop to check for a previously cached matrix
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ##extract the data, solve for the matrix, extract the inverse, and return the new/solved matrix m
}