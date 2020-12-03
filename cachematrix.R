## Caching the inverse of a Matrix


#function that caches its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        a <- NULL
        set <- function(y)
        {
        x <<- y
        a <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) a <<- inverse
        getinverse <- function() a
        list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


#function the computes the inverse of the matrix returned by the above function

cacheSolve <- function(x, ...) {
        a <- x$getinverse()
        if(!is.null(a))
        {
        message("getting cached data")
        return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        a
}
