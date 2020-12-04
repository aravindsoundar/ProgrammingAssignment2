## Caching the inverse of a Matrix


#function that caches its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        a <- NULL ##initialize a(inverse) as null
        set <- function(y) ##defines the set function
        {
        x <<- y ##value of matrix in parent enviironment
        a <<- NULL ##if there is a new matrix reset a to null
        }
        get <- function() x ##define the get fucntion returns value of the matrix argument
        
        setinverse <- function(inverse) a <<- inverse ##assigns value of a in parent environment
        getinverse <- function() a ##gets the value of inv where called
        list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse) ## creating a list in order to retrive them later.
}


#function the computes the inverse of the matrix returned by the above function

cacheSolve <- function(x, ...) ## Return a matrix that is the inverse of 'x'
        { 
        a <- x$getinverse() ##retriving the inverse
        if(!is.null(a)) ## checking is a in not null
        {
        message("getting cached data") ## lets know if the matrix is has been inversed before
        return(a)
        }
        data <- x$get() ## retrive matrix x
        a <- solve(data, ...)
        x$setinverse(a) ## set matrix a
        a
}
