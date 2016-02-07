## Caching the Inverse of a Matrix
## Matrix inversion is often costly computationally, hence, it would be
## beneficial to store the cache of the inverse of a matrix versus always
## computing it again.  We have created 2 functions, one that creates an 
## object that stores a matrix and then a second function to compute
## the inverse of the matrix returned by the 1st function.  If the inverse
## has already been calculated, then the function will simply retrive
## the inverse from the cache

## This function creates a special "matrix" object that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
        inver<- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## The 2nd function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix above.  If the inverse has already been 
## calculated and the matrix has not changed, then this 2nd function should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}

