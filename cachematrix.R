##
## It can be expensive to invert a large matrix.  Therefore, we keep a copy 
## in cache and, if the cache is populated, use the cached copy.  
##
##  This assignment is divided into two halves.  The first half initializes 
##  the environment and defines the functions used in the second half.  It 
##  returns the four functions as a list.  
##  The second half takes the matrix to be inverted, looks to see whether
##  cache is populated.  If it is, it returns the cached value.  
##

makeCacheMatrix <- function(x = matrix()) {
{
##

## First initialize the environent
##
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
##
##	Now define the four functions needed to calculate the inverst
##
        get <- function() x
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
}


## This function looks to see whether the inverse of the matrix is in cache.  
## If so, it uses the cached copy.  If not, it calculates it with the solve function.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
##
##	Do we have a precalculated inverse?  If so se it.
##
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
##
##	If not, calculate it.
##
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
}
