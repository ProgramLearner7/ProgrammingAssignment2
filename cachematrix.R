## The following functions computes inverse of a square matrix. The results can be stored and retrieved from the cache.

## makeCacheMatrix function calculates the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        
        getMatrix = function() x
        setInversion = function (x) m <<- solve(x)
        getInversion = function() m
        
        list(getMatrix = getMatrix,
             setInversion = setInversion,
             getInversion = getInversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m = x$getInversion()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        SourceMatrix = x$getMatrix()
        m = x$setInversion(SourceMatrix)
        m
}

