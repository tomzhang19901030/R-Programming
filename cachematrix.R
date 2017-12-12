## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCaheMatrix = function(x = matrix()){
        
        invmat = NULL                       ## Initialize Null Matrix
        setmat = function(y){               ## Define the set function to 
                X <<- y                     ## Assign now value of matrix
                invmat <<- NULL             ## if there is a new matrix, reset inv to null
        }
        getmat = function() x               ## define getmat function 
        setInverse = function(inverse) invmat <<- inverse   ## assigns value of inverse in parent environment
        getInverse = function() invmat                      ## gets the value of invmat
        list(setmat = setmat,
             getmat = getmat,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve = function(x,...){
        ## return a matrix that is the inverse of X
        invmat = x$getInverse()
        if (!is.null(invmat)){
                message("getting cached data")
                return(invmat)
        }
        data = x$get()
        invmat = solve(data,...)
        x$setInverse(invmat)
        invmat
}






