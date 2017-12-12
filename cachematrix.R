## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCaheMatrix = function(x = matrix()){
        
        invmat = NULL
        setmat = function(y){
                X <<- y
                invmat <<- NULL
        }
        getmat = function() x
        setInverse = function(inverse) invmat <<- inverse
        getInverse = function() invmat
        list(setmat = setmat,
             getmat = getmat,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                          ## Initialize Null Matrix
        set <- function(y) {                 ## Define the set function to 
                x <<- y                      ## Assign now value of matrix 
                inv <<- NULL                 ## if there is a new matrix, reset inv to null
        }
        get <- function() x                  ## define get function 
        setInverse <- function(inverse) inv <<- inverse  ## assigns value of inverse in parent environment
        getInverse <- function() inv                     ## gets the value of inv
        list(set = set,
             get = get,
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






# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }

# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }



