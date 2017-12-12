

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

cacheSolve = function(x,...){
        
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



