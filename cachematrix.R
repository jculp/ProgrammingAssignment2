##################################################################################
##  Repeated calculations can be costly when performed on large quantities of   ##
##  data.  The pair of functions below can be used to reduce the computing time ##
##  in such situations by caching the inverse of a matrix such that it may be   ##
##  referenced later without repeating the calculation.                         ##
##################################################################################




##################################################################################
##  The first function is makeCacheMatrix(), defined below, which returns a     ##
##  list of additional functions when called on a matrix.  The additional       ##
##  functions returned will:                                                    ##
##      1) set the value of the matrix,                                         ##
##      2) get the value of the matrix,                                         ##
##      3) set the value of the inverse of that matrix, and                     ##
##      4) get the value of the inverse of that matrix                          ##
##################################################################################

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##################################################################################
##  The second function is cacheSolve(), defined below, calculates the inverse  ##
##  of the matrix passed to makeCacheMatrix.  If the inverse to that matrix has ##
##  already been calculated, the function gets that inverse from the cache.  If ##
##  the inverse to that matrix has not already been calculated, the function    ##
##  calculates it directly and stores it in the cache for future reference.     ##
##################################################################################

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}




##################################################################################
##  For testing purposes only, validating that the code works.  The object mat  ##
##  is a matrix.  The object a is the list of functions associated with setting ##
##  and retrieving the inverse of that matrix.  When cacheSolve is run on a,    ## 
##  the inverse of the original matrix is calculated, cached, and returned.     ##
##  When cacheSolve is run on a again, the cached value of the matrix inverse   ##
##  is returned instead of recalculating that inverse.  Finally, the output of  ##
##  cacheSolve is multiplied by the initial matrix to produce the identity      ##
##  matrix, verifying that the inverse is correct.                              ##
##################################################################################

mat<-matrix(1:4, nrow=2, ncol=2) # Initial matrix

a<-makeCacheMatrix(mat) # List of functions to calculate, store, and retrieve matrix inverse.

cacheSolve(a) # Calculates matrix inverse.
cacheSolve(a) # Retrieves cached matrix inverse.

cacheSolve(a) %*% mat # Verifies inverse.