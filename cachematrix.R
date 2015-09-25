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
##  For testing purposes only, validating that the code works.                  ##
##  The objects mat1 and mat2 are matrices.  The objects a and b are lists of   ##
##  functions associated with setting and retrieving the inverses of mat1 and   ##
##  mat2, respectively.  When cacheSolve is run on a, the inverse of mat1 is    ##
##  calculated, cached, and returned.  When cacheSolve is run on a again, the   ##
##  cached value of mat1's inverse is returned instead of recalculating it.     ##
##                                                                              ##
##  The same is done for mat2 with object b, without any confusion between the  ##
##  two sets of functions.                                                      ##
##                                                                              ##
##  Finally, object a is directly reset to utilize mat1 instead of mat2.  In so ##
##  doing, the cache is effectively cleared from object a and it must calculate ##
##  the inverse of mat2.                                                        ##
##################################################################################

mat1<-matrix(sample(1:100, 9),  nrow=3, ncol=3);mat1 # Initial matrix 1, then displays it.
mat2<-matrix(sample(1:100, 25), nrow=5, ncol=5);mat2 # Initial matrix 2, then displays it.

a<-makeCacheMatrix(mat1) # List of functions to calculate, store, and retrieve inverse of mat1.
b<-makeCacheMatrix(mat2) # List of functions to calculate, store, and retrieve inverse of mat2.

a$get()       # Returns mat1.
b$get()       # Returns mat2.

a$getsolve()  # Returns null since mat1 has not been inverted yet.  Nothing is available to "get".
cacheSolve(a) # Inverts mat1 since it wasn't previously calculated, caches the result, and returns that result.
cacheSolve(a) # Returns the cached result of inverting mat1 since it was previously calculated.
a$getsolve()  # Returns the cached result of inverting mat1 since it is now there to "get".

b$getsolve()  # Returns null since mat2 has not been inverted yet.  Nothing is available to "get".
cacheSolve(b) # Inverts mat2 since it wasn't previously calculated, caches the result, and returns that result.
cacheSolve(b) # Returns the cached result of inverting mat2 since it was previously calculated.
b$getsolve()  # Returns the cached result of inverting mat2 since it is now there to "get".

              # Note that the results for mat1 and mat2 do not interfere with one another.

a$set(mat2)   # Resets object a to utilize mat2 instead of mat1.
a$getsolve()  # Even though this previously returned the cached result of inverting mat1, we just reset object a to utilize mat2.
              #         As a result, since an inverse hasn't been calculated within object a for mat2, null is returned.
cacheSolve(a) # Inverts mat2 since it wasn't previously calculated within object a, caches the result, and returns that result.
cacheSolve(a) # Returns the cached result of inverting mat2 since it was previously calculated.
a$getsolve()  # This returned a null after resetting, above.  Now returns the cached result of inverting mat2 since it is now there to "get".

rm(a,b,mat1,mat2)