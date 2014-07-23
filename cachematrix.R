## Put comments here that give an overall description of what your
## functions do

######################################################################################################
# This function requires a non-singular square matrix as an input, or it will not operate correctly!!
# It creates the cache for storage of the calculated inverse matrix.  This allows for the inverse matrix
# calculation to only happen once, and then can be called from memory if it already exists. 


makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(y) {
                x <<- y
                matrix_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matrix_inv <<- solve
        getinv <- function() matrix_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
        
}

##########################################################################################################
# This function also requires a non-singular square matrix as an input, or it will not operate correctly!!
# After storing the input matrix into "x", it then checks to see if there is already an existing inverse 
# matrix for that input matrix. If there is one, it calls it from the cache and returns it to the parent 
# function.
# If there is not a pre-cached inverse matrix for the input matrix "x", it then calculates one and stores 
# it into cache for future calls ("matrix_inv"), without the need for inverse recalculation.
# My function also calculates the inverse of "x" using an auto-generated identity matrix 
# (of matching dimenstions), derived from the square root of the lenth operator on the input matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inv <- x$getinv()
        if(!is.null(matrix_inv)) {
                message("getting cached data")
                return(matrix_inv)
        }
        data <- x$getinv()
        matrix_identity<-diag(sqrt(length(data))) ##Auto-generated identity matrix for solve function
        matrix_inv <- solve(data,matrix_identity, ...)
        x$setinv(matrix_inv)
        matrix_inv
}

