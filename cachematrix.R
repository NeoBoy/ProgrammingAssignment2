## makeCacheMatrix creates a special matrix object
## cacheSolve finds the inverse of the special matrix 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.


## It will create the special matrix object for which inverse is to be calculated

makeCacheMatrix <- function(x = matrix()) {
	xInv <- NULL
    set <- function(y) {
            x <<- y
            xInv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) xInv <<- inverse
        getInv <- function() xInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## It will solve to find the inverse of the matrix
## but first will check if cached inverse is available or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		xInv <- x$getInv()
		
		## if cache inverse exists, retrieve that and return
        if(!is.null(xInv)) {
            message("Cached Matrix Inverse Available")
            return(xInv)
        }
		
		## Otherwise solve to find the inverse and return that
        data <- x$get()
        xInv <- solve(data, ...)
        x$setInv(xInv)
        xInv
}
