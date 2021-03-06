## The following two functions can be used to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      getMat <- function() x  ##Function for getting matrix
      setMat <- function(y = matrix()){  ##Fuction for setting matrix
          x <<- y
          inv <<- NULL
      }
      getInv <- function() inv  ##Function for getting inverse
      setInv <- function(inverse) inv <- inverse   ##Function for setting inverse
      list(getMat = getMat, getInv = getInv, setMat = setMat, setInv = setInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, if the inverse is
## not already found in cache memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv
        if(!is.null(inv)){  
            message("Getting cached matrix...")
            return(inv)
        }
        inv <- solve(x)
        x$setInv(inv)
        inv
}
