## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix contains the following functions:
## set - a new matrix is created which sets the global variable x to the new matrix and invm to a 0 by 0
##       matrix to show that the inverse matrix has not been calculated yet
## get - returns the global variable x which is the matrix - which may or may not have an inverse yet
## setinv - sets global variable invm to the inverse matrix of x
## getinv - returns the inverted matrix which might still be an uncalculated inverse matrix of 0 by 0

makeCacheMatrix <- function(x = matrix()) {
  invm <- matrix(numeric(0), 0,0)   ## a matrix of 0 rows and columns is used to indicate that
  set <- function(y) {              ## the inverse has not been calculated yet  
    x <-- y
    invm <<- matrix(numeric(0), 0,0) 
  }
  
  get <- function() x
  setinv <- function(invmparm) invm <<- invmparm
  getinv <- function() invm
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve calculates the inverse o fthe matrix x only once.  If it is called again with the same
## matrix then it returns the cached value of the inverse of the matrix and does not calculate it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  if (!(nrow(invm) == 0) && (ncol(invm) == 0)){     ## if inverse exists then don't recalculate it
    message("getting cached data")
    return(invm)
  }
  ## need to calculate the inverse for the first time
  matrixtoinvert <- x$get()
  invm <- solve(matrixtoinvert)
  x$setinv(invm)
  invm
}
