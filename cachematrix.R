## Put comments here that give an overall description of what your
## functions do

## this function returns a list that has 4 functions:
##  set - "stores" a matrix 
##  get - returns the matrix stored by set
##  setinverse - calculates the inverse of the  

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) i <<- solve
    
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




## Function that returns (a) previously solved and cached inverse of a matrix
##  or (b) newly calculated inverse if (a) has not prevously been calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
