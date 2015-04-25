## -----Caching the Inverse of a Matrix-----
## Since matrix inversion is usually a costly computation,  
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly.  
##
## Here are a pair of functions that cache the inverse of a matrix
## and return the cached value if already calculated.

## The following functions are included to accomplish this:
##  
##  makeCacheMatrix: can cache its inverse.
##  cacheSolve: computes the inverse or retrieves the inverse from the cache.


##  makeCacheMatrix: This function creates a special "matrix" 
##    object that can cache its inverse and is really a list
##    containing functions to:
##
##    1) set the value of the matrix
##    2) get the value of the matrix
##    3) set the value of the inverse
##    4) get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {                  
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##  cacheSolve: This function computes the inverse of the special 
##    "matrix" returned by makeCacheMatrix above. If the inverse 
##     as already been calculated (and the matrix has not changed), 
##    then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                                        #Return a matrix that is the inverse of 'x' or the cached inverse 
  inv <- x$getinverse()
  if(!is.null(inv)) {
#    message("getting cached data")     #Print out for diagnostic perposes only
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)               #Calculate new inverse and then cache
  x$setinverse(inv)
  inv
}

