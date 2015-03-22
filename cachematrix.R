## Author: Intentionally left blank
## Version:
## Last Revision:
## Last Revision Comment:
## Description:
## The first function, makeCacheMatrix creates a special "matrix" 
##which is really a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of the matrix
##4.get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
   m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list (set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##The following function calculates the inverse of the special "matrix" 
##created with the above function. 
##However, it first checks to see if the inverse has already been calculated and the matrix has not changed. 
##If so, it gets the mean from the cache and skips the 
##computation. Otherwise, it calculates the mean of the data and 
##sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  m <- x$getinv()
  if(!is.null(m) & identical(x, x$get())) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
