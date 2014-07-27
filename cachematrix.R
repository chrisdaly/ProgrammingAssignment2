## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverse variable
  inv <- NULL
  
  # set a new value for the matrix, this invalidates the cache
  set <- function(y) {
    x <<- y
    
    # y is a new matrix with its own inverse, so clear old inverse
    inv <<- NULL
  }
  
  # returns the matrix
  get <- function(){
    x
  }
  
  # sets the value of the inverse
  setinv <- function(inv_new){
    inv <<- inv_new
  }
  
  # returns the inverse
  getinv <- function(){
    inv
  }
  
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Checks the matrix object for a cached inverse, otherwise
## computes the matrix's inverse and stores it

cacheSolve <- function(x, ...) {
  
  # check the matrix object for a cached inverse
  inv <- x$getinv()
  
  # if there is a cached inverse
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
    
  # retrieve the matrix
  data <- x$get()
  
  # calculate the inverse
  inv <- solve(x)
  
  # set the inverse in the matrix object
  x$setinv(inv)
  
  inv
  }
}
