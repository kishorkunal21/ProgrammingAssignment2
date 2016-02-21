## The following functions can compute and cache the inverse of a matrix

## makeCacheMatrix : creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
  ##         this list is used as the input to cacheSolve()
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve : computes the inverse of the "matrix" returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## x: output of makeCacheMatrix()
    inv = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
      # get it from the cache and skips the computation. 
      message("getting cached data")
      return(inv)
    }
    
    # else, calculate the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    x$setinv(inv)
    return(inv)## return: inverse of the original matrix input to makeCacheMatrix()
}
