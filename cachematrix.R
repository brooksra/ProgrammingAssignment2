
## This set of 2 functions allows matrix inverses to be calculated and cached.  
## Once cached, the inverses can be returned without recalculation.

## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse. 
## Specifically, it:
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse
## gets the value of the inverse

makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function (solve) {inv <<- solve}
  getinv <- function () {inv}
  list (set = set, get = get, setinv = setinv, getinv = getinv)

}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null (inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}




