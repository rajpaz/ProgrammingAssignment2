## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix defines a special matrix with operations to set and get
## value of the matrix as well as get and set the inverse of the matrix.
## In addition, the function caches the value of the matrix and its inverse

## Write a short comment describing this function

# makeCacheMatrix provides fucntions for setting, getting the value of matrix
# as well as for setting, getting value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL # initial the inverse to null, so that it can be checked for existence
  }
  get <- function() x
  setinv <- function(matrixInv) inv <<- matrixInv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


# This function will retrieve the cached inverse, if it exists
# if the cached inverse does not exist then it will compute the inverse 
# and cache it for future retrieval using the setinv function

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # do not compute the inverse if cached version exists
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # compute the inverse when it does not exist in cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv) # cache the inverse
  inv # return the inverse
}
