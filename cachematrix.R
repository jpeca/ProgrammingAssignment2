## Write a pair of functions that cache the inverse of a matrix.
## Assume that the matrix supplied is always invertible
## R Programming Course Assignment 2 

## This function creates a list object with function to cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(m) {
    x <<- m
    minv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) minv <<- solve
  
  getinv <- function() minv
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
   
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  t <- x$getinv()
  if (!is.null(t)){
    message("getting cached data")
    return(t)
  }
  t <- solve(x$get(),...)
  m$setinv(t)
  t
  ## Return a matrix that is the inverse of 'x'
}
