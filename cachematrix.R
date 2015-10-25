## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## Set the value of the matrix 
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the solution for the matrix's inverse
  setsol <- function(solve) s <<- solve
  
  ## Get the solution for the matrix's inverse
  getsol <- function() s
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Check to see if the inverse has already been converted
  s <- x$getsol()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## Compute the inverse and set the solution in the cache
  data <- x$get()
  s <- solve(data, ...)
  x$setsol(s)
  s
}