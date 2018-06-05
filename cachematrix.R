## These functions  are for caching the inverse of a matrix rather than 
## compute it repeatedly 

## This function creates a special matrix that can cache its inverse.
## The special matrix contains functions to set and get the values of the
## matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<-NULL
  }
  get<- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special matrix created with
## the makeCacheMatrix function.  If it was already computed, then it 
## gets the inverse from the cache and skips the computations. Otherwise,
## it computes the inverse matrix and sets it in the cache via the setinv
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
