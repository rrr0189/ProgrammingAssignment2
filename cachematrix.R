## Assume that all arguments of the functions below consist entirely
## of square matrices with nonzero determinant.
## Then makeCacheMatrix will create a custom matrix object 
## that can cache its inverse (using lexical scoping).
## cacheSolve will compute the custom matrix's inverse if it is new.
## It will retrieve the inverse from the cache otherwise.

## This function assumes that the argument is
## a square, invertible matrix. It then creates
## a custom matrix object capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(b) {
    x <<- as.matrix(b)
    i <- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)
}


## This function computes the inverse of the CacheMatrix
## if the CacheMatrix is new.
## Otherwise, it retrieves the inverse from the cache.

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
