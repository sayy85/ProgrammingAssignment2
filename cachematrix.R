## The file contains two functions: makeCacheMatrix and cacheSolve
## The aim of those functions is to calculate the inverse of a matrix,
## cache calculated inverse, return the cached inverse matrix



## makeCacheMatrix creates a special "vector", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL 
   set <- function(y) {
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setinv <- function(solve) inv <<- solve
   getinv <- function() inv
  
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}



## cacheSolve calculates inverse of a matrix. First it checks whether the inverse
## has already been calculated, if so - it returns the cached inverse matrix, 
## otherwise it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
  
}
