## 'makeCacheMatrix' is a list of 4 functions which can be used for 
## - setting the value of matrix
## - get the value of matrix
## - set the value of inverse of matrix
## - get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
mi <- NULL
set <- function(y){
  x <<- y
  mi <<- NULL
}
get <- function() x
setinv <- function(inv) mi <<- inv
getinv <- function() mi

list(setx = set, getx = get, setinv = setinv, getinv = getinv)

}


## 'cacheSolve' evaluates inverse of matrix created with 'makeCacheMatrix'
## - if the inverse has already been calculated then it gives the cached value
## - otherwise evaluates the inverse

cacheSolve <- function(x, ...) {
  
mi <- x$getinv()

if(!is.null(mi)) {
  message("getting cached inverse")
  return(mi)
}

input <- x$getx()
mi = solve(input, ...)
x$setinv(mi)
return(mi)

}


