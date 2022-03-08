## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix consists of set, get, setinv, getinv
## library(MASS) is used to calculate the inverse of matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     #setting inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x   #function to get matrix x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver<-ginv(x)
    inver%*%x                   #to obtain the inverse of the matrix
  }             
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This is used to get the cache data

cachesol <- function(x, ...)  ##gets cache data
  {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}
