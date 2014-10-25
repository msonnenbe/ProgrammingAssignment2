## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list( set=set, get=get, setinv=setinv, getinv=getinv)
}


#The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  sol<-x$getinv()
  if(!is.null(sol)){
    message("getting cached inverse")
    return(sol)
  }
  data <- x$get()
  sol <- solve(x, ...)
  x$setinv(sol)
  sol
}
