#Creates a list of functions that can cache the inverse of a matrix
#The object does not calculate the inverse, just saves it inside but
#saves the matrix to variable x and its inverse to variable s in scope.

#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
   m <- NULL
   set <- function(y){
   x <<- y
   m <<- NULL
}
   get <- function() x
   setmatrix <- function(solve) m <<- solve
   getmatrix <- function() m
   list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

#Function  computes the inverse of the matrix returned by makeCacheMatrix.
#Takes the object of that type as an argument 'x', checks if the inverse value is already
#cached, and if it is returns the cached value; if not, this function calculates the
#inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
#and returns the result.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
 }
