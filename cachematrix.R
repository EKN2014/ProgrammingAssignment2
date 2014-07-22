# This function creates a special "matrix" object that can cache its inverse
# The object does not calculate the inverse, just saves it inside. It
# saves the matrix to variable x and its inverse to variable m in scope.

#This is how the function works
   #1. set the value of the matrix
   #2. get the value of the matrix
   #3. set the value of the inverse
   #4. get the value of the inverse

makeCacheMatrix <- function(x=matrix()) {
   m <- NULL
   set <- function(y){
   x <<- y
   m <<- NULL
}
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}

# This function computes the inverse of the matrix returned by makeCacheMatrix function.
# It first checks if the inverse has already been calculated. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the inverse of the data
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
 }
