## The makeCacheMatrix function sets the value
## of a matrix, gets the value of a matrix,
## sets the value of the inverse, and gets the
## value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set<- function(y){
    x<<- y
    i<<- NULL
  }
  get<- function()x
  set inverse <- function(inverse) i <<- inverse
  get inverse <- function() i
  list(set= set, get=get, 
     setinverse= setinverse,
     getinverse= getinverse)
}


## The cacheSolve function checks to see if
## the inverse of the matrix has already
## been calculated. If so, it prints
## "getting cached inverse" commnt and 
## prints the inverse while skipping over
## the calculation, as necessary. If not,
## it calculates the inverse of the matrix
## and sets the cached value. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
