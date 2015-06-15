#makeCacheMatrix creates a list containing a function to 
#set the value of the matrix ($set)
#get the value of the matrix ($get)
#set the value of the inverse matrix ($setinverse)
#get the value of the inverse matrix ($getinverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#The function cacheSolve first checks to see 
#if the inverse has already been calculated ($getinverse is not NULL).
#If so, it gets the inverse matrix from the cache and skips the computation
#Otherwise, it calculates the inverse of the matrix 
#and sets the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
