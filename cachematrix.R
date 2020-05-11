#using four functions
#1. set to set the matrix
#2. get to get the matrix
#3. setinverse to set the inverse of the matrix
#4. getinvetse to get the inverse of the matrix

#Making catch Matrix
MakeCacheMatrik <- function(x = matrik()) {
      inve <- NULL
      set <- function(y) {
            x <<- y
            inve <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inve <<- inverse
      getinverse <- function() inve
      list(setmatrix = set,
           getmatrix = get,
           setinversematrix = setinverse,
           getinversematrix = getinverse)
}
#checking for catch inverse before solving the inverse matrix
CacheSolve <- function(x, ...) {
      inve <- x$getinverse()
      if (!is.null(inve)) {
            message("getting cached data")
            return(inve)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(inve)
      inve
}
