#Making catch Matrix
MakeCacheMatrik <- function(x = matrik()) {
      j <- NULL
      set <- function(y) {
            x <<- y
            j <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) j <<- inverse
      getinverse <- function() j
      list(setmatrix = set,
           getmatrix = get,
           setinversematrix = setinverse,
           getinversematrix = getinverse)
}
#checking for catch inverse before solving the inverse matrix
CacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}