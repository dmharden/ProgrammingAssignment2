
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL 
  evn <- environment()
  y <- NULL 
  
  setmatrix <- function(y){ 
    x <<- y
    m <<- NULL
  } 
  
  getmatrix <- function() x
  setinverse <- function(solve) {m <<- solve}
  getinverse <-function() {m}
  getenv <- function() environment()
  
  
  list (setmatrix = setmatrix, getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)){
      message("getting cached data")
      return(m) 
    }
    y <- x$getmatrix()
    m <- solve(y, ...)
    x$setinverse(m)
    m
  }
