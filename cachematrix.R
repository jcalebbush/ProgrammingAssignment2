## The function below take a created matrix and store it into memory.  It also checks to see if the matrix has been
## solved and cached into memory.  If the matrix inverse is not cached, then cacheSolve solvees the matrix for its
##inverse.

## This function is called to check to see if the matrix to be solved exists currently and if it's inverse has been
## solved and cached.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the matrix that was created in the makeCacheMatrix function has a cached solution
## (inverse).  If it does not, then the function solves the matrix for its inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
  }
