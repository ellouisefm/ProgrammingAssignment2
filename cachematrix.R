#### makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
  m <- NULL
  
  ### Set the values of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the values of the matrix
  get <- function() x
  ## Set the values of the matrix inverse
  setinverse <- function(solve) m <<- solve
  ## Get the values of the matrix inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#### cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Checks if inverse was already computed, 
  ## if yes it skips and return cached inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If  inverse not yet calculated, 
  ## inverse is solved and then cached     
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
