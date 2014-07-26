
## makeCacheMatrix makes a matrix that stores functions that will be used by CacheSolve
##to use: x<-matrix(); makeCacheMatrix(x)
## 'x' can be a square or complex matrix

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve uses the solve() function to invert 'x' which can be a square or complex matrix
##logical matrices are coerced to numeric
## ?solve to get R article

cacheSolve <- function(x, ...) {
  
       
        
  
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}
