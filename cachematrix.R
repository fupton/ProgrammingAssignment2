## These functions create the inverse of a matrix
## and look for a stored inverse of the matrix if its already cached

## this function creates elements of the cache solve. 
## it also gets creates the chache
makeCacheMatrix <- function(x = matrix()){
  i_1 <- NULL
  set <- function(y) {
    x <<- y
    i_1 <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i_1 <<- solve
  getinverse <- function() i_1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function checks whether the inverse has already been solved (cached)
## if it hasn't it solves for it.

cacheSolve <- function(x, ...){
  i_1 <- x$getinverse()
  if(!is.null(i_1)) {
    message("getting cached data")
    return(i_1)
  }
  data <- x$get()
  i_1 <- solve(data, ...)
  x$setinverse(i_1)
  i_1
}
