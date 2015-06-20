## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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


## Write a short comment describing this function

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
