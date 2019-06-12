## The functions below cache the inverse of a matrix. The makaeCacheMatrix creates a list with the related functions to 
## set and get the value of matrix and its inverse. This function stores the value of inverse when provided to the 
## setinverse function. The value of inverse can retrieved after than without calculating it again, until the value
## of matrix is changed. cacheSolve function is retrieving the value of cached inverse, if its not already cached its 
## value is calculated and cached for future retrieval

## makeCacheMatrix creates the functions to set and get the matrix and its inverse also stores the cached value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a comment describing this function, it retrives the cached inverse of the matrix, if the inverse
## is not cached then it calculated and set so that it can be cached for future retreival

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

