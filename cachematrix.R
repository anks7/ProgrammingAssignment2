## Programming assignment - 2  
## Objective here is to write a pair of functions that cache the inverse of 
## a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }    
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  list_dat<-list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  # converting the list to a square matrix of size 2
  matrix(unlist(list_dat),nrow = 2,ncol = 2)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix() .
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #calling getinverse() function .
  m <- x[[2,2]]()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #calling get() function.
  data <- x[[2,1]]()
  m <- solve(data)
  #calling setinverse() function.
  x[[1,2]](m)
  m
}


