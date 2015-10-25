## makeCacheMatix is an object that stores the matrix and its inverse in an environment that is different from the current 
## environment. cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix and stores in special
## in a different environment and returns the computed inverse matrix. If the value is already 
## calculated and cached then the inverve is retrieved from the cache.



## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
      i <- NULL
      
      set <- function(y) {
        x <<-y
        i <<- NULL
      }
     
      get <- function() x
      setinverse <- function(s) { i <<- s}
      getinverse <- function() i
      
      #create the special object and return it
      list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

  i <- x$getinverse()
  
  if(!is.null(i)) {
    ## Return a matrix that is the inverse of 'x'
    return(i)
  }
  
  data <- x$get()
  
  ## calculate invers of x
  i <- solve(data, ...)
  
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
