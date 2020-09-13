## Two functions that can  cache the inverse of a matrix 

## The makeCacheMatrix function creates a matrix object that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                     ## inverse of the matrix initialized to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x                           ## to get the matrix x
  setinverse <- function(inverse) i <<- inverse   ## to set the inverse of the matrix to i
  getinverse <- function() i                       ## to get the inverse of x
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 

}


## The cacheSolve function computes the inverse of matrix returned by the makeCacheMatrix above 
##and retrieves the value from cache if the inverse has already been computed

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {        ## to check if the inverse is present in the cache
    message("getting cached data")
    return(i)
  }
  data <- x$get            ## reading the data from x
  i <- solve(data, ...)    ## to compute the inverse of matrix x
  x$setinverse(i)   
  i                        ## Returning the inverse matrix of 'x'
}
