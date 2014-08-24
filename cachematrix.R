## MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  i for inverse of X


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
         x <<- y
        i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        message("Wait. I am getting cached data")
        return(i)
  } 
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        #returns inverse of x
}
