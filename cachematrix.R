## Put comments here that give an overall description of what your
## functions do

##Given a square matrix as input, x, the functions below solves for the matrix inverse, and stores the result in a special "matrix" object.
##If the matrix has already been solved, the previously calculated special matrix is returned without additional computation.
##

## Write a short comment describing this function
##The makeCacheMatrix function below recieves a square matrix, x, as input and returns a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function
##The cacheSolve function recieves the special matrix, x (returned by the makeCacheMatrix function) and checks to see if inverse of the special matrix
##has been calculated. If it has been calculated it returns the inverse, otherwise it calculates the inverse and stores it for later use.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
        
}
