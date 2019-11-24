## MakeCacheMatrix creates a list of functions that perform certain operations on a 
## defined matrix. cacheSolve attempts to either invert the defined matrix OR pull it out of a 
## saved cache environment in makeCacheMatrix 

## This will get, set the matrix or get/set the matrix's inversion
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## This will attempt to extract the cached inversion of the defined matrix or solve it and set it 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Return a matrix that is the inverse of 'x'TEST DRIVE
a <- makeCacheMatrix(matrix(rnorm(20),2,2)) #Make the Cache
cacheSolve(a) #Solve
a$getinv()

