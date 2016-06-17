## makeCacheMatrix: creates list of methods that gets and sets a matrix inverse
## in a seperate enviroment.
## cacheSolve: If an inverse is set from the first function it will leave it be,
## if not, it will calculate the inverse.


## Function creates a matrix x, with the possibility of having the inverse cached.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The returned special matrix from makeCacheMatrix will be inverted if the
## inverse has not been calculated. Otherwise, it will retrive inverse from
## the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setInv(inv)
  inv
}
