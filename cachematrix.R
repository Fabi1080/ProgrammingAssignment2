## Functions to create a matrix and cache its inversion.
##
## Example usage:
##    1. create a matrix with the makeCacheMatrix function, eg:
##     # matrix = makeCacheMatrix( matrix(c(1, 2, 3, 4), 2, 2))
##    2. calculate the inverse for the first time, this caches its inversion
##     # inverseMatrix = cacheSolve(matrix)
##    3. Run step 2. again, this time it will return the cached inversion and therefore run faster


## This function creates the matrix object with its cache

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  
  set <- function(mat) {
    x <<- mat
    inverse_m <<- NULL
  }
  get <- function()
    x
  
  getinverse <- function()
    inverse_m
  setinverse <- function(inverse)
    inverse_m <<- inverse
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function calculates the inverse of the matrix and stores it in the cache if that has not been done before, otherwise it will get the inverse matrix from the cache and return that

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    #message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
