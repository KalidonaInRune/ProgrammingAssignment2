## Set of functions to calculate the inverse of a matrix and then cache it for use later.

## Function to create the special matrix object that will set and get the value of the Matrix
## and then set and get the value of the inverse Matrix.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL }
  get <- function() x
  setInverse <- function(solveMatrix) invx <<- solveMatrix
  getInverse <- function() invx
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) }


## Function to compute the inverse of the result of the makeCacheMatrix function above.
## First it checks to see if the inverse has already been calculated. If yes,
## then it takes the inverse from the cache and skips the calculation.  If no,
## then it will calculate the inverse and puts the value in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getInverse()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx) }
  ans <- x$get()
  invx <- solve(ans, ...)
  x$setInverse(invx)
  invx }