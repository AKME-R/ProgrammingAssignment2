## The makeCacheMatrix initializes a matrix object as an argument for the 
# function and sets another object 'm' to NULL (this will ultimately take up 
# the value of the inverse matrix).makeCacheMatrix also contain four functions 
# which are returned as a list. The elements of this list are further 
# referenced by the cacheSolve. 
## cacheSolve takes in the object returned by makeCacheMatrix as an argument.
# It references getinv to determine if the inverse matrix has already been 
# calculated. If not (as in case of a new object being passed to the function, 
# it calculates the inverse using the solve function (an also assigns this to m). 

## Initialize the matrix object and set the inverse matrix values. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Cache the inverse matrix value if present. If not, determine the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
