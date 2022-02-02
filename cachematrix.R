## This short program creates a matrix object which can cache its inverse. The caching is carried
## out by the function "cacheSolve()" which then computes and returns the inverse of our matrix. 
## The creation of the matrix takes place via the function "makeCacheMatrix()".

## The function "cacheSolve()" first checks if the components of our matrix have changed. If the
## contents are same, then it returns the inverse from cache, thus saving time from an otherwise
## costly and unnecessary computation.

## makeCacheMatrix(): This function creates a matrix and returns a list which contains the
##                    functions to set and get the matrix as well as its inverse. The inverse of
##                    this matrix created by this function can then be cached by the function
##                    "cacheSolve()".

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  getInverse <- function() {
    inverse
  }
  list(
    set = set,# set() function: setter function, used to manipulate the matrix
    get = get,# get() function: getter function, used to return the matrix
    setInverse = setInverse,# setInverse() function: setter function, works with cacheSolve(), to set the inverse
    getInverse = getInverse# getInverse() function: getter function, returns the inverse
  )
}


## cacheSolve(): This function computes the inverse of our matrix. If it encounters a new matrix,
##               then it computes (using the "solve()" function) and returns its inverse. 
##               Otherwise, if it encounters a matrix whose components have not changed, then it
##               returns the inverse from cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Fetching cached inverse!!!")
    return(inverse) # returns the cached inverse of 'x'
  }
  data <- x$get()
  newInverse <- solve(data)
  x$setInverse(newInverse)
  return(newInverse) # returns the inverse of 'x'
}
