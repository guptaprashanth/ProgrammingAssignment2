## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x

  ##  setmatrix function will cache m
  setmatrix <- function(matrix) m <<- matrix

  ##  getmatrix function will return m from cache
  getmatrix <- function() m
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}

## cacheSolve function computes the inverse of a special 
## "matrix" object returned by makeCacheMatrix function
## if the inverse is already been calculated then cacheSolve
## retrieve the inverse from the cache else inverse is calculated

cacheSolve <- function(x, ...) {

  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ## inverse of matrix by solve function
  m <- solve(data, ...)
  x$setmatrix(m)

  ## Return a matrix that is the inverse of 'x'
  m
}
