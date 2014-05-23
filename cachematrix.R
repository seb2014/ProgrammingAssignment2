#creates a special "matrix", This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # if an object is called without a method
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'     
  m <- x$getInverse()
  #Check if cache exists
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

# Ran the code below to test results and confirm matrix is retrieved from cache when it exists
#a <- makeCacheMatrix(matrix(1:4, nrow=2,ncol=2, dimnames = list(c("row1","row2"),c("col1","col2"))))
#cacheSolve(a)
