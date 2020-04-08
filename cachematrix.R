# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  # this function creates a list containing functions that:
  # 1. sets the original matrix (setOrigMatrix)
  # 2. gets the original matrix (getOrigMatrix)
  # 3. sets the inverse matrix (setInvMatrix)
  # 4. gets the inverse matrix (getInvMatrix)
  #  
  #  #init var to null
  m <- NULL
  ## set the beggining matrix to the matrix passed to the function
  setOrigMatrix <- function (y) {
    x <<- y
    m <<- NULL
  }
  ## get the original matrix
  getOrigMatrix <- function() x
  ## set the inverse matrix
  ## creates the inverse matrix through the use of solve
  setInvMatrix <- function(solve) m <<- solve
  ## gets the inverse matrix
  getInvMatrix <- function() m
  ## creates a list containing all the matrice functions
  list(setOrigMatrix = setOrigMatrix, getOrigMatrix = getOrigMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
  
}  



## This function checks passed matrix if it is chached.
## if it is, then it prints the chached results 
## if the matrix is not in cache then in solves the inverse

cacheSolve <- function(x, ...) {
  ## gets the passed inverse matrix 
  m <- x$getInvMatrix()
  ## if the matrix is cached return the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the matrix isn't there use solve to get the inverse matrix
  data <- x$getOrigMatrix()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}