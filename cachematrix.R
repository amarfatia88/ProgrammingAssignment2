## These functioncs will cache the inverse of a matrix by using the square function


## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##first the value of the matrix inv is set to NULL
  matrixinv <- NULL
  ##the set function changes the vector stored in the main function
  set <- function(y) {
    x <<- y
    matrixinv <<- NULL
  }
  ## gets the value of the inverse
  get <- function () x
  ## calculates the inverse of the matrix
  setmatrix <- function(solve) matrixinv <<- solve
  ## gets the inverse
  getmatrix <- function () matrixinv
  ##passes the value of the function
  list(set = set, get = get, setmatrix = setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of a matrix. 
## If it has already been calculated, then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  matrixinv <- x$getmatrix()
  ## if the inverse exists, it grabs it
  if(!is.null(matrixinv)){
    message("getting cached data")
    return(matrixinv)
  }
  ## if the inverse does not exist, it is calculated and then retrieved
  data <-x$get()
  matrixinv <- solve(data,...)
  x$setmatrix(matrixinv)
  matrixinv     
}

