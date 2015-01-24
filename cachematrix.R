makeCachematrix <- function(x=matrix()) {  ##creates a special matrix object that can cache its inverse
  m <- NULL   ## sets the value of m to NULL
  set <- function(y) {  ## sets the value of the matrix
    x <<- y  ## caches the inputed matrix so cacheSolve can check whether it has changed
    m <<- NULL  ## sets the value of m to NULL
    }
  get <- function() x  ##returns the matrix
  setsolve <- function(solve) m <<- solve  ##calculates the value of the inverse
  getsolve <- function () m  ##returns the value of the inverse
  list (set=set, get=get, setsolve=setsolve, getsolve=getsolve)  ##creates a list to hold the four functions
}

cacheSolve <- function(x=matrix() ...) {  ##returns the inverse of the matrix
  m <- x$getsolve()  ##if an inverse has been calculated, returns it
  if(!is.null(m)) {  ##check to see if cacheSolve has been run before
    message ("getting cached data")
    return(m)   ##if it is, returns the cached value
  }
  data <- x$get()  ##if not, calculates the inverse for the matrix saved in x
  m <- solve(data, ...)  ##computes the value of the inverse of the input matrix
  x$setsolve(m)  ##reuns setSolve function on the inverse to cache the inverse
  return(m)  ##returns the inverse
}
