#FUNCTION 1:

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  setmatrix <- function(y) {   #set matrix value
    x <<- y   #cache the matrix - assigns value y from parent environment
    inv <<- NULL   #search through parent environments for an existing definition of the variable and set to NULL
  }
  getmatrix <- function() x   #get the matrix value cached with setmatrix
  setinv <- function(inverse) inv <<- inverse   #cached value of inverse matrix is saved in m
  getinv <- function() inv   #get the saved value of inverse matrix m that was saved with setinverse
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinv = setinv, getinv = getinv)   #creates list to house the four functions
}


#FUNCTION 2:

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


### Running on the console ###

m <- matrix(rnorm(64),8,8)   #Create a matrix
m1 <- makeCacheMatrix(m)   #Run the first function
cachesolve(m1)   #Run the second function