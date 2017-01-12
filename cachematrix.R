## We have two functions makeCacheMatrix and cacheSolve
## makeCacheMatrix will make a Matrix in the used environment and stores the inverse of the matrix
## it returns a list, whereby the inverse can be set and get

## try funtions by:
##> test<-matrix(rnorm(9*9),9,9)
##> test<-makeCacheMatrix(test)
##> cacheSolve(test)
##> cacheSolve(test)

makeCacheMatrix <- function(x = matrix()) {
## likewise makeVector, intialise the inverse with NULL
  inv<-matrix(nrow=nrow(x),ncol=ncol(x))
## likewise makeVector we define the funtions setinverse and getinverse
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##likewise cachemean
  inv<-x$getinverse()
  if(!any(is.na(inv))){
    message("getting cached inverse")
    return(inv)
  }
  inversematrix<-solve(x$get(), ...)
  x$setinverse(inversematrix)
  inversematrix
}



