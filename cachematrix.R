#The makeCacheMatrix function creates and returns a list of functions: 
#setmatrix: sets the matrix value
#getmatrix: gets the matrix value 
#setinvertmatrix: sets the inverse value of the matrix to be cached
#getinvertmatrix: gets the inverse value of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  setmatrix<-function(y) { 
    x <<- y
    i<<- NULL
  }
  getmatrix<-function(){
    x
  }
  setinvertmatrix<-function(solve) {
    i<<- solve
  }
  
  getinvertmatrix<-function() {
    i
  }
  list(setmatrix= setmatrix, getmatrix = getmatrix,
       setinvertmatrix= setinvertmatrix,
       getinvertmatrix= getinvertmatrix) 
}

#The cacheSolve function calculates the inverse value of the matrix that was created by the makeCacheMatrix function
#This function first checks to see if there is a cached matrix value. If there is a cached value then the cacheSolve function returns this value and skips the solve function.If there is no cached value the inverse matrix value is calculated from a matrix and this value is stored in the cache.

cacheSolve <- function(x, ...) {
  i<- x$getinvertmatrix()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$getmatrix()
  i<-solve(data,...)
  x$setinvertmatrix(i)
  i  ## Returns a matrix that is the inverse of 'x'
}  

