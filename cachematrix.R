## Put comments here that give an overall description of what your
## functions do

## Standrd usage and output
## m<-matrix(c(1,2,4,5),nrow=2,ncol=2)
## mp<-makeCacheMatrix(m)
##> cacheSolve(mp)
##[,1]       [,2]
##[1,] -1.6666667  1.3333333
##[2,]  0.6666667 -0.3333333


## this first function creates a list that contains a function to call methods on the matrix,  
## including set, get, set inverse and get inverse functions.  These are similar to object
## method calls. 
## 

makeCacheMatrix <- function(x = matrix()) {
  
  ## here the values are initialised,  and the set method creates a reference to x and i
  ## in a seperate environment
  
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
  ## further methods defined
  
    get <- function() x
    setinverse <- function(inverse) i <<- solve
    getinverse <- function() i
  
  ## the call list of methods defined
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## this function returns a matrix that is the inverse of x
## whcih itself has been created by the method above.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ## here the matrix inverse is checked for existance before recalculating
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  
}
