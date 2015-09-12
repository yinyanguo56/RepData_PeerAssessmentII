## cache the inverse of a matrix

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x=matrix) {
  
  if(!is.matrix(x)) stop("x must be a matrix")
  
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinverse <- function(inversedMatrix) invM <<- inversedMatrix
  getinverse <- function() invM
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve get inverse matrix invM
## checks if the inverse has already been computed. If so, it gets the result
## without computation. If not, it computes the inverse, sets the value in 
##  the cache using setinverse function.
cacheSolve <- function(x, ...) {
  invM <- x$getinverse()
  if(!is.null(invM)) {
    message("getting cached inverse matrix")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data)
  x$setinverse(invM)
  invM
}

## >x <-  rbind(c(1,  2,  3), c(2,  3,  1), c(1,  1,  1))
## >m <-  makeCacheMatrix(x)
## >m$get()
##       [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    2    3    1
## [3,]    1    1    1

