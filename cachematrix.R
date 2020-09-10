## Function to store the inverse of a matrix to an object and use
## more efficiently

## function makeCacheMatrix creates a vector with a list of functions

makeCacheMatrix <- function(x = matrix()) {
     h <- NULL
     set <- function(y){
          x<<-y
          h<<- NULL
     }
     get <- function()x
     setInverse <- function(inverse)h<<-inverse
     getInverse <- function()h
     list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## function calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     h<-x$getInverse()
     if(!is.null(h)){
          message("getting cached data")
          return(h)
     }
     mtx <- x$get()
     h<-solve(mtx,...)
     x$setInverse(h)
     h
}
