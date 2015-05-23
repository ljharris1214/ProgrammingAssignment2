## this pair of functions (makeCacheMatrix and cacheSolve) calculates the inverse of
## a matrix, stores the result, and makes it accessible (without having to recalculate)

## makeCacheMatrix creates a list with functions to 
## a) set (store) the matrix
## b) get (return) the matrix
## c) set (store/cache) the inverse of the matrix
## d) get (return) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     ## input parameters:
     ##   x: a matrix for which you want to cache the inverse
     ## output:
     ##   a list of 
     ##
     ##   i represents the inverse of the input matrix
     
     i<-NULL
     
     set<-function(y) {
               x<<-y
               i<<-NULL  ## when item is added/modified, inverse is nulled out
     }
     get<-function() x  ##just return the input parameter
     setinverse<-function(solve) i<<-solve  ## store the inverse
     print (i)
     getinverse<-function() i ## return the inverse
     list(set=set, get=get, set=set, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first checks to see whether the inverse of the input matrix has been stored
## if so, it just returns that inverse
## if not, it calculates and then stores the inverse for use next time

cacheSolve <- function(x, ...) {
     ## input parameters: 
     ##   x: a list of functions, created by makeCacheMatrix
     ## returns
     ##   i: the inverse of the original matrix
     
     ## first time the function is called with a particular list, 
     ## the inverse is calculated and stored/cached in the list.
     ## ensuing times, the value of the inverse is just returned
     
     ## check if inverse has been stored. If so, retrieve it and return
    i<-x$getinverse()
    if(!is.null(i)) {
          message("getting cached data")
          return(i)
    } 
    
    ## if inverse matrix not already stored, then
    ## solve the inverse, cache it, and return the matrix.
    data<-x$get()
    i<-solve(data, ...)
    x$setinverse(i)
    i
}
