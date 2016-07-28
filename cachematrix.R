## Inverting a matrix is typically a time consuming and costly space sacrifice
## The user can cache the matrix rather than continuously computing the inverse. 
## The following formula will cache the inverse of the matrix

## The function will create a list containing 4 items:
## Setting the values of the matrix 
## Get the values of the matrix
## Set the value of the inverse of the matrix
## Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(inv.mat) inv <<- inv.mat
      getinv <- function() inv
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The second function below solves for the inverse of the matrix
## First, the function checks to see if the inverse has alreadybeen calculated, and if it has then 
## it returns the inverse. If not, then the formula computes the inverse by setting the value in the cache 
## using the function above (makeCacheMatrix)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv<- solve(data,...)
        x$setinv(inv)
        inv
}
