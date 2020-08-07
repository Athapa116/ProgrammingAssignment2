## The first function makeCacheMateric creates a special "matrix". We have one function in the global environment and the other one is in the parent.

## The function makeCacheMatrix consists of list 
#set the value of the matrix
#get the value of matrix
#set the value of the inverse
#get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL 
  }
  get<- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list( set = set, get = get, 
          setinverse = setinverse,
          getinverse = getinverse)

}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
    if(!is.null(inv)){
      message ("getting cached data")
      return(inv)
    }
  data <-x$get()
  inv <- solve(data,... )
  x$setinverse(inv)
  inv
}
