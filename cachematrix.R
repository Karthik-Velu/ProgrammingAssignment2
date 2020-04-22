##Overall design of the code is similar to the structure used to cache mean value in the assignment question 
##by using lexical scoping to cache the result of inverse caculations the first time the cachesolve function 
##is called on an object of makeCacheMatrix. subsequent calls to cachesolve function for the same object retrieves 
##cache data  

##function returns a list of 4 functions - getter and setter for the input matrix(i.e. X) and inverse matrix (i.e. IM)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setim <- function(inv) im <<- inv
  getim <- function() im
  list(set = set, get = get,
       setim = setim,
       getim = getim)
}


## function which actually calculates the inverse by calling solve function for the first time while simeltaneously
## setting im value in makeCacheMatrix function and subsequent calls using same object - the im value will be retreived
## using getim function from makeCacheMatrix

cacheSolve <- function(x, ...) {
  im <- x$getim()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setim(im)
  im
}
