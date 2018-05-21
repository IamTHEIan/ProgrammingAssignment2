## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 'makeCacheMatrix' returns a list that contains 4 functions and 
## 2 data objects that are set in the 'makeCacheMatrix'
## parent environment. 
## initialization of the two variables, set x with a default value
## to prevent error message in get()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
## define the "getters and setters"
## set the input in x in the parent environment
## set NULL value in m in the parent environment to clear any value
## of m that had been cached before
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
## define the setter and getter for the inverse matrix inv
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
## return a list and name the elements so we could access them
## with '$' operator 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
## retrieve the inverse matrix from input x
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## calculate the inverse matrix if input x does not
## already contain the inverse matrix 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(inv)
  inv
}
