## These two functions allow a user to input a matrix and cache its inverse for future use. 
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(Q = matrix()){
  a <- NULL
  set <- function(y){
    Q <<- y
    a <<- NULL
  }
  get <-function() Q
  setinv <- function(inv) a <<- inv
  getinv <- function() a 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. It allows you to access the inverse without changing the
##original matrix 

cacheSolve <- function(Q, ...) {
  a <- Q$getinv()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- Q$get()
  a <- solve(data, ...)
  Q$setinv(a)
  a
}
