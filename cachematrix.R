##Assignemnt 2 of R Programming
##Caching Inverse of any matrix

##This function creates R object that stores the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve, getsolve = getsolve)
}

#Retrieve the inverse from the cached value that is stored in the makeVector() 
##object's environment. 
##This wil be done to see if if for sam matrix input inverse has been already
##calculted and if it has been than its cached value will be given, instead
##of calculating again.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s))
  {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
