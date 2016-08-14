makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##  Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse
makecachematrix <- function(x = matrix()) 
  {
  inverse1 <- NULL
  set <- function(y) 
    {
    x <<- y
    inverse1 <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inverse1 <<- inverse
  getinverse <- function() inverse1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }



##  This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cachesolve <- function(x, ...) 
  {
  inverse1 <- x$getinverse()
  if(!is.null(inverse1)) 
    {
    message("getting cached data")
    return(inverse1)
  }
  data <- x$get()
  inverse1 <- solve(data)
  x$setinverse(inverse1)
  inverse1
}
        ## Return a matrix that is the inverse of 'x'
x = matrix(1:4,2, 2)
y= makecachematrix(x)
y$get()

cachesolve(y)
