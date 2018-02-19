# Coursera - R programming - Programming assignment (week 3)
# Assignment: Caching the Inverse of a Matrix

# HVera
# February 2018 

####

# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly.
# The following pair of functions cache the inverse of a matrix.

####  function makeCacheMatrix
# This function creates a special "list" object that can cache its inverse and contain
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inv <<- inverse
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#### function cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed), then the function 
# should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m_inv <- x$getinverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data)
  x$setinverse(m_inv)
  m_inv
}

