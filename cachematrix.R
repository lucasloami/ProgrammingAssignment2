## Put comments here that give an overall description of what your
## functions do

## This function simulates a class called makeCacheMatrix and it
## contains four main methods(functions):
## 1. set: set the matrix
## 2. get: return the matrix defined in makeCacheMatrix
## 3. setinverse: set the inverse of the matrix to be cached
## 4. getinverse: return the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinverse <- function(inversematrix) m <<- inversematrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix. If the matrix
## has already been calculated, the function gets the data from cache
## to make faster processing

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    print("data has been gotten from cached data")
    return(m)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}