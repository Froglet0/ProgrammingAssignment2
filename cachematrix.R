## Put comments here that give an overall description of what your
## functions do

## The two functions below allow the inverse of a matrix to be computed
## In conputing the inverse the first function creates a 'special' copy of the matrix in the function's environment
## and also a copy of its inverse once computed
## Subsequent computations of hte inverse then merely return a cached copy rather than computing again
## The second function computes the inverse of the matrix given a reference to the 'special' matrix
## With the reference it uses the set and get functions to store the result

##The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

## No specific error checks are undertaken in the function itself to confirm the matrix is square
## or that the matrix is otherwise invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m2) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matinv) inv <<- matinv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function takes the reference to the 'special' matrix returned by makeCacheMatrix
## and uses this to retreive a copy of the matrix and also a copy of its inverse
## If the inverse does not exist then the function calculates the inverse and uses
## the reference to store it in the 'special' matrix as well as returning it
## If the inverse does exist then it advises the user that it is retreving the cached solution
## and returns this

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
