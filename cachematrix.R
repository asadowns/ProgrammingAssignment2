##cachematrix.R

# Given an invertible matrix 
# If the inverse of the the matrix is cached return that value and the message "getting cached data"
# Else calculate the inverse and return that value

#usage: 
# matrix <- makeCacheMatrix() 
# matrix$set(x)
# OR 
# matrix <- makeCacheMatrix(x)
# cacheSolve(matrix)

#test using test(matrix)

# makeCacheMatrix 
# returns a list with 4 functions
# 1. Set the value of a matrix
# 2. Get the value of a matrix
# 3. Set the value of the inverse of a matrix
# 4. get the value of the inverfse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve
## Checks to see if the inverse of a matrix has been cached
## Returns the cached inverse if available
## If data not cached calculates inverse of matrix, sets inverse, and returns inverse

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

## Optional additional testing function
test <- function(x) {
  
  ##Tests above functions
  matrix <- x$get()
  inv <- cacheSolve(x)
  
  ## Identity matrix
  I <- diag(ncol(matrix))
  
  #Need to round to deal with rounding errors
  round(matrix %*% inv) == I
}
