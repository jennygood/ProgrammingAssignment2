## Programming Assignment 2 : Lexical Scoping

## Part 1: makeCacheMatrix which creates a list containing a function to perform the following steps:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse    
  getInv <- function() inv
  list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## Part 2: cacheSolve which calculates the inverse of the matrix using the makeCacheMatrix function:

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  
  if(!is.null(i)) {
    message("Retrieving the cached data.")   
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setInv(i)
  i
}


## matrix_test: testing the "makeCacheMatrix" and "cacheSolve" functions.

# z <- cbind(c(1, 3), c(2, 4))
# z
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4

# matrix_test = makeCacheMatrix(z)
# matrix_test$get()
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4

# cacheSolve(matrix_test)
#      [,1] [,2]
# [1,]  -2.0  1.0
# [2,]   1.5 -0.5

# cacheSolve(matrix_test)
# Retrieving the cached data.
#      [,1] [,2]
# [1,]  -2.0  1.0
# [2,]   1.5 -0.5
