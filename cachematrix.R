## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# You define an object blueprint "makeCacheMatrix" with an empty data-variable
# and 2 setter- and 2 gettermethods.
#
# set: writing a matrix in a variable
# get: reading the matrix from variable
# set_inverse_matrix: calculating the inverse of a matrix with solve()
# get_inverse_matrix: reading the inverted matrix from variable
#
#
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  get <- function() x
  
  set_inverse_matrix <- function() invM <<- solve(x)
  
  get_inverse_matrix <- function() invM
  
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
  
}



## Write a short comment describing this function
#
# 1st you build a real object from bluprint "makeCacheMarix". (e.g. MatrixA <- makeCacheMatrix())
# this objext you give to cacheSolve.   (e.g. CacheSolve(MatrixA))
#
# If you have given a matrix to your real object (e.g. MatrixA$set(matrix(1:4,2)))
# Cachesolve trys to get the cached inverted matrix. 
# If exist than return the cached matrix.
# If NOT exist than call the function set_inverse_matrix() for calculating the inverted matrix.
# After that, read and return the inverted matrix.
#
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$get_inverse_matrix()
  if (!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  x$set_inverse_matrix()
  x$get_inverse_matrix()
}