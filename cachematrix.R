## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) ## assumption: the matrix is invertible
  {
  inv <- NULL
  set <- function(y) ## setting the value of the matrix using function
    {
    x <<- y ## double arrow operator is capable of modifying in multiple level
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse ## setting the value of the inverse
  getinverse <- function() inv ##getting the value of the inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This cacheSolve function 

cacheSolve <- function(x, ...) 
  {
  
  i <- x$getinverse() ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)) ## checking if the inverse has already been calculated
    {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() ## should the "if" statement fail, the inverse should be calculated and set the inverse in the cache
  inv <- solve(data, ...)
  x$setinverse(inv)
  i
}