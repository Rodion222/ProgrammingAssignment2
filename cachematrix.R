## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
  inv <- matrix() #begins by setting the empty matrix as a placeholder for a 
  #future value
  
  setmatrix <- function(y) { 
    x <<- y
    inv <<- matrix()
  } #defines a function to set the matrix, x, to a new matrix, y, and resets 
  #the inverse matrix, m to empty one  
  
  
  getmatrix <- function() x #returns the matrix x
  setinverse <- function(inverse) inv <<- inverse #sets the inverse, m, to mean
  
  
  getinverse <- function() inv #returns the mean, inv
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}



# by the task of the assesment it is assumed that 
#a matrix used is reversable (and square...?)
cacheSolve <- function(x, ...) {#x here is an object (list) containing 
  #functions defined above
  
  inv <- x$getinverse()
  if(!is.na(inv[1,1])) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data) #calc of the inverse one  
  x$setinverse(inv)
  inv
}
