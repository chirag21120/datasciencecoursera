## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function is responsible for set, get, setinverse and getinverse operations
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse_matrix 
  inverse_matrix <- NULL
  
  # Function to set the matrix
  set <- function(mat){
    x <<- mat
    inverse_matrix <<- NULL
  }
  
  # Function to get the matrix 
  get <- function() x  
  
  # Function to set the inverse matrix
  setinverse <- function(im) inverse_matrix <<- im 
  
  # Function to get the inverse matrix
  getinverse <- function() inverse_matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
# This function checks whether the inverse matrix is already present or not. If present, it returns it otherwise
# it solves for inverse matrix and then returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Retrieve the inverse matrix
  inverse_matrix <- x$getinverse()
  
  # If the inverse matrix is cached, return it
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  
  # If the inverse matrix is not cached, calculate it
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  
  # Cache the calculated inverse matrix
  x$setinverse(inverse_matrix)
  
  # Return the calculated inverse matrix
  inverse_matrix
}

## Sample Example for Testing
myMatrix <- makeCacheMatrix(matrix(c(1,2,1,3,4,5,2,1,3), nrow = 3, byrow = TRUE))

cat("Original Matrix:\n")
print(myMatrix$get())

invMatrix <- cacheSolve(myMatrix)
cat("\nInverse Matrix:\n")
print(invMatrix)

cachedInvMatrix <- cacheSolve(myMatrix)
cat("\nCached Inverse Matrix:\n")
print(cachedInvMatrix)




