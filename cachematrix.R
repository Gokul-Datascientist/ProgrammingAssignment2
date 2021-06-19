##creating a function named makeCatcheMatrix 
##declaring the inv variable to NULL
##creating a function names set
##declaring the variable y to x using Closures,which  can access all variables and parameters in that function.
##creating a function get to get the value
##setinv is used to set the inverse value
##getinv is used to get the inverse 
##printing the output of the function 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {  
    x <<- y 
    inv <<- NULL            
  }
  get <- function() x      
  setinv <- function(inverse) {inv <<- inverse}    
  getinv <- function() inv    
  print(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
}


#creating a function to calculate the inverse of the matix
##Checking whether the matrix to be inverted is NULL
##If not NULL printing "getting catched result"
## Returning inverse
##calling get function to get the values
##solve is used to find the inverse of the matix 
## setting the value of the inverse by calling setinv
##printing the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")  
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)  
  inv         
}


m <- matrix(rnorm(16),4,4)  ##getting the input matrix 
m1 <- makeCacheMatrix(m)  ##calling the makeCacheMatrix to create a cache for matrix
cacheSolve(m1) ##finding the inverse of the matrix by calling the function "CacheSolve"

