## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object 
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix<-NULL
  oldMatrix<-NULL
  
  set <- function(y){
      
      x <<-y
      invMatrix <<-NULL
     
    
  }
  
  get <- function() x
  
  getoldmatrix <- function() oldMatrix
  setoldmatrix <- function(old) oldMatrix <<-old
  
  setinverse <-function(inverse) invMatrix <<-inverse
  getinverse <- function() invMatrix
  
  list(set = set, get =get, 
       getoldmatrix = getoldmatrix,
       setoldmatrix = setoldmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Write a short comment describing this function
## This function computes the inverse of a special
## "Matrix" object returned by makeCacheMatrix
## if the inverse has already been calculated and 
## the matrix has not changed cachesolve will retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getinverse()
  oldMatrix <- x$getoldmatrix()
  if(!is.null(invMatrix)){
    if(oldMatrix==x$get() || is.null(oldMatrix)){
      ## retrieve the cache
      message("getting cached data")
      return(invMatrix)
      
      
    }
    
    
    
  }else{
    ## calculate the inverse of x and set the closure
    ## function and populate the cache
    
    matrix <- x$get()
    invMatrix <- solve(matrix)
    x$setinverse(invMatrix)
    x$setoldmatrix(matrix)
    invMatrix
    
    
    
    
  }
  
  
  
}
