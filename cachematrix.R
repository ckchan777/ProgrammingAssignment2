## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this funtion stores  a cached version in variable m using setinversematrix function using the <<- operator

makeCacheMatrix <- function(x = matrix()) {

    ##assume x is a square matrix, no exception handlers for this part  
    m<-NULL
    set<-function(y)
    {
      x<<-y
      m<<-NULL  ##uses <<- operator
    }
    get<-function() x
    
    ##solve returns the inverse of the matrix argument, as per assignment instructions
    setinversematrix<-function(m) 
    {
      solve(m)
    }
    
    ##returns the matrix
    getinversematrix<- function() m  
}


## Write a short comment describing this function
## cacheSolve will do he following:
##If the inverse has already been calculated (and the matrix has not changed) (using identical keyword to check), 
##cachesolve should retrieve the inverse from the cache
##if m is NOT null (implies cached data exists) AND the two matrices are the same (using the identical keyword), return cached version


cacheSolve <- function(x, ...) {
    
    ##try to get m, cached version, if available
    m<-x$getinversematrix()
    
    if(!is.null(m) && identical(x,m) ) ##cache exists and matrices are identical, return that cached version
    {
      message("getting cached data")
      return(m)
    }
    
    ##if m is null OR matrices NOT the same, execute this block since no cache data available or matrix is different
    ##thus must calculate a new inverse matrix 
    data<-x$get() ##data stores the user input matrix 
    
    ##assigns variable m with the inverse of the matrix
    ##after this is executed, if(!is.null(m)) will be TRUE and there will be a cached copy of the matrix
    m<-solve(data)
    x$setinversematrix(m)##cache this matrix for future use 
    m    
}
