## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    ##assume x is a square matrix, no exception handlers for this part  
    m<-NULL
    set<-function(y)
    {
      x<<-y
      m<<-NULL  ##uses <<- operator
    }
    get<-function() x
    
    ##solve returns the inverse of the matrix argument
    setinversematrix<-function(m) 
    {
      solve(m)
    }
    
    ##returns the matrix
    getinversematrix<- function() m  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinversematrix()
    
    ##if m is NOT null, implies cached data exists and thus returns that data
    if(!is.null(m)) ##AND must also check the SAME matrix is store in m
    {
      message("getting cached data")
      return(m)
    }
    
    ##if m is null, this block will be executed, no cache data available
    ##thus must calculate a new inverse matrix 
    data<-x$get() ##data stores the user input matrix 
    
    ##assigns variable m with the inverse of the matrix
    ##after this is executed, if(!is.null(m)) will be TRUE and there will be a cached copy of the matrix
    m<-store(data)
    x$setinversematrix(m)
    m
    
}
