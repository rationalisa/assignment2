## The function checks if the inverse of the matrix input 
## is already calculated. If so, it will return the inverse
## of the matrix calculated before which saves a lot of time.


## makeCacheMatrix creates a special "vector", 
##which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   i<-NULL
   set<-function(y){
      x<<-y
      inverse<<-NULL
   }
   get<- function(){
      x
   }
   setinverse <-function(z){
      i <<- solve(z)
   }
   getinverse <-function() i
   list(set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)
  
}


## Write a short comment describing this function
## first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
   if(!is.null(x$getinverse())){
      message("getting cached data")
      return(x$getinverse())
   }
   matrix<-x$get()
   i<- solve(matrix,...)
   x$setinverse(i)
   i
  ## Return a matrix that is the inverse of 'x'
}
