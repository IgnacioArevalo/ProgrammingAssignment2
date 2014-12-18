makeCacheMatrix<-function(x=matrix()) {
  m<-NULL                                 # m will be our matrix. It is reset to NULL every time we call makeCacheMatrix
  set<-function(y) {                      # Takes an input matrix
    x<<-y                                 # Saves that input matrix
    m<<-NULL                              # Resets to NULL when a new object is generated
  }       
  get<-function() x                       # This function returns the value of the original matrix 
  setinverse<-function(solve) m<<-solve   # This function is called by cacheSolve() during the first cacheSolve()
  getinverse<-function() m                # This function will return the cached value to cacheSolve() 
  list(set=set,get=get,                   # This is a list of the internal functions 
       setinverse=setinverse,             # It is accessed each time makeCacheMatrix() is called 
       getinverse=getinverse)  
}

cacheSolve <- function(x, ...) {          # Input x is a matrix created by makeCacheMatrix()
  m <- x$getinverse()                     # Access the matrix and gets its inverse 
  if(!is.null(m)) {                       # If the matrix was already cached...
    message("getting cached data")        # ...it sends that message to the console...
    return(m)                             # ...and returns the inverse of the given matrix
  }
  data <- x$get()                         # If x$getinverse() returns NULL...
  m <- solve(data, ...)                   # ...it calculates the inverse...
  x$setinverse(m)                         # ... and stores it in x
  m                                       # Prints the inverse in the console
}
