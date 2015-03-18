## These functions will take a matrix object 
## and cache the inverse of that matrix.
## They will then print the inverse matrix.


##This function takes a matrix and creates a 'special' matrix object. 
##It will cache the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  v<-NULL
  set<-function(y){
    x<<-y
    v<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) v<<-solve
  getinverse<-function() v
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes and prints the inverse of 
## the matrix calculated by makeCacheMatrix function.
## If previous cache data exists it will print a message 
## and collect the cached inverse.

cacheSolve <- function(x, ...) {
  v<-x$getinverse()
  if(!is.null(v)){
    message("getting previous cache data")
    return(v)
  }
  data<-x$get()
  v<-solve(data,...)
  x$setinverse(v)
  print(v)
}
