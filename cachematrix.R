## Two functions being implemented as follows:
## 1 function "makeCacheMatrix" is the method to contain the original matrix and the caching the inverse of the matrix
## 2:function "cacheSolve" is the helper method to create and cache the inverse of a matrix

require(MASS)

## Create a cache to store/access the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initial
  inverse<-NULL
  
  #store the original matrix
  set<-function(y){
     x<<-y
     inverse<<-NULL
  }
  
  # Original matrix acccessor
  get<-function() x
  
  # Cache the inverse of the matrix  
  setInverse <-function(inv) inverse<<-inv
    
  # acccessor of the inverse of the matrix  
  getInverse <-function() inverse
    
  # store object  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Helper method to retrieve, create, and cache the inverse of a passed in matrix (makeCacheMatrix Object)
cacheSolve <- function(x, ...) {
    # try to get inverse from the cacheMatrix object
    inv<-x$getInverse()
    if(!is.null(inv)) {
      # return from the cache object
       message("retrieve from cache")
       inv
    }
    
    # otherwise access the original matrix
    data <-x$get()
     
    # create the inverse
    inv <- ginv(data)
    
    # cache the inverse in the cache object
    x$setInverse(inv)
     
    # return the inverse
    inv
}
