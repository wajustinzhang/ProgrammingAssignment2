## Two functions being implemented as follows:
## 1 function "makeCacheMatrix" caching the mean of a matrix
## 2:function "cacheSolve" returns a matrix that is the inverse of 'x'


## Cache the mean of a matrix so that for subsequence call of same matrix mean
## there is no need to compute again, instead, retrieve from the cache.
require(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
     x<<-y
     inverse<<-NULL
  }
  
  get<-function() x
  
  setInverse <-function(inv) inverse<<-inv
  getInverse <-function() inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv)) {
       message("retrieve from cache")
       inv
    }
    
    data <-x$get()
    inv <- ginv(data)
    x$setInverse(inv)
    inv
}
