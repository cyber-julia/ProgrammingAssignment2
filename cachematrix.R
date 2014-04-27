## Functions makeCacheMatrix and cacheSolve are two functions 
## that are used to create a special object that stores a matrix 
## and cache's its inverse.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                                   ## Default value of the cache of the inverse matrix.
  set<-function(y){                         ## This nested function saves 
    x<<-y                                   ## value of a matrix to the cache, and cleans the cache
    m<<-NULL                                ## of the inverse matrix if a matrix is reset.
  }
  get<-function() x                         ## This nested function returns the value 
                                            ## of a matrix from cache.
  setsolve<-function(x) m<<-solve(x)        ## This nested function computes the inverse matrix 
                                            ## and saves its value to the cache.
  getsolve<-function() m                    ## This nested function returns the value 
                                            ## of the inverse matrix from cache.
  list(set=set, get=get, setsolve=setsolve, ## This command creates a special "matrix" object.
  getsolve=getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getsolve()                           ## Queries the value of the inverse matrix cache.
  if(!is.null(m)) {                         ## Checks if there is a cache, and if it does
    message("getting cached data")          
    return(m)                               ## returns it.
  }
  data <- x$get()                           ## If there is no inverse matrix cache, 
                                            ## then we get the 'x' matrix cache
  m <- solve(data)                          ## and compute the inverse of 'x',
  x$setsolve(m)                             ## and then save it to the inverse matrix cache.
  m                                         ## Return a matrix that is the inverse of 'x'
}
