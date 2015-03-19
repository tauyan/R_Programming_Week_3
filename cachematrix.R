## A pair of functions that compute and cache the inverse
## of a matrix.

## "makeCacheMatrix" creates a special "matrix" object that
## can cache the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      set <- function(y) {          
            x   <<- y
            inv <<- NULL
      }
      
      get<-function() x
      
      setInverse <- function(inverse) inv<<-inverse
      
      getInverse <- function() inv
      
      ## Create a list that includes all the object's functions
      list (set=set, get=get, 
            setInverse=setInverse,
            getInverse=getInverse)   
}


## This function returns the inverse of the special "matrix" 
## given by makeCacheMatrix above. If the inverse has already been calculated
## for this exact matrix, then the function retrieves the inverse from the
## cache. If not, the function calculates and caches the inverse.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      
      if(!is.null(inv)){
            message("getting cached matrix")
            return(inv)      ## Return the inverse of 'x' if already cached
      }
      
      data <- x$get()
      
      inv <- solve(data,...)
      
      x$setInverse(inv)
      
      inv                    ## Return the inverse of 'x'
}
