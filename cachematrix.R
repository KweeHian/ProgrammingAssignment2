## <Put comments here that give an overall description of 
## what your functions do>
## The following are 2 functions that cache and compute the  
## inverse of a matrix. 

## <Write a short comment describing this function> 
## This function creates a special "matrix" object 
## that can cache its inverse. 

 
makeCacheMatrix <- function(x = matrix()) { 
     Minv <- NULL 
     set <- function(y) { 
         x <<- y; 
         Minv <<- NULL; 
     } 
     get <- function() return(x); 
     setinv <- function(inv) Minv <<- inv; 
     getinv <- function() return(Minv); 
     return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
 } 
 

## <Write a short comment describing this function>
## This function computes the inverse of the special "matrix" 
## returned (using solve()) by makeCacheMatrix above.
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
 
cacheSolve <- function(x, ...) { 
     Minv <- x$getinv() 
     if(!is.null(Minv)) { 
         message("Getting cached data...") 
         return(Minv) 
     } 
     data <- x$get() 
     Minv <- solve(data, ...) 
     x$setinv(Minv) 
     return(Minv) 
 } 
