## Put comments here that give an overall description of what your
## functions do
# The functions are intended to get the inverse of a numeric matrix.
# For a very large matrix, it may take a long time to compute the 
# inverse matrix. If the the matrix doesn't change and we need its 
# inverse again, the functions can cache the inverse when they get 
# the inverse for the first time, and looked up in the cache when we 
# need the invese again, instead of recomputing.


## Write a short comment describing this function
# `makeCacheMatrix` creates a special "matrix" object
# that can cache its inverse. Concretely, it creates 
# a list containing a function to
#   1.  set the value of the matrix
#   2.  get the value of the matrix
#   3.  set the inverse of the matrix
#   4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
    Inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        Inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(xinv) Inv <<- xinv
    
    getinv <- function() Inv
    
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# 'cacheSolve' computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# 
# Besides, before I know the function 'system.time()', I have also 
# added a function allowing it to print out the time it took 
# to get inverse. Well, when I knew that  
# system.time(cacheSolve(cacheMatrix) could also do this,
# I found it not necessary to keep that code in the script.

cacheSolve <- function(x, timing=FALSE,...) {
        ## Return a matrix that is the inverse of 'x'
    
    Inv <- x$getinv()
    
    if(!is.null(Inv))
    {
        message("getting cahed data")
        return(Inv)
    }
    
    data <- x$get()
    
    Inv <- solve(data)
    
    x$setinv(Inv)
        
    Inv
}
