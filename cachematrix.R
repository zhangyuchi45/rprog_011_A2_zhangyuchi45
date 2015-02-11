## The functions are intended to get the inverse of a numeric matrix.
# For a very large matrix, it may take a long time to compute the 
# inverse matrix. If the the matrix doesn't change and we need its 
# inverse again, the functions can cache the inverse when they get 
# the inverse for the first time, and looked up in the cache when we 
# need the invese again, instead of recomputing.
# 
# I'm showing how to use the functions below.
# 
# 1. Create a random 1000-by-1000 matrix
#     n <- 1000
#     mat <- rnorm(n^2)
#     dim(mat) <- c(n,n)
# 2. Make the matrix that can cache its inverse
#     Cmat <- makeCacheMatrix(mat)
# 3. Get its inverse
#     cacheSolve(Cmat)
# P.S. it takes less time when it gets the inverse from cache,
#     which can be proven by running
#         system.time(cacheSolve(Cmat))   for the first and 
#     second time.


## `makeCacheMatrix` creates a special "matrix" object
# that can cache its inverse. Concretely, it creates 
# a list containing a function to
#   1.  set the value of the matrix
#   2.  get the value of the matrix
#   3.  set the inverse of the matrix
#   4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
    Inv <- NULL
        #initialize the Inv (the inverse of x) to be NULL
    set <- function(y)
    {
        x <<- y
        Inv <<- NULL
            #reset Inv when x changes
    }
    
    get <- function() x
        #return x
    
    setinv <- function(xinv) Inv <<- xinv
        #set Inv manualy
    
    getinv <- function() Inv
        #return Inv
    
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


cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
    
    Inv <- x$getinv()
        #get Inv from cache
    
    if(!is.null(Inv))
    {
        message("getting cahed data")
        return(Inv)
        #return cached Inv if it's not NULL
    }
    
    #else, compute Inv and cache it
    
    data <- x$get()
    
    Inv <- solve(data)
    
    x$setinv(Inv)
        
    Inv
}
