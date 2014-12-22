## This pair of functions first creates a list of objects for a supplied matrix which
## store the original values of the matrix and are ready to store its inverse 
## The second function looks into the cached values created by the first function and if
## they are empty calculates the inverse and returns it, and if they are supplied
## returns that inverse.

## Create a list of objects to contain the original values and the created inverse values.

makeCacheMatrix <- function(x = matrix()) {      # input x will be a matrix
        
        m <- NULL    #  m will be our 'inverse matrix' and it's reset to NULL every 
        #    time makeVector is called
        
        #  these next three functions are defined but not run when makeVector is called.
        #  instead, they will be used by cachesolve() to get values for x or for
        #  m (inverse) and for setting the inverse.
        
        get <- function() { x }   # this function returns the value of the original matrix
        
        setinverse <- function(inverse)  { m <<- inverse }
        # this is called by cachesolve() during the first cachesolve()
        #  access and it will store the value using superassignment
        
        getinverse <- function() { m } # this will return the cached value to cachesolve() on
        #  subsequent accesses
        
        list(get = get,          #  OK, this is accessed each time makeCacheMatrix() is called,       
             setinverse = setinverse,  #   that is, each time we make a new object.  This is a list of 
             getinverse = getinverse)  #   the internal functions ('methods') so a calling function
        #   knows how to access those methods.                            
}



## See if the inverse is supplied, if it is not, calculate it and cache it in the specified object

cacheSolve <- function(x, ...) {   # the input x is an object created by makeVector
        m <- x$getinverse()               # accesses the object 'x' and gets the value of the mean
        if(!is.null(m)) {              # if inverse was already cached (not NULL) ...
                
                message("getting cached matrix")  # ... send this message to the console
                return(m)                       # ... and return the mean ... "return" ends 
                #   the function cacheSolve()
        }
        data <- x$get()        # we reach this code only if x$getmean() returned NULL
        m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
        x$setinverse(m)           # store the calculated inverse matrix in x (see setinverse() in makeVector
        m               # return the inverse matrix to the code that called this function
}
