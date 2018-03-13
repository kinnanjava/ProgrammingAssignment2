## This function, makeCacheMatrix creates a special "vector", which is really a list containing a function to set , get , set inverse and get inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    
    xi<-  NULL
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) xi <<- inv
    getinverse <- function() xi
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of  a matrix from from cache if it is already calculated. And calculates if the inverse is not created yet
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xi <- x$getinverse()
    if(!is.null(xi)) {
        message("getting cached data")
        return(xi)
    }
    data <- x$get()
    xi <- solve(data, ...)
    x$setinverse(xi)
    xi
}


#####################       How to Test it #################################
#Create a Matrix
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
#assign it
l<-makeCacheMatrix(A)

#call the cacheSolve
# first time it calculates the matric
m<-cacheSolve(l)
m

#call the cacheSolve again . This time you will see it getting from cached data
m<-cacheSolve(l)
m