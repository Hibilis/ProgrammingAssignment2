## Put comments here that give an overall description of what your functions do

#There are two functions in the file: makeCacheMatrix, and cacheSolve.  
#makeCacheMatrix is a function that creates a matrix, "m", when the user provides values and dimensions
#the function also creates an inverse of the matrix using the solve function
#cacheSolve is a function that pulls the values of the matrix created in makeCacheMatrix
#it pulls cached data if no new data is available


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(),nrow=2, ncol=2) {
    m <- NULL
    set <- function(y) {
        x <<- y     #x takes on the value of y
        m <<- NULL  #m is set to NULL
    }
        
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix  
    getmatrix <- function() m         #this function returns matrix 'm', is used in cacheSolve
    getinverse  <- function(solve) m  #this function creates the inverse of matrix 'm'

    list(set = set, get = get,   #this is a list of the individual funtions (above) within makeCacheMatrix
         setmatrix = setmatrix,
         getmatrix = getmatrix,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    m <- x$getmatrix()    #m takes on the value of x from the getmatrix funciton
    if(!is.null(m)) {
        message("getting cached data")  #if m is null/has no values, give message "getting cached data"
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  #solve function inverts the matrix, using data from get function in makeCache
    x$setmatrix(m)
    x$getinverse(m)
    m
}




