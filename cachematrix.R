## Put comments here that give an overall description of what your functions do

#There are two functions in the file: makeCacheMatrix, and cacheSolve.  
#makeCacheMatrix is a function that creates a matrix, "m", when the user provides values and dimensions
#the function also creates an inverse of the matrix using the solve function
#cacheSolve is a function that pulls the values of the matrix created in makeCacheMatrix
#it pulls cached data if no new data is available




makeCacheMatrix <- function(x = matrix(),nrow=2, ncol=2) {   #x is an assigned variable, a matrix function
    m <- NULL  #set the matrix value to null, m is now an assigned variable
    set <- function(y) {
        x <<- y     #x takes on the value of y
        m <<- NULL  #m is set to NULL
    }
        
    get <- function() x   #this will return the value of the matrix, if it exists
    setmatrix <- function(matrix) m <<- matrix  #cacheSolve will call this function, if necessary
    getmatrix <- function() m         #this function returns matrix 'm', is used in cacheSolve
    getinverse  <- function(solve) m  #this function creates the inverse of matrix 'm'

    list(set = set, get = get,   #this is a list of the individual funtions (above) within makeCacheMatrix
         setmatrix = setmatrix,
         getmatrix = getmatrix,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## cacheSolve is a function that pulls the values of the matrix created in makeCacheMatrix
#it pulls cached data if no new data is available

cacheSolve <- function(x, ...) {    #the 'x' here refers to the 'x' created in makeCacheMatrix - because
                                    #we are in the same environment
        
    m <- x$getmatrix()    #m takes on the value of from the getmatrix function in makeCacheMatrix
    if(!is.null(m)) {     #check to see if the matrix is NOT null (represented by the '!')
        message("getting cached data")  #if m is null/has no values, then give message "getting cached data"
        return(m)                       #and also return the cached matrix
    }
    data <- x$get()        #if the matrix was null, then function works through these steps
    m <- solve(data, ...)  #solve function inverts the matrix, using data from get function in makeCache
    x$setmatrix(m)
    x$getinverse(m)  #call the cached getinverse results from makeCacheMatrix
    m
}




