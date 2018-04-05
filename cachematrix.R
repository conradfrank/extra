## makeCacheMatrix is used to build a set of functions 
## by getting a matrix as input, implement set , get,
## set inverse of matrix and get inverse of matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {            ## x' is initialized to empty matrix 
                                                       ## user defined matrix value taken as input here
                                                       ## and assigned to 'x'
  inv_mat <- NULL                                      
  
  set <- function(y) {                                 ## 'set' function used to set matrix value
    x <<- y                                            ## assigns input matrix to 'x' in 'set' parent environment
    inv_mat <<- NULL                                   ## assigns NULL to 'inv_mat' in 'set' parent environment  
    
  }
  
  get <- function() x                                  ## 'get' function gets the value of matirx 
                                                       ## from 'x' in the parent env of 'makeCacheMatrix'...LEXICO SCOPING
  
  setinvmtx <- function(inverse_matrix) inv_mat <<- inverse_matrix  
                                                       ## 'setinvmtx' function sets the value of inversed matrix
                                                       ## <<- is used here to assign input 'inverse_matrix'
                                                       ##  to 'inV_mat' in the parent env of 'makeCacheMatrix' 
  
  getinvmtx <- function() inv_mat                      ## 'getinvmtx' function gets the value of inversed matrix
                                                       ##  from 'inv_mat' in the parent env of 'makeCacheMatrix'...LEXICO SCOPING
                                                      
  list(set = set, get = get,
       setinvmtx= setinvmtx,
       getinvmtx = getinvmtx)  
}


## cacheSolve function takes as input , the output of function
## makeCacheMatrix(user defined matrix eg m1) i.e. inv_mat, and 
## checks whether inV_mat i.e. inverse of matrix m1 is empty or not
## If empty gets the original matrix eg m1 and sets inV_mat
## to inverse of m1 using 'solve' function
## If not empty it prints message "Getting cached data"

cacheSolve <- function(x, ...) {                       ## Return a matrix that is the inverse of 'x'

  inv_mat <- x$getinvmtx()                             ## gets value of inversed matrix from function 'makeCacheMatrix'
  
  if(!is.null(inv_mat)) {                              ## checks if 'inV_mat' is not NULL
    message("Getting cached data")                     ## prints the message in quotes
    return(inv_mat)                                    ## returns the inversed matrix 
    
  }
 ## if 'inV_mat' ie value of inversed matrix not NULL then following code lines executes
  
  data <- x$get()                                      ## gets the value of matrix from the input
  inv_mat <- solve(data, ...)                          ## 'solve' function used to inverse matrix  
  x$setinvmtx(inv_mat)                                 ## set the inversed matrix value to 'inV_mat'
  inv_mat                                              ## returns value of 'inv_mat' ie inversed matirx
  
}