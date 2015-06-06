## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    
    im <- NULL
    ## Set the matrix
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    ## Get the matrix
    get <- function(){x}
    
    ## Set the inverse of the matrix
    setinverse <- function(inverse){ im <<- inverse } 
    getinverse <- function(){im}
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## getting a matrix that is the inverse of 'x'
    im <- x$getinverse()
    
    ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    
    #### If the inverse wasn't yet been calculated ####
    ## getting the matrix from the object
    matrix <- x$get()
    ## calculating the inverse by using matrix multiplication
    # im <- solve(matrix) %*% matrix
    im <- solve(matrix,...)
    x$setinverse(im)
    ## returning a matrix that is the inverse of 'x'
    im
}
