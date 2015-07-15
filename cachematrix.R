## Put comments here that give an overall description of what your
## functions do
#The functions here work together to be able to work with a matrix object that 
#can keep its inverse in cache. This prevents having to repeat the relatively
#expensive operation of inversion.

## Write a short comment describing this function
#makeCacheMatrix creates a list of functions that can access both a matrix
#and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #prepare variable for caching
    inverse <- NULL
    #enable resetting the data (also reset the inverse!)
    set <- function(newmatrix) {
        x <<- newmatrix
        inverse <<- NULL
    }
    #enable access to the data
    get <- function() {x}
    #get and set the inverse
    setinverse <- function(inversematrix) {inverse <<- inversematrix}
    getinverse <- function() {inverse}
    #register the functions and return these
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#This function works with lists created my makeCacheMatrix
#when called, it will first try to retrieve the inverse matrix from cache.
#only if it is not there, it will calculate the inverse, and then stores it
#for future access, as well as returning it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #see what we have stored as inverse
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        #we have some data, no need to calculate anything
        message("getting cached data")
        return(inverse)
    }
    #ok, so we have no inverse yet. Let's calcuate it.
    originalMatrix <- x$get()
    inverse <- solve(originalMatrix)
    #don't forget to store it before returning it to the caller!
    x$setinverse(inverse)
    inverse
}
