## This is Programming Assignment 2 for Penny Ridgdill
## This function will use cached data, when possible, 
##to improve the efficiency of matrix computations

## This function will create and return a list of functions which, in turn, will: 
## initialize and cache a matrix
## return that matrix
## compute and cache the inverse of the matrix
## return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL #sets the inverse to NULL until computed
        set <- function(y){ #this function initializes and caches the matrix
          x <<- y
          xinv <<- NULL
        } 
        get <- function() x #a function which returns the matrix
        setinv <- function(inverse) xinv <<- inverse #a function which sets/caches inv of matrix
        getinv <- function() xinv #a function returns the inverse of the matrix
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function will check to see if the inverse of the matrix has already
## been computed. If it has, it will return that inverse. If it hasn't,
## It will compute, set and cache the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         xinv <- x$getinv() #calls the getinv function from the list of functions
         if(!is.null(xinv)){#checks to see if cached
           messae("getting cached data") #tells you that it is retrieving the cached info
           return(xinv) #returns the cached inverse
         }
         mymatrix <- x$get()  #gets the matrix so that it can solve for the inverse
         
         xinv <- solve(mymatrix) #solves for inverse of the matrix
         ##note: solve, when the second argument is missing, solves for the solution
         ##to firstargument%*%x=IDENTITYMATRIX, i.e., it solves for the inverse of
         ##the matrix passed in the first argument
         
         x$setinv(xinv) #sets and caches the inverse
         xinv #returns the inverse
  
}
