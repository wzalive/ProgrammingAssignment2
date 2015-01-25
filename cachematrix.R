# CacheMatrix.R contains two function makeCacheMatrix and cacheSolve.
# makeCacheMatrix : This function creates a special matrix object that can cache its inverse
# cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix.
# .........If the inverse has been calculate then the cachesolve should retrive the inverse from the cache.  


# makeCacheMatrix: return a list of functions to 
#1. set the value of matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { # x is an invertible matrix
inv <- matrix(NA,nrow(x),ncol(x)) # inv is an matrix for holding cached values all elements are set to NA
set <- function(y){
x <<- y # setting an invertible matrix to x for x to be used in different function
inv <- matrix(NA,nrow(x),ncol(x)) # inv is an matrix for holding cached values all elements are set to NA, while setting up the matrix
}
get <- function() x #return the invertible matrix supplied to makeCacheMatrix function as argument
setSolve <- function(solve) inv <<- solve #calculate the inverse of an invertible matrix and assigning the value to inv function
getSolve <- function() inv #returns the inv variable as inverse of the invertible matrix
list(set = set, get = get, setSolve = setSolve, getSolve = getSolve) # creates a list of actions assigned in the makeCacheMatrix function
}

# cacheSolve function accepts the special matrix created by makeCacheMatrix and checks if the inverse of the matrix is already available in cache if not it calculates

cacheSolve <- function(x, ...) {
	
# Return a matrix that is the inverse of 'x'
inv <- x$getSolve() # Getting the inverse of the matrix 
if(!is.na(inv[1,1])){ # if inverse is available inv[1,1] value wouldn't be NA this logic checks that	
message("getting cached data") # confirming we are getting the value from the cache
return(inv) # returning the value of inverse of the matrix if the value available in the cache
}

data <- x$get() # assigning the matrix to data variable to be used for calculating the ivnerse of the matrix
inv <- solve(data) # setting up the inv variable to be used in cache function
x$setSolve(inv) # setting the recently set inverse to cache
inv # returing the inverse of the matrix
