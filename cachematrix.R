## There are two functions:
## 1. makeCacheMatrix: The "main" fn returns a list of 4 fns that 
##    creates a special "matrix" object "x" that caches its inverse.
## 2. cacheSolve: Computes the inverse of the "matrix" 
##    "x" which is the argument of makeCacheMatrix. 
##    Cached inverse = NULL implies the matrix has changed,
##    in which case an inverse is computed using "solve" 
##    If not NULL, the fn returns the cached value
##    Does not check whether x is invertible


## makeCacheMatrix is a list of 4 functions with one argument "x":
## "x" is required to be a matrix that is assumed to be invertible
## set, get, setcachedinverse and getcachedinverse
## set(y): Updates the global variable x with value of y 
## get:    Returns the matrix x stored in the main function
## setcachedinverse: Updates the cached inverse with a value!
## getcachedinverse: gets the value cached as the inverse

makeCacheMatrix <- function(x = matrix()) 
{
        cachedinverse <- NULL

        ## set updates global variable x with value of y
        set <- function(y) 
        {
                x <<- y
                cachedinverse <<- NULL ## For the new matrix, the inverse has not been computed
        }

        ## Returns the matrix created when makeCacheMatrix was last called
        get <- function() x

        ## Updates the cached inverse
        setcachedinverse <- function(inverse) cachedinverse <<- inverse

        ## Returns the cached inverse
        getcachedinverse <- function() cachedinverse

        list(set = set, get = get,setcachedinverse = setcachedinverse,getcachedinverse = getcachedinverse)
}


## Write a short comment describing this function
## CacheSolve calculates the mean of the special matrix created with the main fn. 
## It first checks to see if the mean has already been calculated. 
## If so, it return the inverse from the cache
## Else the inverse is calculated
## Updates the cached valu and returns the inverse


cacheSolve <- function(x, ...) 
{
        cachedinverse <- x$getcachedinverse()
        ## cachedinverse is NULL implies matrix has been changed
        if(!is.null(cachedinverse)) 
                 {
                 message("getting cached data")
                 return(cachedinverse)
                 }
        data <- x$get()
        cachedinverse <- solve(data, ...)
        x$setcachedinverse(cachedinverse)
        cachedinverse
}