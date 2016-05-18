## This function "makeCacheMatrix" creates a special "matrix" object (x) 
## that can cache its inverse (inv). 

## It returns a special list containing the following functions:

## 1.  "set": set a matrix to x, initialize the inverse matrix inv
## 2.  "get": get the maxtrix x
## 3.  "setinv": set the inverse martrix inv
## 4.  "getinv": get the inverse martrix inv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function " cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Example:
## define a 2x2 matrix called "mx" 
## > mx
##       [,1] [,2]
## [1,]    1    2
## [2,]    2    1

## set mx
## > m1 <- makeCacheMatrix(mx)

## calculate inverse matrix of mx and cache it
## > cacheinv(m1)
##                [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## get the calculated inverse martrix
## > m1$getinv()
##               [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## get the original matrix
## > m1$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    2    1

## re-run and show the cached inverse matrix
## > cacheinv(m1)
## getting cached inverse matrix
##                [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333


