## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix provides a place to store and retrieve a matrix, x, and an 
## inverse of x, a.  The two set functions assign the values of the matrices each
## time the function is triggered within the parent environment using the <<- 
## operator, while the two get functions retrieve these matrices from their 
## parent environment by placing their associated variable next to - not 
## within - the function.  Listing them allows us to use the $ operator in the 
## cacheSolve function, which loads all the associated variables and their 
## values, including a and x by way of y; using [], for example, would not.

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set_mat <- function(y)
                x <<- y
                a <<- NULL
        get_mat <- function() x
        set_inv <- function(our_inv) a <<- our_inv
        get_inv <- function() a
        list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv =
                      get_inv)
}


## cacheSolve inverts the matrix x by using the list from CacheMatrix as an 
## argument.  If this inversion is already contained in 
## makeCacheMatrix, b will not be null because a will have been reset by a 
## previous iteration of cacheSolve, and the function will simply retrieve the 
## previous value; otherwise, it calculates a new inversion and sends it to 
## makeCacheMatrix using the m$set function to store it.  Either way, it 
## then reports the inverted matrix.

cacheSolve <- function(m, ...) {
        b <- m$get_inv()
        if(!is.null(b)){
                message("getting cached data")
                return(b)
        }
        data <- m$get_mat()
        m3 <- solve(data, ...)
        m$set_inv(m3)
        m3
        ## Return a matrix that is the inverse of 'x'
}

