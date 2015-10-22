## Performance optimized matrix inverse through caching.
## Functions work together to take a matrix and return inverse using solve. 
## If cached solution exists, message is printed and cache result is returned .
## 

## makeCacheMatrix returns a list containing fuctions for value assignments to calling enviroment.
## Uses <<- to assign symbol values in enclosing enviroments as a caching mechanisim.

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square matrix
        ## returns a list of functions to set or get symbol values in calling enclousure 
        
        
        ## initilaize local matrix to NULL
        mat_inverse = NULL
        
        ## create and assign original matrix "x" and "mat_inverse" in calling enviroment
        set = function(y) {
                x <<- y
                ## Reinitializes "mat_inverse" symbol in calling enviroment to NULL.
                mat_inverse <<- NULL
        }
        
        ## return original matrix
        get = function() x
        
        ## assigns assigns value of given matrix to calling enviroment symbol "mat_inverse" 
        setinv = function(i) mat_inverse <<- i
        
        ## returns either a matrix or null depending whether "setinv" has ever been called
        getinv = function() mat_inverse
        
        ## returned list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve will return the inverse of a given square matrix. It is used in conjunction with 
## makeCacheMatrix to create and maintain a results cache.
## example usage: 
## zz = makeCacheMatrix(matrix(rnorm(1000), 10, 10))
## cacheSolve(zz)

cacheSolve <- function(x, ...) {
        ## x is a list of functions returned by makeCacheMatrix
        ## returns inverse of given matrix. 
        ## cached results are returned if previously calculated
        
        ## retreive previous result - NULL or matrix
        mat_inverse = x$getinv()
        
        ## check to see if results are cached
        if(!is.null(mat_inverse)) {
                ## if cached results exist, print message, return cached result and exit
                message("getting cached data")
                return(mat_inverse)
        }
        
        ## retrive original matrix
        matdata = x$get()
        ## calucluate inverse
        mat_inverse = solve(matdata, ...)
        ## cache result for next time
        x$setinv(mat_inverse)
        ## return inverse
        mat_inverse
}
