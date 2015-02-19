## These functions store a value of a calculation of inverse of a matrix in cache
## and it will be brought up from cache once the same matrix needs to be inverted

## This function creates a list of functions to get or set the value of a vector  
## and to get or set the calculated inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL                                        
    	set <- function(y) {                              ##function to set the value of a matrix
        	x <<- y
        	m <<- NULL
    	} 
    	get <- function() x                               ##function to get the value of a matrix
    	set_inv_matrix <- function(solve) m <<- solve     ##set the value of the solve 
    	get_inv_matrix <- function() m                    ##get the value of the solve
    	list(set = set, get = get,                        ##create the list of functions
         set_inv_matrix = set_inv_matrix,
         get_inv_matrix = get_inv_matrix)	
}


## This function checkes if the inverse of a matrix was already 
## calcualted and returns it from the cache, or if not, calculates it and stores in the cache

cacheSolve <- function(x, ...) {
	m <- x$get_inv_matrix()                    ##assign already inverted matrix to m
    	if(!is.null(m)) {                          ##check if m had been already calculated
        message("getting cached data")
        return(m)                                  ##and return already calculated inverted matrix
    }
    data <- x$get()                                   
    m <- solve(data, ...)                          ##if not calculated before, solve the matrix 
    x$set_inv_matrix(m)                            ##and store the value in cached matrix
    m						   ##return a matrix that is the inverse of 'x'
}
