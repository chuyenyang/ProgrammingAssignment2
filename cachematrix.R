## These functions are able to cache inverse matrix in an space that is created.
## If the inverse matrix is not changing, then the function will output
## "cache matrix exists" and returns the inverse matrix rather than recompute.

## This function is going to create an space where the inverse matrix can be 
## stored. 
## It contains four functions which are: set, get, setincerse and getinverse. 
## 1. set function : Deal with the input matrix if it needs to be reconstructed,
##                   such as multiple by 5 or adding 3. This function is not 
##                   useful in this situation since the input matrix is not 
##                   reconstructed.
## 2. get function : An space where the input matrix stored.
## 3. setinverse function : Assign the inverse matrix to ix, which stands for 
##                          inverse x that is assign to be NULL at first.
## 4. getinverse function : As what it called, it can return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y){
                x <- y
                ix <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ix <<- inverse
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}

## This function is going to solve the input matrix which is created by the 
## function above. This function assign temp_ix as the getinverse in the above 
## function, which is NULL at the first. Then checks to see if the temp_ix exist.
## If the inverse matrix is already be computed than it returns the inverse 
## matrix directly rather than recompute it.

cacheSolve <- function(x, ...) {
        temp_ix <- x$getinverse()
        if(!is.null(temp_ix)){
                message("cache matrix exists")
                return(temp_ix)
        }
        
        else{
                inputmatrix <- x$get()
                temp_ix <- solve(inputmatrix)
                x$setinverse(temp_ix)
                temp_ix
        }
}
