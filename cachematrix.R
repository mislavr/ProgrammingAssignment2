## stores and retrieves original matrix and an inverse of a matrix

makeCacheMatrix <- function(a = matrix()) {
    ## parameters:
    ## a - matrix
    ## result:
    ## list of 4 getter/setter functions - list(set, get, setInverse, getInverse)
  
    ## reset inverse value on function call
    inverseA <- NULL
  
    ## set original matrix value
    set <- function(b) {
      a <<- b
      inverseA <<- NULL
    }
    
    ## get original matrix value
    get <- function() a
    
    ## set inverse matrix value
    setInverse <- function(inverse) inverseA <<- inverse
    
    ## get inverse matrix value
    getInverse <- function() inverseA
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## calculates inverse of a matrix

cacheSolve <- function(a) {
    ## parameters:
    ## a - list returned by function makeCacheMatrix
    ## result:
    ## matrix - inverse of matrix a$get() 
      
    ## check if inverse of a matrix is already calculated/cached
  
    inverse <- a$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    ## check if matrix is singular (non invertible) by calculating determinant (det)
    ## linear algebra fact: matrix A is singular iff det(A)=0
    
    if (det(a$get())==0) stop("matrix is singular")
    
    ## calculate and store inverse of matrix
    
    inverse <- solve(a$get())
    a$setInverse(inverse)
    inverse
}
