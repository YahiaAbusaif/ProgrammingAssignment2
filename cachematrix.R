## we have 2 function for solve the problem of calculate inverse matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
    InvMatrix <- NULL
    set <- function(newMtrx) {
      mtrx <<- newMtrx
      InvMatrix <<- NULL
    }
    get <- function() mtrx
    setInvMatrix <- function(NewInvMtrx) InvMatrix <<- NewInvMtrx
    getInvMatrix <- function() InvMatrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)

}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    InvMtrx <- x$getInvMatrix()
    if(!is.null(InvMtrx)) {
      message("getting cached data")
      return(InvMtrx)
    }
    data <- x$get()
    InvMtrx <- solve(data, ...)
    x$setInvMatrix(InvMtrx)
    InvMtrx
}
