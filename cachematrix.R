## 1. Run the Source Code
## 2. Create variable 'cm <- makeCacheMatrix()'     *View your matrix with 'cm$get()'
## 3. Store the inverse matrix with 'cm_inver <- cm$getinv()'     *View the inverse matrix with 'cm_inver'
## 4. Solve and store cached matrix 'cm_solved <- cacheSolve(cm)'     *View the solution with 'cm_solved'
## 5. Check the code with 'identical(cm_inver, cm_solved)'      *Should equal true!

library(MASS)

makeCacheMatrix <- function(x = matrix(rnorm(1:20), 4, 5)) {
  
    inv <- NULL
    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() {
          inver <- ginv(x)
          inver %*% x
    }
    
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setmean(inv)
    inv

}
