## The first function makes a list with methods that set and get a matrix and its inverse in an intrinsic environment variable
## The second function is passed the list from the first and attempts to calculate and set its inverse.  If the inverse is already set, teh cached value is used


makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  get <- function() x
  
  
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##create test matrix 2by2
  
  ## any inverse calculated?
  calculatedInverse <- x$getInverse() 
  
  ##cached Matrix?
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("We found cached data and saved valuable cpus!!!")
    return(calculatedInverse)
  }
  
  ## is no cached matrix get it.
  matrixToSolve <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}