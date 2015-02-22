## Put comments here that give an overall description of what your
## functions do

## This function stores input matrix and it's inverse

makeCacheMatrix <- function(MSrc = matrix())
{
  mInv <- NULL
  set <- function(mScr_New_Val)
  {
    if(class(mScr_New_Val)!="matrix")
    {
      message(sprintf("Input data is not a matric, it is %s\nProgramm will try to conver it",class(mScr_New_Val)))
      mScr_New_Val<-as.matrix(mScr_New_Val)
    }
    MSrc <<- mScr_New_Val
    #mInv <<- NULL
  }
  get <- function() MSrc
  setInv <- function(Inv) mInv<<-Inv
  getInv <- function() mInv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function solves cached matrix

cacheSolve <- function(x, ...) {
  mStored<-x$get()
  if(is.null(mStored))
  {
    message("Please cache matrix")
    return(NULL)
  }
  if(nrow(mStored)!=ncol(mStored))#!isSymmetric(mStored) - some times dont't work
  {
    message("Stored matrix isn't Symmetric")
    return(NULL)
  }
  if(!is.null(x$getInv()) && all.equal(mStored,solve(x$getInv())))#identical don't realy work
  {
    message("Stored Inversed matrix is same as input matrix")
    return(x$getInv())
  }
  out <- tryCatch(solve(mStored), error = function(e) e)
  if(any(class(out) == "error"))
  {
    message(out$message)
    return(mStored)
  }
  mSolved<-solve(mStored)
  x$setInv(mSolved)
  mSolved
        ## Return a matrix that is the inverse of 'x'
}
