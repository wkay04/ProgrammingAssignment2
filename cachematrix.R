## Creates two functions, makeCacheMatrix and cacheSolve which returns the
## inverse of a matrix after first checking for a cached value from an earlier
## computation




## makeCacheMatrix returns a list of several functions manipulating a given 
## matrix, x. 

makeCacheMatrix <- function(x= matrix()){
        inv <- NULL #Varriable inv initalized to NULL
        get <-function() x #get() simply returns the given matrix
        setinv <- function(i) inv <<- i # serinv takes a given value, i
        #and sets the inverse of the matrix to that value
        getinv<- function() inv #getinv returns the inverse of the matrix 
        
        list(set = set, get=get, setinv=setinv, getinv=getinv) #makeCahceMatrix returns a list of the above functions
       }


## cacheSolve taxes an input in the form of the list returned from makeCahceMatrix
## and searches for and returns a cached inverse of the matrix. If no cahce is avaliable it
## computes the inverse and stores it within the cahce

cacheSolve <- function(x, ...)  {
        z<-x$getinv() #z is set to the invers of the input matrix 
        if(!is.null(z)){
               print("Getting cached data") #if z is not null (ie has a cached value, then z is returned)
               return(z)
       }
       data<-x$get() #data is retrived from the input matrix
       i<<- solve(data, ...) #i is set to the inverse of the matrix using solve function
       x$setinv(i) #i is then stored in the cache of the object x
       i   ## Return a matrix that is the inverse of 'x'
}
