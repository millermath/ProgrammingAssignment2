makeCacheMatrix<-function(x=matrix()){                     #makeCacheMatrix consists of list of four functions  set, get, setinverse, getinverse
        I<-NULL
        set<-function(y){                                                          #changes the matrix stored in the main function
                x<<-y
                I<<-NULL
        }
        get<-function() x                                                           #returns matrix stored in the main function
        setinverse<-function(inverse) I <<- inverse                                 #stores value of original input in a variable I in the main function
        getinverse<-function() I                                                    #returns value stored by setinverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)        #creates list consisting of the 4 functions
}


cacheSolve<-function(x,...){         
        I<-x$getinverse()                        #sets I to the value returned from getinverse          
        if(!is.null(I)){                         #if the inverse has already been computed, returns "getting cached data" and prints out the inverse
                message("getting cached data")
                return(I)
        }
        data<-x$get()                            #if I is NULL, then you get the matrix input from makeCacheMatrix          
        I<-solve(data, ...)                      #calculates inverse of matrix input from makeCacheMatrix
        x$setinverse(I)                          #takes inverse and stores it in variable I via setinverse function from makeCacheMatrix
        I
}