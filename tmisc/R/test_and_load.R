#' @title test_and_load
#' @description
#' A really simple function that checks to see if a file is loaded in the global namespace and then loads from a file if needed
#' This is useful for anticipating and preventing errors in scripts.
#' 
#' @param object
#' The quoted name of the object to be checked.
#' @param file_path
#' String or file.path() for the path to the file to be loaded if the file is not present.
#' @export

test_and_load<-function(file_path,object="poast_dyadic"){
  eval(substitute(testObject(x),list(x=as.character(object))))
  if(eval(substitute(testObject(x),list(x=as.character(object))))==F){
    print(sprintf("loading %1$s from file",object))  
    load(file_path,envir = .GlobalEnv)
  }else{
    sprintf("using %1$s from memory",object)
  }
}
