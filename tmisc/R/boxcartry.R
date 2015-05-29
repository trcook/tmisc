#' @title boxcartry
#' @description
#' Execute an expression, sending a boxcar notification when done or on error.
#' 
#' 
#' @param token
#' the boxcar token
#' @param taskname
#' the name of the task to be run.
#' @export

boxcartry<-function(token = .boxcar_token,taskname = 'task',task_expression) {
  
  tryCatch({
    start <- Sys.time()
    eval(...)
    end <- Sys.time()
    boxcar_notify(
      token = token,
      title = sprintf("%1$s completed successfully",taskname) ,
      body = sprintf(
        "processing took %1$s minutes",difftime(start,end,units = 'mins')
      )
    )
    0
  },
  error = function(err) {
    end <- Sys.time()
    boxcar_notify(
      token = token,
      title = sprintf("%1$s encountered error",taskname) ,
      body = sprintf(
        "Error %2$s at  %1$s minutes",err,difftime(start,end,units = 'mins')
      )
    )
  })
}
