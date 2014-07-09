#' @title bootstrap_figure
#' @name bootstrap_figure
#' @description
#' A block that puts an image into a panel in bootstrap.
#' @param fig_title
#' The name of the figure
#' @param  fig_prefix
#' The word/s before the number. Default is "Figure", but you might change it to "number" or whatever
#' @param fig_number
#' the number of the figure. Use a counter(see example) here if you want to update the figure numbers throughout the document
#' @param content
#' This is the part that is in the body of the figure. Ideally, it'd be something like an image.
#' @usage
#' bootstrap_figure(fig_title="",fig_number=NULL,fig_prefix="Figure",content=...)
#' @examples
#' # Setup a counter to use for this
#' counter<-function(){i<-0;function(){i<<-i+1;return(i)}}
#' fig_num<-counter()
#' # run bootstrap_figure on a picture of a historgram
#' bootstrap_figure(fig_title="title",fig_number="a",content=hist(runif(10,0,1)))
#' # To run with ggplot, be sure to enclose the expression in a print() command:
#' require(ggplot2)
#' bootstrap_figure(fig_title="title",fig_number="a",content=print(qplot(rnorm(1000,0,1),binwidth=.2)))
#' @note
#' ggplot2 will work with this function, but your expression MUST be enclosed in a print command. See the examples.
#' @export


bootstrap_figure<-function(fig_title="",fig_number=NULL,fig_prefix="Figure",content=...){
cat('<div class="panel panel-default">\n<div class="panel-heading">\n<h3 class="panel-title">\n')
  if(!is.null(fig_number)){
    cat(paste(fig_prefix," ",fig_number,": ",sep=""))
  }
  cat(fig_title)
  cat('</h3>\n</div>\n<div class="panel-body">')
cat(
  paste('{% capture plot',fig_number,' %}\n',sep="") #liquid templating
  )
exitfun<-function(){
  content
  cat('\n{% endcapture %}\n')
  cat(paste('{{ plot',fig_number,'|markdownify }}',sep=""))
  cat('\n</div>\n</div>')}

return(exitfun())

}

