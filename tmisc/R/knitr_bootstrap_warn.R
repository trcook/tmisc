#' 
#' @title bootstrap_warning
#' @description
#' A simple knitr function that outputs a bootstrap warning block in html. Good for knitr-build websites. 
#' @param head
#' A bold portion of the warning. A character string.
#' @param x
#' the text of the body of the warning. A character string.
#' @examples 
#' 
#' bootstrap_warning("Warning 05-19: ","This warning is serious. seriously.")
#' # returns:
#' <div class="alert alert-danger alert-dismissable">
#' <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;
#' </button>
#' <strong>Warning 05-19: </strong>This warning is serious. seriously.
#' </div>
#' @usage
#' bootstrap_warning(head,x)
#' @return
#' A html tag with the warning in it
#' @export
bootstrap_warning<-function(head,x){
  cat('<div class="alert alert-danger alert-dismissable">
<button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;
      </button>')
  cat(paste('
            <strong>',head,'</strong>',x,sep='')) # It's crucial to the markdown processor that tags begin/end immediately after newline
  cat('\n</div>')
}


