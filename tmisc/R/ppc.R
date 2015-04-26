#' @title ppc
#' @name ppc
#' @description
#' Gives ppc and pre for model
#' @param model
#' The name of the figure
#' 
#' @usage
#' bootstrap_figure(fig_title="",fig_number=NULL,fig_prefix="Figure",content=...)
#' @note
#' Model must be of type 'glm' (or inherit it) for this to work correclty
#' 
#' 
#' 
#' @export

ppc<-function(model){
  stopifnot("glm"%in%class(model))
  test<-ifelse(model$fitted.values>.5,1,0)
  actual<-model$y
  correct<-ifelse(test==actual,1,0)
  ppc<-sum(correct)/length(actual)
  mode_cat<-as.numeric(names(table(model$y))[table(model$y)==max(table(model$y))])
  #: yes, this really does seem to be the best way to calculate the mode
  mode_percent<-max(table(actual))/length(actual) 
  #: this is more simple than calculting the mode category (but we keep the mode category for usefulness). it gives us the percent of the mode category
  
  percent_improvement<-((1-mode_percent)-(1-ppc))/(1-mode_percent)
  out<-list(ppc=ppc,
            Ncorrect=sum(correct),
            mode_actual=mode_cat,
            mode_size=max(table(actual)),
            mode_percent=mode_percent,
            improvement=percent_improvement,
            N=length(actual)
  )
  class(out)<-append(class(out),"my_ppc",after = 0)
  return(out
  )
  
}

print.my_ppc<-function(x){
  cat("percent predicted correctly:",x$ppc,"\n\nraw improvement",x$ppc-x$mode_percent, "\n(change in percentage of correct classifications over prediction based on mode)","\n\npercentage improvement over modal",x$improvement,"\n(magnitude of improvement in classification *relative* to prediction based on mode. add 1 to get factor of improvement -- e.g. 'this model is",1+x$improvement,"times better at predicting the response than an empty model","\nnumber correctly predicted:",x$Ncorrect,"\n\nN of observations tested (length of data fit by the model:)",x$N,"\nmodal category",x$mode_actual,"\nobservations in mode",x$mode_size)
}