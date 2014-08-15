#' 
#' @title boxcar_notify
#' @description
#' A simple function that sends a push notification to the device with indicated token. 
#' @param token
#' The token needed to send the push notification. See here: http://help.boxcar.io/knowledgebase/articles/314474-how-to-get-my-boxcar-access-token
#' @param body
#' the text of the body of the warning. A character string.
#' @param title
#' the title of the message (this appears first and foremost in the message.)
#' @examples 
#' \dontrun{
#' #.boxcar_token<-(your token)
#' boxcar_notify(token=.boxcar_token,"example",title="this is an example")
#' }
#' @usage
#' boxcar_notify(token,body,title)
#' @return
#' Sends a push notification to the specified token.
#' @export
boxcar_notify<-function(token=.boxcar_token,body,title){
    message<-paste('curl -d  \"user_credentials=',token, ' \" \\
     -d   \"notification[title]=',title,'\" \\
                   -d   \"notification[long_message]=',body,'\" \\
                   -d   \"notification[sound]=bird-1\" \\
                   -d   \"notification[source_name]=Your Computer  \" \\
                   -d   \"notification[icon_url]=http://www.r-project.org/Rlogo.jpg  \" \\
                   https://new.boxcar.io/api/notifications',sep="")
    system(message)
}