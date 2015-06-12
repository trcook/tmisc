#' 
#' @title slack_notify
#' @description
#' A simple function that sends a push notification via slack 
#' @param webhookuri
#' this is the uri that the webhook integration of slack will give you for posting into a specified channel. You need to configure this through slack. 
#' @param body
#' the text of the body of the warning. A character string.
#' @param title
#' the title of the message (this appears first and foremost in the message.)
#' @examples 
#' \dontrun{
#' #.slack_webhook_uri<-"https://hooks.slack.com/services/xxxxx/xxxxx/xxxxx"
#' slack_notify(webhookuri=.slack_webhook_uri,title="this is an example","example")
#' }
#' @usage
#' boxcar_notify(token,body,title)
#' @return
#' Sends a push notification to the specified team chat  via slack.
#' @export

slack_notify<-function(webhookuri=.slack_webhook_uri,title,body){
	require(rjson)
  require(RCurl)   
	
	x<-list(text='message from R',
		attachments=list(list(fallback=title,
		prefix='new message from R',
		fields=list(list(
		title=title,
		value=body,
		short="FALSE")))))
	
	curlPerform(url=.slack_webhook_uri,postfields=toJSON(x))
}

