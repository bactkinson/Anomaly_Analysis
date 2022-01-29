source(paste0(getwd(),"/send_myself_mail.R"))

tryCatch(
  {
    source(paste0(getwd(),"/DBSCAN_Direct_Analysis.R"))
    send_message_to_myself("Code execution complete","Code execution complete on home computer")
  }, error = function(e){
    send_message_to_myself("Error thrown in code execution","Error thrown in code execution on home computer")
  }
  
  
)