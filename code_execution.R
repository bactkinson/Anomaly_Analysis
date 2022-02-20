source(paste0(getwd(),"/send_myself_mail.R"))

tryCatch(
  {
    source(paste0(getwd(),"/anomaly_clusters.R"))
    send_message_to_myself("Code execution complete","Code execution complete on GriffinLab")
  }, error = function(e){
    send_message_to_myself("Error thrown in code execution","Error thrown in code execution on GriffinLab")
  }
  
  
)