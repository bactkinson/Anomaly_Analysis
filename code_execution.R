source(paste0(getwd(),"/send_myself_mail.R"))
start_time <- Sys.time()
tryCatch(
  {
    source(paste0(getwd(),"/DBSCAN_Direct_Analysis.R"))
    send_message_to_myself("Code execution complete",paste0("Code execution complete on home. Took",
                                                            difftime(Sys.time(),start_time,units = "hours"),
                                                            "hours."))
  }, error = function(e){
    send_message_to_myself("Error thrown in code execution","Error thrown in code execution on home")
  }
  
  
)