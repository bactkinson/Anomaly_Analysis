require(gmailr)

setwd("../")

gm_auth_configure(path=paste0(getwd(),"/credentials.json"))
  
gm_auth(email = "blakeactkinson@gmail.com",
          path = NULL,
          scopes = "full",
          cache = gargle::gargle_oauth_cache(),
          use_oob = TRUE,
          token = NULL) 

send_message_to_myself <- function(title,body){
  message <- gm_mime() %>%
    gm_to("blakeactkinson@gmail.com") %>%
    gm_from("blakeactkinson@gmail.com") %>%
    gm_subject(title) %>%
    gm_text_body(body)  
  
  gm_send_message(message)
}

## END FUNCTION