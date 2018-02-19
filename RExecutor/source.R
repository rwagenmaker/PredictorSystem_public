#write_function <- function(value) {
#    for (i in 1:value) {
#        write(i)
#    }
#}

client <- function(){
  while(TRUE){
    con <- socketConnection(host="localhost", port = 6011, blocking=TRUE,
                            server=FALSE, open="r+")
    f <- file("stdin")
    open(f)
    print("Enter text to be upper-cased, q to quit")
    sendme <- readLines(f, n=1)
    if(tolower(sendme)=="q"){
      break
    }
    write_resp <- writeLines(sendme, con)
    server_resp <- readLines(con, 1)
    print(paste("Your upper cased text:  ", server_resp))
    close(con)
  }
}

create_matrix <- function(value) {
#     result <- matrix()
#     for (i in 1:value) {
#         result <- matrix(nrow = i, ncol = i)
#     }
    result <- matrix(nrow = value, ncol = value)
    return(result)
}
