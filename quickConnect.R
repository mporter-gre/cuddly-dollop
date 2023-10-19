

quickConnect <- function(username, password, host, port){
  omConn <- OMEROServer(username = username, password = password, host = host, port = as.integer(port))
  #omConn <- OMEROServer(username = uname, password = pword, host = serv, port = prt)
  omConn <- connect(omConn)
}