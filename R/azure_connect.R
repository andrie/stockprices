#' Connect to Azure SQL database.
#' 
#' @param db database server name, e.g. `tcp:adv-stock-price.database.windows.net`
#' @param port port number
#' @param id user name
#' @param pw password
#' 
#' @return database connection string
#' @export
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' 
#' @importFrom stats complete.cases cov filter var
#' @importFrom utils tail
connect_to_azure_sql <- function(db, id, pw, port = 1433){
  con <- paste0(
    "Driver={ODBC Driver 13 for SQL Server};", 
    "Server=%s,%s;", 
    "Database=adv-stock-price;", 
    "Uid=%s;", 
    "Pwd=%s;", 
    "Encrypt=yes;", 
    "TrustServerCertificate=no;",
    "Connection Timeout=30;"
  )
  connection_string <- sprintf(con, db, port, id, pw)
  dbConnect(odbc::odbc(), .connection_string = connection_string)
}

