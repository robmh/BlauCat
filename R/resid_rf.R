resid_rf <- function(m, df, y) {

  return(predict(m, data = df)$predictions - y)

}
