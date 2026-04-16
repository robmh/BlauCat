eta_squared <- function(df) {

  m <- aov(y ~ ., data = df)
  eta <- lsr::etaSquared(m)
  return(eta)
}
