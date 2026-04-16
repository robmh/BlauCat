epsilon_squared <- function(df) {

  epsilon <- effectsize::rank_epsilon_squared(y ~ ., data = df)

  return(epsilon)


}
