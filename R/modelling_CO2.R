modelling_CO2 <- function() {

  df <- blaucat_dat[, c("Carboni orgànic del sediment_mean",
                        "Ús del sòl del lloc de mostreig",
                        "Distància al mar")]
  df$`Ús del sòl del lloc de mostreig` <- factor(df$`Ús del sòl del lloc de mostreig`)


  m1 <- glm(`Carboni orgànic del sediment_mean` ~ ., data = df)

}
