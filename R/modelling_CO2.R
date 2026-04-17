modelling_CO2 <- function() {

  df <- blaucat_dat[, c("Carboni orgànic del sediment_mean",
                        "Ús del sòl del lloc de mostreig",
                        "Distància al mar",
                        "Distància a zones agrícoles",
                        "Distància a zones urbanes",
                        "Temperatura mitjana de l'aire",
                        "Precipitació acumulada",
                        "Elevació respecte al nivell del mar",
                        "Tipus de sòl")]
  df$`Ús del sòl del lloc de mostreig` <- factor(df$`Ús del sòl del lloc de mostreig`)
  df$`Tipus de sòl` <- factor(df$`Tipus de sòl`)

  df <- na.omit(df)

  m1 <- glm(`Carboni orgànic del sediment_mean` ~ ., data = df)
  print(summary(m1))
  m2 <- MASS::stepAIC(m1)
  print(summary(m2))

}
