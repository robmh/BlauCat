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

  df$`Distància al mar` <- log(df$`Distància al mar`)
  df$`Distància a zones agrícoles` <- log(df$`Distància a zones agrícoles`)
  df$`Distància a zones urbanes` <- log(df$`Distància a zones urbanes`)

  m1 <- glm(`Carboni orgànic del sediment_mean` ~ ., data = df, family = Gamma(link = "log"))
  print(summary(m1))
  m2 <- MASS::stepAIC(m1)
  print(summary(m2))

  print(performance::r2_mcfadden(m2))

  #####
  library(gamlss)

  m3 <- gamlss(formula = `Carboni orgànic del sediment_mean` ~ . ,
               sigma.formula = ~ 1,
               data = df, family = LOGNO)
  m4 <- MASS::stepAIC(m3)
  print(summary(m3))
  print(summary(m4))
  print(cor(df$`Carboni orgànic del sediment_mean`, predict(m4)))
  plot(df$`Carboni orgànic del sediment_mean`, predict(m4))

  # Let us gamlss choose the best distribution.
  best_dist <- chooseDist(m4, type = "realplus")
  getOrder(best_dist)



  df_clean <- df
  names(df_clean) <- c("Carbon", "LandUs", "DistSea", "DistAgri", "DistUrba", "Temp", "Prec", "Elev", "Soil")
  m3 <- gamlss(formula = Carbon ~ . ,
               sigma.formula = ~ 1,
               data = df_clean, family = LOGNO)


}
