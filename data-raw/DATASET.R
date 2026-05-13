# Read Excel database.
df <- readxl::read_excel("data-raw/BBDD_existents_CAT.xlsx",
                         sheet = "RESUM",
                         col_names = TRUE,
                         progress = FALSE)


# These are two cells that contain the text "70-80". We modify them by hand.
df[60:61, 51] <- rep("75", 2)


# Split columns showing numbers as value +/- sd. Also changes decimal comas.
col_df_names <- trimws(colnames(df))
col_names_new <- NULL
blaucat_dat <- NULL
for (i in 1:ncol(df)) {
  x <- parse_column(unlist(df[, i]))
  blaucat_dat <- cbind(blaucat_dat, x)
  name_added <- col_df_names[i]
  if (NCOL(x) == 2) name_added <- paste0(name_added, "_", c("mean", "sd"))
  col_names_new <- c(col_names_new, name_added)

}
colnames(blaucat_dat) <- col_names_new
rownames(blaucat_dat) <- NULL


# Add a new column using the equivalency CO2 = f * Organic matter when CO2 is not given.
f <- 0.45
i <- which(is.na(blaucat_dat$`Carboni orgànic del sediment_mean`))
blaucat_dat$`Carboni orgànic del sediment_mean`[i] <- blaucat_dat$`Matèria orgànica del sediment_mean`[i] * f


usethis::use_data(blaucat_dat, overwrite = TRUE)
