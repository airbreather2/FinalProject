# --- Load Packages ---
library(missForest)
library(dplyr)
library(readr)
library(mice)
library(corrplot)

# --- Set Global Options ---
options(digits = 10, scipen = 999)

# --- Load Dataset ---
df_raw <- read_csv("../../../data/finaldatasets/testdata/XGBoost.csv")

# --- Select Relevant Columns (no 'year') ---
df_subset <- df_raw %>%
  select(
    lon = Conversion.for.longitude,
    lat = Conversion.for.latitude,
    yield = `Grain.yield..tons.ha.1.`,
    N_rate = `N.rate..kg.N.ha.1.`,
    starts_with("temp"),
    starts_with("prc"),
    AEZ,
    Soil_N,
    Soil.pH,
    Soil.organic.carbon..g.C.kg.1.,
    Sand,
    Silt,
    Elevation,
    pr_irrigated,
    Wheat.Type,
    Pest.prescence.,
    year,
    gdp_per_capita
  )


summary(df_subset)

###########################Evaluation metrices ###########################################

#assess missingness
par(cex =0.5)
md.pattern(df_subset)
par(cex = 1)

#choose only numeric values
numeric_df <- df_subset[, sapply(df_subset, is.numeric)]
#drop coords and year
numeric_df <- numeric_df[, !(names(numeric_df) %in% c("lon", "lat", "year"))]


# Compute correlation matrix
cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs", method = "pearson", )

# View it
print(cor_matrix)


corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")



df_subset$Wheat.Type <- as.factor(df_subset$Wheat.Type)
df_subset$Pest.prescence. <- as.factor(df_subset$Pest.prescence.)

str(df_subset)

imputed_data <- mice(df_subset, m=1, method = "rf")

completed_data <- complete(imputed_data, action = 1)

str(completed_data)
summary(completed_data)
colSums(is.na(completed_data))  # confirm no NAs

#see the outputs
summary(imputed_data)
imputed_data$imp$N_rate

#assess missingness
par(cex =0.5)
md.pattern(completed_data)
par(cex = 1)


#save dataset 
write.csv(completed_data, "../../../data/finaldatasets/testdata/imputation/imputed_dataset.csv", row.names = FALSE)



