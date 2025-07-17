library(dplyr)
library(sp)
library(spacetime)
library(gstat)

# Load and clean
df_clean <- read.csv("../../../../data/finaldatasets/testdata/xgboost.csv") %>%
  select(lon = Conversion.for.longitude,
         lat = Conversion.for.latitude,
         year,
         yield = Grain.yield..tons.ha.1.) %>%
  na.omit()

# ⬇️ Aggregate by location and year to prevent duplicates
df_agg <- df_clean %>%
  group_by(lon, lat, year) %>%
  summarise(yield = mean(yield), .groups = "drop")

# ⬇️ Optional: downsample for speed
set.seed(1)
df_agg <- df_agg[sample(nrow(df_agg), size = min(10000, nrow(df_agg))), ]

# ⬇️ Build spacetime object
coords <- SpatialPoints(df_agg[, c("lon", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))
time_vals <- as.Date(paste0(df_agg$year, "-01-01"))
stidf <- STIDF(coords, time_vals, data = data.frame(yield = df_agg$yield))

# ⬇️ Variogram computation (conservative settings)
vg_st <- variogramST(yield ~ 1, data = stidf,
                     width = 1,  # ~1 degree bins for latitude/longitude
                     tlags = c(0, 365, 730),  # lag of 0, 1, 2 years
                     assumeRegular = FALSE)

summary(vg_st)
head(vg_st)
anyNA(vg_st)
colSums(is.na(vg_st))

vg_st_clean <- subset(vg_st, !is.na(gamma) & !is.na(dist) & !is.na(np))

# ⬇️ Plot
plot(vg_st_clean, wireframe = TRUE, main = "Spatiotemporal Variogram")


