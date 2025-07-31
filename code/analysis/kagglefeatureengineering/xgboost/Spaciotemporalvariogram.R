library(dplyr)
library(sp)
library(spacetime)
library(gstat)
library(ggplot2)
library(plotly)


# Load and clean
df_clean <- read.csv("../../../../data/finaldatasets/testdata/XGBoost.csv") %>%
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

# ⬇️ Build spacetime object
coords <- SpatialPoints(df_agg[, c("lon", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))
time_vals <- as.Date(paste0(df_agg$year, "-01-01"))
stidf <- STIDF(coords, time_vals, data = data.frame(yield = df_agg$yield))

# ⬇️ Variogram computation (conservative settings)
vg_st <- variogramST(yield ~ 1, data = stidf,
                     width = 0.05,  # ~1 degree bins for latitude/longitude
                     tlags = seq(0, 15 * 365, by = 365),  # lag of 0, 1, 2 years
                     assumeRegular = FALSE)

#Metrices obtained by the the variogram
summary(vg_st)
head(vg_st)
anyNA(vg_st)
colSums(is.na(vg_st))
table(vg_st$tlag)


#remove NAs
vg_st_clean <- subset(vg_st, !is.na(gamma) & !is.na(dist) & !is.na(np))
# Remove bins with not enough point pairs for statistical reliability
vg_st_clean <- subset(vg_st, !is.na(gamma) & !is.na(dist) & !is.na(np) & np >= 5)


# ⬇️ Plot


ggplot(vg_st_clean, aes(x = dist, y = timelag, fill = gamma)) +
  geom_tile(height = 250, width = 250) +
  scale_fill_viridis_c(option = "D", na.value = "transparent") +
  labs(
    title = "Spatiotemporal Semivariance",
    x = "Distance (km)",
    y = "Time Lag (days)",
    fill = "Gamma"
  ) +
  coord_cartesian(xlim = c(0, 500)) +
  theme_minimal()



plot_ly(vg_st_clean, 
        x = ~dist, 
        y = ~timelag, 
        z = ~gamma, 
        type = "scatter3d", 
        mode = "markers",
        marker = list(
          size = 3,
          color = ~gamma,        # This is the key
          colorscale = "Viridis", # Or "Jet", "Cividis", etc.
          colorbar = list(title = "Gamma"),
          showscale = TRUE
        )) %>%
  layout(
    title = "3D Spatiotemporal Semivariogram",
    scene = list(
      xaxis = list(title = "Distance (km)"),
      yaxis = list(title = "Time Lag (days)"),
      zaxis = list(title = "Gamma", range = c(0, max(vg_st_clean$gamma, na.rm = TRUE)/3))
    )
  )

