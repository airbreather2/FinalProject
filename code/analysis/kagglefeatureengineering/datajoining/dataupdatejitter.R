library(terra)        # For raster data handling (rast, extract functions)
library(dplyr)
# extracting temperature and precipitation 

# Load wheat trial data

dat <- read.csv("../../../../data/finaldatasets/testdata/jittered_treedata.csv")
names(dat)

df <- dat[,c("id","Continent","Country","Location","longitude_jittered","latitude_jittered","Observation.period",
             "Grain.yield..tons.ha.1.","start_date","end_date")]

df_coords <- df[,c("longitude_jittered","latitude_jittered")]

temp <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/temperature_1979_2022.nc")
prc <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/total_precipitation_1979_2022.nc")

#Examine PRC rASTER
print(prc)
terra::nlyr(prc)  # Should be 528 layers (44 years × 12 months)
terra::names(prc)  # Check layer namesno 
terra::global(prc, "range", na.rm=TRUE)  # Check data ranges - should not be all zeros



temp_coords <- terra::extract(temp, df_coords)
prc_coords <- terra::extract(prc, df_coords)

df_weather <- cbind(temp_coords, prc_coords)
# rename the new column of temperature and precipitation 

years <- 1979:2022
months_1 <- paste0("t", 1:12)
column_names_1 <- as.vector(outer(months_1, years, paste, sep = "-"))

# Rename columns 13 to 540
colnames(df_weather)[2:529] <- column_names_1

months_2 <- paste0("p", 1:12)
column_names_2 <- as.vector(outer(months_2, years, paste, sep = "-"))

# Rename columns 13 to 540
colnames(df_weather)[531:1058] <- column_names_2


df_w <- cbind(df,df_weather)
colnames(df_w)[7] <- "year"
#openxlsx::write.xlsx(df_w, here::here("data/wheat_data_with_weather_data.xlsx"))


# splitting data by year crop calendar
unique(df_w$start_date)

index_year_80 <- c(12:35,541:564)  # set months September 1979 to December 1980 as an index
index_month_5 <- c(15:23, 39:47) # for months 6
#index_month_6 <- c(16:24, 40:48) # for months 6
index_month_6 <- index_month_5 + 1 # for months 6
index_month_9 <- index_month_6 + 3 # for month 9
index_month_10 <- index_month_9 + 1  # for months 10
index_month_11 <- index_month_10 + 1 # for months 11
index_month_12 <- index_month_11 + 1  # for months 12
index_month_1 <- index_month_12 + 1 # for months 1

# 1980
w_1980 <- df_w[df_w$year==1980,]
unique(w_1980$start_date)
w_1980 <- w_1980[,c(1:10,index_year_80)]

w_1980_11 <- w_1980[w_1980$start_date==11,]
w_1980_11 <- w_1980_11[,c(1:10,index_month_11)]
colnames(w_1980_11) <- c("id","continent", "country","location", "Conversion.for.longitude", "Conversion.for.latitude", "year",
                         "yield", "start_date","end_date","temp1","temp2","temp3", "temp4",
                         "temp5", "temp6","temp7","temp8","temp9", "prc1", "prc2",
                         "prc3", "prc4", "prc5", "prc6", "prc7", "prc8", "prc9")
w_1980 <- w_1980_11

# 1981
w_1981 <- df_w[df_w$year==1981,]
unique(w_1981$start_date)
index_81 <- index_year_80 + 12
w_1981 <- w_1981[,c(1:10,index_81)]

w_1981_11 <- w_1981[w_1981$start_date==11,]
w_1981_11 <- w_1981_11[,c(1:10,index_month_11)]
colnames(w_1981_11) <- colnames(w_1980_11)

w_1981_10 <- w_1981[w_1981$start_date==10,]
w_1981_10 <- w_1981_10[,c(1:10,index_month_10)]
colnames(w_1981_10) <- colnames(w_1980_11)

w_1981 <- rbind(w_1981_10,w_1981_11)

# 1982 
w_1982 <- df_w[df_w$year==1982,]
unique(w_1982$start_date)
index_82 <- index_81 + 12
w_1982 <- w_1982[,c(1:10,index_82)]

w_1982_11 <- w_1982[w_1982$start_date==11,]
w_1982_11 <- w_1982_11[,c(1:10,index_month_11)]
colnames(w_1982_11) <- colnames(w_1980_11)

w_1982_10 <- w_1982[w_1982$start_date==10,]
w_1982_10 <- w_1982_10[,c(1:10,index_month_10)]
colnames(w_1982_10) <- colnames(w_1980_11)

w_1982 <- rbind(w_1982_10,w_1982_11)


# 1983
w_1983 <- df_w[df_w$year==1983,]
unique(w_1983$start_date)
index_83 <- index_82 + 12
w_1983 <- w_1983[,c(1:10,index_83)]

w_1983_11 <- w_1983[w_1983$start_date==11,]
w_1983_11 <- w_1983_11[,c(1:10,index_month_11)]
colnames(w_1983_11) <- colnames(w_1980_11)

w_1983_10 <- w_1983[w_1983$start_date==10,]
w_1983_10 <- w_1983_10[,c(1:10,index_month_10)]
colnames(w_1983_10) <- colnames(w_1980_11)

w_1983 <- rbind(w_1983_10,w_1983_11)


# 1984
w_1984 <- df_w[df_w$year==1984,]
unique(w_1984$start_date)
index_84 <- index_83 + 12
w_1984 <- w_1984[,c(1:10,index_84)]

w_1984_11 <- w_1984[w_1984$start_date==11,]
w_1984_11 <- w_1984_11[,c(1:10,index_month_11)]
colnames(w_1984_11) <- colnames(w_1980_11)

w_1984_10 <- w_1984[w_1984$start_date==10,]
w_1984_10 <- w_1984_10[,c(1:10,index_month_10)]
colnames(w_1984_10) <- colnames(w_1980_11)

w_1984 <- rbind(w_1984_10,w_1984_11)


# 1985
w_1985 <- df_w[df_w$year==1985,]
unique(w_1985$start_date)
index_85 <- index_84 + 12
w_1985 <- w_1985[,c(1:10,index_85)]

w_1985_11 <- w_1985[w_1985$start_date==11,]
w_1985_11 <- w_1985_11[,c(1:10,index_month_11)]
colnames(w_1985_11) <- colnames(w_1980_11)

w_1985_10 <- w_1985[w_1985$start_date==10,]
w_1985_10 <- w_1985_10[,c(1:10,index_month_10)]
colnames(w_1985_10) <- colnames(w_1980_11)

w_1985 <- rbind(w_1985_10,w_1985_11)

# 1986
w_1986 <- df_w[df_w$year==1986,]
unique(w_1986$start_date)
index_86 <- index_85 + 12
w_1986 <- w_1986[,c(1:10,index_86)]

w_1986_11 <- w_1986[w_1986$start_date==11,]
w_1986_11 <- w_1986_11[,c(1:10,index_month_11)]
colnames(w_1986_11) <- colnames(w_1980_11)

w_1986_10 <- w_1986[w_1986$start_date==10,]
w_1986_10 <- w_1986_10[,c(1:10,index_month_10)]
colnames(w_1986_10) <- colnames(w_1980_11)

w_1986 <- rbind(w_1986_10,w_1986_11)

# 1987
w_1987 <- df_w[df_w$year==1987,]
unique(w_1987$start_date)
index_87 <- index_86 + 12
w_1987 <- w_1987[,c(1:10,index_87)]

w_1987_11 <- w_1987[w_1987$start_date==11,]
w_1987_11 <- w_1987_11[,c(1:10,index_month_11)]
colnames(w_1987_11) <- colnames(w_1980_11)

w_1987_10 <- w_1987[w_1987$start_date==10,]
w_1987_10 <- w_1987_10[,c(1:10,index_month_10)]
colnames(w_1987_10) <- colnames(w_1980_11)

w_1987 <- rbind(w_1987_10,w_1987_11)

# 1988
w_1988 <- df_w[df_w$year==1988,]
unique(w_1988$start_date)
index_88 <- index_87 + 12
w_1988 <- w_1988[,c(1:10,index_88)]

w_1988_11 <- w_1988[w_1988$start_date==11,]
w_1988_11 <- w_1988_11[,c(1:10,index_month_11)]
colnames(w_1988_11) <- colnames(w_1980_11)

w_1988_10 <- w_1988[w_1988$start_date==10,]
w_1988_10 <- w_1988_10[,c(1:10,index_month_10)]
colnames(w_1988_10) <- colnames(w_1980_11)

w_1988 <- rbind(w_1988_10,w_1988_11)

# 1989
w_1989 <- df_w[df_w$year==1989,]
unique(w_1989$start_date)
index_89 <- index_88 + 12
w_1989 <- w_1989[,c(1:10,index_89)]

w_1989_11 <- w_1989[w_1989$start_date==11,]
w_1989_11 <- w_1989_11[,c(1:10,index_month_11)]
colnames(w_1989_11) <- colnames(w_1980_11)

w_1989_10 <- w_1989[w_1989$start_date==10,]
w_1989_10 <- w_1989_10[,c(1:10,index_month_10)]
colnames(w_1989_10) <- colnames(w_1980_11)

w_1989 <- rbind(w_1989_10,w_1989_11)

# 1990
w_1990 <- df_w[df_w$year==1990,]
unique(w_1990$start_date)
index_90 <- index_89 + 12
w_1990 <- w_1990[,c(1:10,index_90)]

w_1990_11 <- w_1990[w_1990$start_date==11,]
w_1990_11 <- w_1990_11[,c(1:10,index_month_11)]
colnames(w_1990_11) <- colnames(w_1980_11)

w_1990_10 <- w_1990[w_1990$start_date==10,]
w_1990_10 <- w_1990_10[,c(1:10,index_month_10)]
colnames(w_1990_10) <- colnames(w_1980_11)

w_1990 <- rbind(w_1990_10,w_1990_11)

# 1990
w_1991 <- df_w[df_w$year==1991,]
unique(w_1991$start_date)
index_91 <- index_90 + 12
w_1991 <- w_1991[,c(1:10,index_91)]

w_1991_11 <- w_1991[w_1991$start_date==11,]
w_1991_11 <- w_1991_11[,c(1:10,index_month_11)]
colnames(w_1991_11) <- colnames(w_1980_11)

w_1991_10 <- w_1991[w_1991$start_date==10,]
w_1991_10 <- w_1991_10[,c(1:10,index_month_10)]
colnames(w_1991_10) <- colnames(w_1980_11)

w_1991 <- rbind(w_1991_10,w_1991_11)

# 1992
w_1992 <- df_w[df_w$year==1992,]
unique(w_1992$start_date)
index_92 <- index_91 + 12
w_1992 <- w_1992[,c(1:10,index_92)]

w_1992_11 <- w_1992[w_1992$start_date==11,]
w_1992_11 <- w_1992_11[,c(1:10,index_month_11)]
colnames(w_1992_11) <- colnames(w_1980_11)

w_1992_10 <- w_1992[w_1992$start_date==10,]
w_1992_10 <- w_1992_10[,c(1:10,index_month_10)]
colnames(w_1992_10) <- colnames(w_1980_11)

w_1992 <- rbind(w_1992_10,w_1992_11)

# 1993
w_1993 <- df_w[df_w$year==1993,]
unique(w_1993$start_date)
index_93 <- index_92 + 12
w_1993 <- w_1993[,c(1:10,index_93)]

w_1993_11 <- w_1993[w_1993$start_date==11,]
w_1993_11 <- w_1993_11[,c(1:10,index_month_11)]
colnames(w_1993_11) <- colnames(w_1980_11)

w_1993_10 <- w_1993[w_1993$start_date==10,]
w_1993_10 <- w_1993_10[,c(1:10,index_month_10)]
colnames(w_1993_10) <- colnames(w_1980_11)

w_1993 <- rbind(w_1993_10,w_1993_11)

# 1994
w_1994 <- df_w[df_w$year==1994,]
unique(w_1994$start_date)
index_94 <- index_93 + 12
w_1994 <- w_1994[,c(1:10,index_94)]

w_1994_11 <- w_1994[w_1994$start_date==11,]
w_1994_11 <- w_1994_11[,c(1:10,index_month_11)]
colnames(w_1994_11) <- colnames(w_1980_11)

w_1994_10 <- w_1994[w_1994$start_date==10,]
w_1994_10 <- w_1994_10[,c(1:10,index_month_10)]
colnames(w_1994_10) <- colnames(w_1980_11)

w_1994 <- rbind(w_1994_10,w_1994_11)

# 1995
w_1995 <- df_w[df_w$year==1995,]
unique(w_1995$start_date)
index_95 <- index_94 + 12
w_1995 <- w_1995[,c(1:10,index_95)]

w_1995_11 <- w_1995[w_1995$start_date==11,]
w_1995_11 <- w_1995_11[,c(1:10,index_month_11)]
colnames(w_1995_11) <- colnames(w_1980_11)

w_1995_10 <- w_1995[w_1995$start_date==10,]
w_1995_10 <- w_1995_10[,c(1:10,index_month_10)]
colnames(w_1995_10) <- colnames(w_1980_11)

w_1995 <- rbind(w_1995_10,w_1995_11)

# 1996
w_1996 <- df_w[df_w$year==1996,]
unique(w_1996$start_date)
index_96 <- index_95 + 12
w_1996 <- w_1996[,c(1:10,index_96)]

w_1996_11 <- w_1996[w_1996$start_date==11,]
w_1996_11 <- w_1996_11[,c(1:10,index_month_11)]
colnames(w_1996_11) <- colnames(w_1980_11)

w_1996_10 <- w_1996[w_1996$start_date==10,]
w_1996_10 <- w_1996_10[,c(1:10,index_month_10)]
colnames(w_1996_10) <- colnames(w_1980_11)

w_1996 <- rbind(w_1996_10,w_1996_11)

# 1997
w_1997 <- df_w[df_w$year==1997,]
unique(w_1997$start_date)
index_97 <- index_96 + 12
w_1997 <- w_1997[,c(1:10,index_97)]

w_1997_11 <- w_1997[w_1997$start_date==11,]
w_1997_11 <- w_1997_11[,c(1:10,index_month_11)]
colnames(w_1997_11) <- colnames(w_1980_11)

w_1997_10 <- w_1997[w_1997$start_date==10,]
w_1997_10 <- w_1997_10[,c(1:10,index_month_10)]
colnames(w_1997_10) <- colnames(w_1980_11)

w_1997_6 <- w_1997[w_1997$start_date==6,]
w_1997_6 <- w_1997_6[,c(1:10,index_month_6)]
colnames(w_1997_6) <- colnames(w_1980_11)

w_1997 <- rbind(w_1997_6,w_1997_10,w_1997_11)

# 1998
w_1998 <- df_w[df_w$year==1998,]
unique(w_1998$start_date)
index_98 <- index_97+ 12
w_1998 <- w_1998[,c(1:10,index_98)]

w_1998_11 <- w_1998[w_1998$start_date==11,]
w_1998_11 <- w_1998_11[,c(1:10,index_month_11)]
colnames(w_1998_11) <- colnames(w_1980_11)

w_1998_10 <- w_1998[w_1998$start_date==10,]
w_1998_10 <- w_1998_10[,c(1:10,index_month_10)]
colnames(w_1998_10) <- colnames(w_1980_11)

w_1998 <- rbind(w_1998_10,w_1998_11)

# 1999
w_1999 <- df_w[df_w$year==1999,]
unique(w_1999$start_date)
index_99 <- index_98+ 12
w_1999 <- w_1999[,c(1:10,index_99)]

w_1999_11 <- w_1999[w_1999$start_date==11,]
w_1999_11 <- w_1999_11[,c(1:10,index_month_11)]
colnames(w_1999_11) <- colnames(w_1980_11)

w_1999_10 <- w_1999[w_1999$start_date==10,]
w_1999_10 <- w_1999_10[,c(1:10,index_month_10)]
colnames(w_1999_10) <- colnames(w_1980_11)

w_1999_6 <- w_1999[w_1999$start_date==6,]
w_1999_6 <- w_1999_6[,c(1:10,index_month_6)]
colnames(w_1999_6) <- colnames(w_1980_11)


w_1999 <- rbind(w_1999_6,w_1999_10,w_1999_11)

# 2000
w_2000 <- df_w[df_w$year==2000,]
unique(w_2000$start_date)
index_0 <- index_99+ 12
w_2000 <- w_2000[,c(1:10,index_0)]

w_2000_11 <- w_2000[w_2000$start_date==11,]
w_2000_11 <- w_2000_11[,c(1:10,index_month_11)]
colnames(w_2000_11) <- colnames(w_1980_11)

w_2000_10 <- w_2000[w_2000$start_date==10,]
w_2000_10 <- w_2000_10[,c(1:10,index_month_10)]
colnames(w_2000_10) <- colnames(w_1980_11)

w_2000_6 <- w_2000[w_2000$start_date==6,]
w_2000_6 <- w_2000_6[,c(1:10,index_month_6)]
colnames(w_2000_6) <- colnames(w_1980_11)


w_2000 <- rbind(w_2000_6,w_2000_10,w_2000_11)


# 2001
w_2001 <- df_w[df_w$year==2001,]
unique(w_2001$start_date)
index_1 <- index_0+ 12
w_2001 <- w_2001[,c(1:10,index_1)]

w_2001_11 <- w_2001[w_2001$start_date==11,]
w_2001_11 <- w_2001_11[,c(1:10,index_month_11)]
colnames(w_2001_11) <- colnames(w_1980_11)

w_2001_10 <- w_2001[w_2001$start_date==10,]
w_2001_10 <- w_2001_10[,c(1:10,index_month_10)]
colnames(w_2001_10) <- colnames(w_1980_11)

w_2001 <- rbind(w_2001_10,w_2001_11)

# 2002
w_2002 <- df_w[df_w$year==2002,]
unique(w_2002$start_date)
index_2 <- index_1 + 12
w_2002 <- w_2002[,c(1:10,index_2)]

w_2002_11 <- w_2002[w_2002$start_date==11,]
w_2002_11 <- w_2002_11[,c(1:10,index_month_11)]
colnames(w_2002_11) <- colnames(w_1980_11)

w_2002_10 <- w_2002[w_2002$start_date==10,]
w_2002_10 <- w_2002_10[,c(1:10,index_month_10)]
colnames(w_2002_10) <- colnames(w_1980_11)

w_2002_6 <- w_2002[w_2002$start_date==6,]
w_2002_6 <- w_2002_6[,c(1:10,index_month_6)]
colnames(w_2002_6) <- colnames(w_1980_11)

w_2002 <- rbind(w_2002_6,w_2002_10,w_2002_11)


# 2003
w_2003 <- df_w[df_w$year==2003,]
unique(w_2003$start_date)
index_3 <- index_2 + 12
w_2003 <- w_2003[,c(1:10,index_3)]

w_2003_11 <- w_2003[w_2003$start_date==11,]
w_2003_11 <- w_2003_11[,c(1:10,index_month_11)]
colnames(w_2003_11) <- colnames(w_1980_11)

w_2003_10 <- w_2003[w_2003$start_date==10,]
w_2003_10 <- w_2003_10[,c(1:10,index_month_10)]
colnames(w_2003_10) <- colnames(w_1980_11)

w_2003_6 <- w_2003[w_2003$start_date==6,]
w_2003_6 <- w_2003_6[,c(1:10,index_month_6)]
colnames(w_2003_6) <- colnames(w_1980_11)

w_2003 <- rbind(w_2003_6,w_2003_10,w_2003_11)


# 2004
w_2004 <- df_w[df_w$year==2004,]
unique(w_2004$start_date)
index_4 <- index_3 + 12
w_2004 <- w_2004[,c(1:10,index_4)]

w_2004_11 <- w_2004[w_2004$start_date==11,]
w_2004_11 <- w_2004_11[,c(1:10,index_month_11)]
colnames(w_2004_11) <- colnames(w_1980_11)

w_2004_10 <- w_2004[w_2004$start_date==10,]
w_2004_10 <- w_2004_10[,c(1:10,index_month_10)]
colnames(w_2004_10) <- colnames(w_1980_11)

w_2004_9 <- w_2004[w_2004$start_date==9,]
w_2004_9 <- w_2004_9[,c(1:10,index_month_9)]
colnames(w_2004_9) <- colnames(w_1980_11)

w_2004 <- rbind(w_2004_9,w_2004_10,w_2004_11)


# 2005
w_2005 <- df_w[df_w$year==2005,]
unique(w_2005$start_date)
index_5 <- index_4 + 12
w_2005 <- w_2005[,c(1:10,index_5)]

w_2005_11 <- w_2005[w_2005$start_date==11,]
w_2005_11 <- w_2005_11[,c(1:10,index_month_11)]
colnames(w_2005_11) <- colnames(w_1980_11)

w_2005_10 <- w_2005[w_2005$start_date==10,]
w_2005_10 <- w_2005_10[,c(1:10,index_month_10)]
colnames(w_2005_10) <- colnames(w_1980_11)

w_2005_9 <- w_2005[w_2005$start_date==9,]
w_2005_9 <- w_2005_9[,c(1:10,index_month_9)]
colnames(w_2005_9) <- colnames(w_1980_11)

w_2005 <- rbind(w_2005_9,w_2005_10,w_2005_11)


# 2006
w_2006 <- df_w[df_w$year==2006,]
unique(w_2006$start_date)
index_6 <- index_5 + 12
w_2006 <- w_2006[,c(1:10,index_6)]

w_2006_11 <- w_2006[w_2006$start_date==11,]
w_2006_11 <- w_2006_11[,c(1:10,index_month_11)]
colnames(w_2006_11) <- colnames(w_1980_11)

w_2006_10 <- w_2006[w_2006$start_date==10,]
w_2006_10 <- w_2006_10[,c(1:10,index_month_10)]
colnames(w_2006_10) <- colnames(w_1980_11)

w_2006_9 <- w_2006[w_2006$start_date==9,]
w_2006_9 <- w_2006_9[,c(1:10,index_month_9)]
colnames(w_2006_9) <- colnames(w_1980_11)

w_2006 <- rbind(w_2006_9,w_2006_10,w_2006_11)

# 2007
w_2007 <- df_w[df_w$year==2007,]
unique(w_2007$start_date)
index_7 <- index_6 + 12
w_2007 <- w_2007[,c(1:10,index_7)]

w_2007_11 <- w_2007[w_2007$start_date==11,]
w_2007_11 <- w_2007_11[,c(1:10,index_month_11)]
colnames(w_2007_11) <- colnames(w_1980_11)

w_2007_10 <- w_2007[w_2007$start_date==10,]
w_2007_10 <- w_2007_10[,c(1:10,index_month_10)]
colnames(w_2007_10) <- colnames(w_1980_11)

w_2007_9 <- w_2007[w_2007$start_date==9,]
w_2007_9 <- w_2007_9[,c(1:10,index_month_9)]
colnames(w_2007_9) <- colnames(w_1980_11)

w_2007_6 <- w_2007[w_2007$start_date==6,]
w_2007_6 <- w_2007_6[,c(1:10,index_month_6)]
colnames(w_2007_6) <- colnames(w_1980_11)

w_2007 <- rbind(w_2007_6,w_2007_9,w_2007_10,w_2007_11)

# 2008
w_2008 <- df_w[df_w$year==2008,]
unique(w_2008$start_date)
index_8 <- index_7 + 12
w_2008 <- w_2008[,c(1:10,index_8)]

w_2008_11 <- w_2008[w_2008$start_date==11,]
w_2008_11 <- w_2008_11[,c(1:10,index_month_11)]
colnames(w_2008_11) <- colnames(w_1980_11)

w_2008_10 <- w_2008[w_2008$start_date==10,]
w_2008_10 <- w_2008_10[,c(1:10,index_month_10)]
colnames(w_2008_10) <- colnames(w_1980_11)

w_2008_9 <- w_2008[w_2008$start_date==9,]
w_2008_9 <- w_2008_9[,c(1:10,index_month_9)]
colnames(w_2008_9) <- colnames(w_1980_11)


w_2008 <- rbind(w_2008_9,w_2008_10,w_2008_11)

# 2009
w_2009 <- df_w[df_w$year==2009,]
unique(w_2009$start_date)
index_9 <- index_8 + 12
w_2009 <- w_2009[,c(1:10,index_9)]

w_2009_11 <- w_2009[w_2009$start_date==11,]
w_2009_11 <- w_2009_11[,c(1:10,index_month_11)]
colnames(w_2009_11) <- colnames(w_1980_11)

w_2009_10 <- w_2009[w_2009$start_date==10,]
w_2009_10 <- w_2009_10[,c(1:10,index_month_10)]
colnames(w_2009_10) <- colnames(w_1980_11)

w_2009_9 <- w_2009[w_2009$start_date==9,]
w_2009_9 <- w_2009_9[,c(1:10,index_month_9)]
colnames(w_2009_9) <- colnames(w_1980_11)

w_2009_6 <- w_2009[w_2009$start_date==6,]
w_2009_6 <- w_2009_6[,c(1:10,index_month_6)]
colnames(w_2009_6) <- colnames(w_1980_11)


w_2009 <- rbind(w_2009_6,w_2009_9,w_2009_10,w_2009_11)


# 2010
w_2010 <- df_w[df_w$year==2010,]
unique(w_2010$start_date)
index_10 <- index_9 + 12
w_2010 <- w_2010[,c(1:10,index_10)]

w_2010_11 <- w_2010[w_2010$start_date==11,]
w_2010_11 <- w_2010_11[,c(1:10,index_month_11)]
colnames(w_2010_11) <- colnames(w_1980_11)

w_2010_10 <- w_2010[w_2010$start_date==10,]
w_2010_10 <- w_2010_10[,c(1:10,index_month_10)]
colnames(w_2010_10) <- colnames(w_1980_11)

w_2010_9 <- w_2010[w_2010$start_date==9,]
w_2010_9 <- w_2010_9[,c(1:10,index_month_9)]
colnames(w_2010_9) <- colnames(w_1980_11)


w_2010 <- rbind(w_2010_9,w_2010_10,w_2010_11)


# 2011
w_2011 <- df_w[df_w$year==2011,]
unique(w_2011$start_date)
index_11 <- index_10 + 12
w_2011 <- w_2011[,c(1:10,index_11)]

w_2011_12 <- w_2011[w_2011$start_date==12,]
w_2011_12 <- w_2011_12[,c(1:10,index_month_12)]
colnames(w_2011_12) <- colnames(w_1980_11)

w_2011_11 <- w_2011[w_2011$start_date==11,]
w_2011_11 <- w_2011_11[,c(1:10,index_month_11)]
colnames(w_2011_11) <- colnames(w_1980_11)

w_2011_10 <- w_2011[w_2011$start_date==10,]
w_2011_10 <- w_2011_10[,c(1:10,index_month_10)]
colnames(w_2011_10) <- colnames(w_1980_11)

w_2011_9 <- w_2011[w_2011$start_date==9,]
w_2011_9 <- w_2011_9[,c(1:10,index_month_9)]
colnames(w_2011_9) <- colnames(w_1980_11)

w_2011_6 <- w_2011[w_2011$start_date==6,]
w_2011_6 <- w_2011_6[,c(1:10,index_month_6)]
colnames(w_2011_6) <- colnames(w_1980_11)

w_2011_5 <- w_2011[w_2011$start_date==5,]
w_2011_5 <- w_2011_5[,c(1:10,index_month_5)]
colnames(w_2011_5) <- colnames(w_1980_11)

w_2011 <- rbind(w_2011_5,w_2011_6,w_2011_9,w_2011_10, w_2011_11, w_2011_12)


# 2012
w_2012 <- df_w[df_w$year==2012,]
unique(w_2012$start_date)
index_12 <- index_11 + 12
w_2012 <- w_2012[,c(1:10,index_12)]

w_2012_12 <- w_2012[w_2012$start_date==12,]
w_2012_12 <- w_2012_12[,c(1:10,index_month_12)]
colnames(w_2012_12) <- colnames(w_1980_11)

w_2012_11 <- w_2012[w_2012$start_date==11,]
w_2012_11 <- w_2012_11[,c(1:10,index_month_11)]
colnames(w_2012_11) <- colnames(w_1980_11)

w_2012_10 <- w_2012[w_2012$start_date==10,]
w_2012_10 <- w_2012_10[,c(1:10,index_month_10)]
colnames(w_2012_10) <- colnames(w_1980_11)

w_2012_9 <- w_2012[w_2012$start_date==9,]
w_2012_9 <- w_2012_9[,c(1:10,index_month_9)]
colnames(w_2012_9) <- colnames(w_1980_11)

w_2012_6 <- w_2012[w_2012$start_date==6,]
w_2012_6 <- w_2012_6[,c(1:10,index_month_6)]
colnames(w_2012_6) <- colnames(w_1980_11)

w_2012 <- rbind(w_2012_6,w_2012_9,w_2012_10, w_2012_11, w_2012_12)


# 2013
w_2013 <- df_w[df_w$year==2013,]
unique(w_2013$start_date)
index_13 <- index_12 + 12
w_2013 <- w_2013[,c(1:10,index_13)]

w_2013_12 <- w_2013[w_2013$start_date==12,]
w_2013_12 <- w_2013_12[,c(1:10,index_month_12)]
colnames(w_2013_12) <- colnames(w_1980_11)

w_2013_11 <- w_2013[w_2013$start_date==11,]
w_2013_11 <- w_2013_11[,c(1:10,index_month_11)]
colnames(w_2013_11) <- colnames(w_1980_11)

w_2013_10 <- w_2013[w_2013$start_date==10,]
w_2013_10 <- w_2013_10[,c(1:10,index_month_10)]
colnames(w_2013_10) <- colnames(w_1980_11)

w_2013_9 <- w_2013[w_2013$start_date==9,]
w_2013_9 <- w_2013_9[,c(1:10,index_month_9)]
colnames(w_2013_9) <- colnames(w_1980_11)

w_2013_6 <- w_2013[w_2013$start_date==6,]
w_2013_6 <- w_2013_6[,c(1:10,index_month_6)]
colnames(w_2013_6) <- colnames(w_1980_11)

w_2013_5 <- w_2013[w_2013$start_date==5,]
w_2013_5 <- w_2013_5[,c(1:10,index_month_5)]
colnames(w_2013_5) <- colnames(w_1980_11)

w_2013 <- rbind(w_2013_5,w_2013_6,w_2013_9,w_2013_10, w_2013_11, w_2013_12)


# 2014
w_2014 <- df_w[df_w$year==2014,]
unique(w_2014$start_date)
index_14 <- index_13 + 12
w_2014 <- w_2014[,c(1:10,index_14)]

w_2014_12 <- w_2014[w_2014$start_date==12,]
w_2014_12 <- w_2014_12[,c(1:10,index_month_12)]
colnames(w_2014_12) <- colnames(w_1980_11)

w_2014_11 <- w_2014[w_2014$start_date==11,]
w_2014_11 <- w_2014_11[,c(1:10,index_month_11)]
colnames(w_2014_11) <- colnames(w_1980_11)

w_2014_10 <- w_2014[w_2014$start_date==10,]
w_2014_10 <- w_2014_10[,c(1:10,index_month_10)]
colnames(w_2014_10) <- colnames(w_1980_11)

w_2014_9 <- w_2014[w_2014$start_date==9,]
w_2014_9 <- w_2014_9[,c(1:10,index_month_9)]
colnames(w_2014_9) <- colnames(w_1980_11)

w_2014_6 <- w_2014[w_2014$start_date==6,]
w_2014_6 <- w_2014_6[,c(1:10,index_month_6)]
colnames(w_2014_6) <- colnames(w_1980_11)

w_2014_5 <- w_2014[w_2014$start_date==5,]
w_2014_5 <- w_2014_5[,c(1:10,index_month_5)]
colnames(w_2014_5) <- colnames(w_1980_11)

w_2014 <- rbind(w_2014_5,w_2014_6,w_2014_9,w_2014_10, w_2014_11, w_2014_12)

## 2015
w_2015 <- df_w[df_w$year==2015,]
unique(w_2015$start_date)
index_15 <- index_14 + 12
w_2015 <- w_2015[,c(1:10,index_15)]

w_2015_12 <- w_2015[w_2015$start_date==12,]
w_2015_12 <- w_2015_12[,c(1:10,index_month_12)]
colnames(w_2015_12) <- colnames(w_1980_11)

w_2015_11 <- w_2015[w_2015$start_date==11,]
w_2015_11 <- w_2015_11[,c(1:10,index_month_11)]
colnames(w_2015_11) <- colnames(w_1980_11)

w_2015_10 <- w_2015[w_2015$start_date==10,]
w_2015_10 <- w_2015_10[,c(1:10,index_month_10)]
colnames(w_2015_10) <- colnames(w_1980_11)

w_2015_9 <- w_2015[w_2015$start_date==9,]
w_2015_9 <- w_2015_9[,c(1:10,index_month_9)]
colnames(w_2015_9) <- colnames(w_1980_11)

w_2015_6 <- w_2015[w_2015$start_date==6,]
w_2015_6 <- w_2015_6[,c(1:10,index_month_6)]
colnames(w_2015_6) <- colnames(w_1980_11)

w_2015_5 <- w_2015[w_2015$start_date==5,]
w_2015_5 <- w_2015_5[,c(1:10,index_month_5)]
colnames(w_2015_5) <- colnames(w_1980_11)

w_2015 <- rbind(w_2015_5,w_2015_6,w_2015_9,w_2015_10, w_2015_11, w_2015_12)

# 2016
w_2016 <- df_w[df_w$year==2016,]
unique(w_2016$start_date)
index_16 <- index_15 + 12
w_2016 <- w_2016[,c(1:10,index_16)]

w_2016_12 <- w_2016[w_2016$start_date==12,]
w_2016_12 <- w_2016_12[,c(1:10,index_month_12)]
colnames(w_2016_12) <- colnames(w_1980_11)

w_2016_11 <- w_2016[w_2016$start_date==11,]
w_2016_11 <- w_2016_11[,c(1:10,index_month_11)]
colnames(w_2016_11) <- colnames(w_1980_11)

w_2016_10 <- w_2016[w_2016$start_date==10,]
w_2016_10 <- w_2016_10[,c(1:10,index_month_10)]
colnames(w_2016_10) <- colnames(w_1980_11)

w_2016_9 <- w_2016[w_2016$start_date==9,]
w_2016_9 <- w_2016_9[,c(1:10,index_month_9)]
colnames(w_2016_9) <- colnames(w_1980_11)

w_2016_5 <- w_2016[w_2016$start_date==5,]
w_2016_5 <- w_2016_5[,c(1:10,index_month_5)]
colnames(w_2016_5) <- colnames(w_1980_11)

w_2016 <- rbind(w_2016_5,w_2016_9,w_2016_10, w_2016_11, w_2016_12)


# 2017
w_2017 <- df_w[df_w$year==2017,]
unique(w_2017$start_date)
index_17 <- index_16 + 12
w_2017 <- w_2017[,c(1:10,index_17)]

w_2017_12 <- w_2017[w_2017$start_date==12,]
w_2017_12 <- w_2017_12[,c(1:10,index_month_12)]
colnames(w_2017_12) <- colnames(w_1980_11)

w_2017_11 <- w_2017[w_2017$start_date==11,]
w_2017_11 <- w_2017_11[,c(1:10,index_month_11)]
colnames(w_2017_11) <- colnames(w_1980_11)

w_2017_10 <- w_2017[w_2017$start_date==10,]
w_2017_10 <- w_2017_10[,c(1:10,index_month_10)]
colnames(w_2017_10) <- colnames(w_1980_11)

w_2017_9 <- w_2017[w_2017$start_date==9,]
w_2017_9 <- w_2017_9[,c(1:10,index_month_9)]
colnames(w_2017_9) <- colnames(w_1980_11)


w_2017 <- rbind(w_2017_9,w_2017_10, w_2017_11, w_2017_12)


# 2018
w_2018 <- df_w[df_w$year==2018,]
unique(w_2018$start_date)
index_18 <- index_17 + 12
w_2018 <- w_2018[,c(1:10,index_18)]

w_2018_12 <- w_2018[w_2018$start_date==12,]
w_2018_12 <- w_2018_12[,c(1:10,index_month_12)]
colnames(w_2018_12) <- colnames(w_1980_11)

w_2018_11 <- w_2018[w_2018$start_date==11,]
w_2018_11 <- w_2018_11[,c(1:10,index_month_11)]
colnames(w_2018_11) <- colnames(w_1980_11)

w_2018_10 <- w_2018[w_2018$start_date==10,]
w_2018_10 <- w_2018_10[,c(1:10,index_month_10)]
colnames(w_2018_10) <- colnames(w_1980_11)

w_2018_9 <- w_2018[w_2018$start_date==9,]
w_2018_9 <- w_2018_9[,c(1:10,index_month_9)]
colnames(w_2018_9) <- colnames(w_1980_11)


w_2018 <- rbind(w_2018_9,w_2018_10, w_2018_11, w_2018_12)

# 2019
w_2019 <- df_w[df_w$year==2019,]
unique(w_2019$start_date)
index_19 <- index_18 + 12
w_2019 <- w_2019[,c(1:10,index_19)]

w_2019_12 <- w_2019[w_2019$start_date==12,]
w_2019_12 <- w_2019_12[,c(1:10,index_month_12)]
colnames(w_2019_12) <- colnames(w_1980_11)

w_2019_11 <- w_2019[w_2019$start_date==11,]
w_2019_11 <- w_2019_11[,c(1:10,index_month_11)]
colnames(w_2019_11) <- colnames(w_1980_11)

w_2019_10 <- w_2019[w_2019$start_date==10,]
w_2019_10 <- w_2019_10[,c(1:10,index_month_10)]
colnames(w_2019_10) <- colnames(w_1980_11)

w_2019_9 <- w_2019[w_2019$start_date==9,]
w_2019_9 <- w_2019_9[,c(1:10,index_month_9)]
colnames(w_2019_9) <- colnames(w_1980_11)


w_2019 <- rbind(w_2019_9,w_2019_10, w_2019_11, w_2019_12)

## 2020
w_2020 <- df_w[df_w$year==2020,]
unique(w_2020$start_date)
index_20 <- index_19 + 12
w_2020 <- w_2020[,c(1:10,index_20)]

w_2020_11 <- w_2020[w_2020$start_date==11,]
w_2020_11 <- w_2020_11[,c(1:10,index_month_11)]
colnames(w_2020_11) <- colnames(w_1980_11)

w_2020_10 <- w_2020[w_2020$start_date==10,]
w_2020_10 <- w_2020_10[,c(1:10,index_month_10)]
colnames(w_2020_10) <- colnames(w_1980_11)

w_2020_9 <- w_2020[w_2020$start_date==9,]
w_2020_9 <- w_2020_9[,c(1:10,index_month_9)]
colnames(w_2020_9) <- colnames(w_1980_11)


w_2020 <- rbind(w_2020_9,w_2020_10, w_2020_11)

## 2021
w_2021 <- df_w[df_w$year==2021,]
unique(w_2021$start_date)
index_21 <- index_20 + 12
w_2021 <- w_2021[,c(1:10,index_21)]

w_2021_11 <- w_2021[w_2021$start_date==11,]
w_2021_11 <- w_2021_11[,c(1:10,index_month_11)]
colnames(w_2021_11) <- colnames(w_1980_11)

w_2021_10 <- w_2021[w_2021$start_date==10,]
w_2021_10 <- w_2021_10[,c(1:10,index_month_10)]
colnames(w_2021_10) <- colnames(w_1980_11)

w_2021_9 <- w_2021[w_2021$start_date==9,]
w_2021_9 <- w_2021_9[,c(1:10,index_month_9)]
colnames(w_2021_9) <- colnames(w_1980_11)


w_2021 <- rbind(w_2021_9,w_2021_10, w_2021_11)

## 2022
w_2022 <- df_w[df_w$year==2022,]
unique(w_2022$start_date)
index_22 <- index_21 + 12
w_2022 <- w_2022[,c(1:10,index_22)]

w_2022_11 <- w_2022[w_2022$start_date==11,]
w_2022_11 <- w_2022_11[,c(1:10,index_month_11)]
colnames(w_2022_11) <- colnames(w_1980_11)

w_2022_10 <- w_2022[w_2022$start_date==10,]
w_2022_10 <- w_2022_10[,c(1:10,index_month_10)]
colnames(w_2022_10) <- colnames(w_1980_11)

w_2022_9 <- w_2022[w_2022$start_date==9,]
w_2022_9 <- w_2022_9[,c(1:10,index_month_9)]
colnames(w_2022_9) <- colnames(w_1980_11)


w_2022 <- rbind(w_2022_9,w_2022_10, w_2022_11)

df_w2 <- rbind(w_1980,w_1981,w_1982,w_1983,w_1984,w_1985,w_1986,w_1987,
               w_1988,w_1989,w_1990,w_1991,w_1992,w_1993,w_1994,w_1995,
               w_1996,w_1997,w_1998,w_1999,w_2000,w_2001,w_2002,w_2003,
               w_2004,w_2005,w_2006,w_2005,w_2008,w_2009,w_2010,w_2011,
               w_2012,w_2013,w_2014,w_2015,w_2016,w_2017,w_2018,w_2019,
               w_2020,w_2021,w_2022)

#----------------------------------------------------------------------------

# Extract the value of other covariats 

coords <- df_w2[,c("Conversion.for.longitude","Conversion.for.latitude")]

dem <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/elevation_world.tif")
aez <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif")
clay <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/clay_0_30cm.tif")
sand <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/sand_0_30cm.tif")
silt <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/silt_0_30cm.tif")
ph <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/phh2o_0_30cm.tif")
soc <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/soc_0_30cm.tif")
pr_irrigated <- terra::rast("../../../../data/finaldatasets/covariates/Covariates/irrigated_gmia_v5_aei_pct.asc")

nlyr(dem)
nlyr(aez)
nlyr(pr_irrigated)

dem_coords <- terra::extract(dem, coords, ID = FALSE)
aez_coords <- terra::extract(aez,coords, ID = FALSE)
clay_coords <- terra::extract(clay,coords, ID = FALSE)
sand_coords <- terra::extract(sand,coords, ID = FALSE)
silt_coords <- terra::extract(silt,coords, ID = FALSE)
ph_coords <- terra::extract(ph,coords, ID = FALSE)
soc_coords <- terra::extract(soc,coords, ID = FALSE)
pr_irrigated_coords <- terra::extract(pr_irrigated,coords, ID = FALSE)


trial_df <- cbind(df_w2, dem_coords, aez_coords,
                  clay_coords, sand_coords, silt_coords, ph_coords, soc_coords,
                  pr_irrigated_coords)


trial_df <- trial_df %>%
  rename(
    Elevation = elevation_world,
    Soil.organic.carbon..g.C.kg.1. = `soc_0-5cm`,
    Clay = `clay_5-15cm`,
    Silt = `silt_0-5cm`,
    Soil.pH = `phh2o_0-5cm`,
    pr_irrigated = irrigated_gmia_v5_aei_pct,
    Sand = `sand_0-5cm`,
    AEZ = aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg,
    Grain.yield..tons.ha.1. = yield
  )

overlap_cols <- intersect(names(dat), names(trial_df))
print(overlap_cols)
dat[overlap_cols] <- trial_df[overlap_cols]

warnings()


write.csv(dat, ("../../../../data/finaldatasets/testdata/Treedataupdate.csv"))
