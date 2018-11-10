# Suomen väkilukutiedot 1990-

data0 <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_001.px",
                 dims = list(Vuosi = c('1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017'),
                             Sukupuoli = c('S'),
                             Ikä = c('SSS'),
                             Tiedot = c('lkm')),
                 clean = TRUE)

data <- data0

# Remove redundant columns
   data <- data %>% select(Vuosi, values)

# Change the type of vuosi
   data$Vuosi <- as.integer(as.character(data$Vuosi))

# Change names, name values might conflict when joining to other data sets.
   names(data) <- c("Vuosi", "vakiluku")

# name data set
   vakiluku <- data

# Save data
   saveRDS(vakiluku, file = "R/data_clean/vakiluku.rds")
