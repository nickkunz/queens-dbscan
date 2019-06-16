#### settings & requirements ####

## set working directory
setwd("placeholder")

## settings
set.seed(123)          # fix random number for reproduction
options(warn = -1)     # disable warning messages (0 = on, -1 = off)
options(scipen = 999)  # disable scientific notation

## load libraries

## data handling
library(lubridate)   # data cleaning, date / time
library(stringr)     # data cleaning, string whitespace removal
library(data.table)  # data cleaning, rename columns, matrix

## geographic info systems
library(sp)          # spatial join
library(rgdal)       # shapefile handling
library(rgeos)       # centroid
library(fpc)         # dbscan
library(spatstat)    # ripley's k
library(tmaptools)   # geolocation search
library(leaflet)     # interactive mapping

#### data ####

## shapefiles - land lots

## load qoz shapefile - wgs84 projection
qoz <- readOGR("queens_qoz/QN_OZ_trct.shp") 
qoz <- spTransform(qoz, CRS("+proj=longlat +ellps=WGS84"))

## csv data - sales

## load sales data
sales <- read.csv("./queens_sales/all_queens_geo.csv",
                  sep  = ",",                # separator values 
                  header = TRUE,             # create headers
                  check.names = TRUE,        # auto change header names
                  strip.white = TRUE,        # remove leading/trailing whitespace
                  stringsAsFactors = TRUE,   # strings as factor data type
                  na.strings = c("", "NA"))  # assign blank cells na

## remove non-applicable features
sales <- sales[ , c("Sales_subset_real.SALEDATE", 
                    "Queens_Lots_Point.Latitude", 
                    "Queens_Lots_Point.Longitude")]

## rename headers
sales <- setnames(sales,
                  old = c("Sales_subset_real.SALEDATE", 
                          "Queens_Lots_Point.Latitude", 
                          "Queens_Lots_Point.Longitude"),
                  new = c("SALEDATE",
                          "LONGITUDE",
                          "LATITUDE"))

## convert to date type
sales$SALEDATE <- as.Date(ymd(sales$SALEDATE))

## subset data - application filings only, 2015
sales_2015 <- subset(sales,
                     SALEDATE <= "2016-01-01") 

## subset data - application filings only, 2016
sales_2016 <- subset(sales,
                     SALEDATE >= "2016-01-01" &
                         SALEDATE < "2017-01-01" ) 

## subset data - application filings only, 2017
sales_2017 <- subset(sales,
                     SALEDATE >= "2017-01-01" &
                         SALEDATE < "2018-01-01" )  

## subset data - application filings only, 2018
sales_2018 <- subset(sales,
                     SALEDATE >= "2018-01-01")

## create geolocation points
sales_pts <- sales[2:3]
sales_pts_2015 <- sales_2015[2:3]
sales_pts_2016 <- sales_2016[2:3]
sales_pts_2017 <- sales_2017[2:3]
sales_pts_2018 <- sales_2018[2:3]

## csv data - permits
permits <- read.csv("dob_permits.csv",
                    sep  = ",",  # separator values 
                    header = TRUE,  # create headers
                    check.names = TRUE,  # auto change header names
                    strip.white = TRUE,  # remove leading/trailing whitespace
                    stringsAsFactors = TRUE,  # strings as factor data type
                    na.strings = c("", "NA"))  # assign blank cells na

## preview dimensions
dim(permits)

## preview variable names
names(permits)

## preview permit types
unique(permits$Permit.Status)

## preview filing types
unique(permits$Filing.Status)

## subset data - queens only
permits_queens <- subset(permits, 
                         BOROUGH == "QUEENS")

## preview dimensions
dim(permits_queens)

## preview variable names
names(permits_queens)

## preview permit types
unique(permits_queens$Permit.Status)

## preview filing types
unique(permits_queens$Filing.Status)

## subset data - remove non-applicable features
permits_queens <- permits_queens[c("House..",
                                   "Street.Name",
                                   "Block",
                                   "Lot",
                                   "Permit.Status",
                                   "Filing.Status",
                                   "Filing.Date",
                                   "Issuance.Date",
                                   "LATITUDE",
                                   "LONGITUDE",
                                   "NTA_NAME")]

## clean dates - filing date
permits_queens$Filing.Date <- gsub(permits_queens$Filing.Date,
                                   pattern = "12:00:00 AM",
                                   replacement = "",
                                   fixed = TRUE);

## clean dates - issuance date
permits_queens$Issuance.Date <- gsub(permits_queens$Issuance.Date,
                                     pattern = "12:00:00 AM",
                                     replacement = "",
                                     fixed = TRUE);

## convert to date type
permits_queens$Filing.Date <- as.Date(mdy(permits_queens$Filing.Date))
permits_queens$Issuance.Date <- as.Date(mdy(permits_queens$Issuance.Date))

## subset data - date range scope
permits_queens <- subset(permits_queens, 
                         Filing.Date >= "2015-01-01" &
                             Filing.Date < "2019-01-01")    

## subset data - initial filings only
permits_queens <- subset(permits_queens,
                         Filing.Status == "INITIAL")

## remove duplicate observations
permits_queens <- permits_queens[!duplicated(permits_queens), ] 

## remove rows with missing geocoords
permits_queens <- permits_queens[complete.cases(permits_queens$LATITUDE), ] 

## annualize data

## subset data - application filings only, 2015
permits_queens_2015 <- subset(permits_queens,
                              Filing.Date <= "2016-01-01") 

## subset data - application filings only, 2016
permits_queens_2016 <- subset(permits_queens,
                              Filing.Date >= "2016-01-01" &
                                  Filing.Date < "2017-01-01" ) 

## subset data - application filings only, 2017
permits_queens_2017 <- subset(permits_queens,
                              Filing.Date >= "2017-01-01" &
                                  Filing.Date < "2018-01-01" )  

## subset data - application filings only, 2018
permits_queens_2018 <- subset(permits_queens,
                              Filing.Date >= "2018-01-01")

## create geolocated points
permits_queens_pts <- permits_queens[9:10]
permits_queens_pts_2015 <- permits_queens_2015[9:10]
permits_queens_pts_2016 <- permits_queens_2016[9:10]
permits_queens_pts_2017 <- permits_queens_2017[9:10]
permits_queens_pts_2018 <- permits_queens_2018[9:10]

## combine rows of sales and permit activity
sales_permits_pts <- rbind(sales_pts, permits_queens_pts)
sales_permits_pts_2015 <- rbind(sales_pts_2015, permits_queens_pts_2015)
sales_permits_pts_2016 <- rbind(sales_pts_2015, permits_queens_pts_2016)
sales_permits_pts_2017 <- rbind(sales_pts_2015, permits_queens_pts_2017)
sales_permits_pts_2018 <- rbind(sales_pts_2015, permits_queens_pts_2018)

#### dbscan ####

## tuning param epsilon

## determine optimal epsilon
dbscan::kNNdistplot(sales_permits_pts, 
                    k = sqrt(nrow(sales_permits_pts)))

## set epsilon
epsilon <- 0.0066

## draw epsilon line
abline(h = epsilon, lty = 2)

## dbscan

## conduct dbscan - 2015
dbscan_sales_permits_2015 <- 
    fpc::dbscan(sales_permits_pts_2015, 
                eps = epsilon, 
                MinPts = sqrt(nrow(sales_permits_pts))
    );

## conduct dbscan - 2016
dbscan_sales_permits_2016 <- 
    fpc::dbscan(sales_permits_pts_2016, 
                eps = epsilon, 
                MinPts = sqrt(nrow(sales_permits_pts))
    );

## conduct dbscan - 2017
dbscan_sales_permits_2017 <- 
    fpc::dbscan(sales_permits_pts_2017, 
                eps = epsilon, 
                MinPts = sqrt(nrow(sales_permits_pts))
    );

## conduct dbscan - 2018
dbscan_sales_permits_2018 <- 
    fpc::dbscan(sales_permits_pts_2018, 
                eps = epsilon, 
                MinPts = sqrt(nrow(sales_permits_pts))
    );

## duplicate points data frame
dbscan_results_2015 <- sales_permits_pts_2015 
dbscan_results_2016 <- sales_permits_pts_2016
dbscan_results_2017 <- sales_permits_pts_2017 
dbscan_results_2018 <- sales_permits_pts_2018 

## cluster identification - 2015
dbscan_results_2015$CLUSTERID <- dbscan_sales_permits_2015$cluster
dbscan_results_2015$SEEDCLUSTER <- dbscan_sales_permits_2015$isseed

## cluster identification - 2016
dbscan_results_2016$CLUSTERID <- dbscan_sales_permits_2016$cluster
dbscan_results_2016$SEEDCLUSTER <- dbscan_sales_permits_2016$isseed

## cluster identification - 2017
dbscan_results_2017$CLUSTERID <- dbscan_sales_permits_2017$cluster
dbscan_results_2017$SEEDCLUSTER <- dbscan_sales_permits_2017$isseed

## cluster identification - 2018
dbscan_results_2018$CLUSTERID <- dbscan_sales_permits_2018$cluster
dbscan_results_2018$SEEDCLUSTER <- dbscan_sales_permits_2018$isseed

## subset clusters only - 2015
dbscan_clusters_2015 <- subset(dbscan_results_2015,
                               CLUSTERID >= 1)

## subset clusters only - 2016
dbscan_clusters_2016 <- subset(dbscan_results_2016,
                               CLUSTERID >= 1)

## subset clusters only - 2017
dbscan_clusters_2017 <- subset(dbscan_results_2017,
                               CLUSTERID >= 1)

## subset clusters only - 2018
dbscan_clusters_2018 <- subset(dbscan_results_2018,
                               CLUSTERID >= 1)

#### visualization ####

## create a color palette
colors <- colorNumeric(palette = "Set1", 
                       domain = dbscan_clusters_2016$CLUSTERID)

## map view centroid search
map_centroid_search <- "KEW GARDENS QUEENS"

## geocode map view centroid
map_view <- geocode_OSM(map_centroid_search, 
                        projection = "WGS84", 
                        return.first.only = TRUE,
                        details = TRUE, 
                        as.data.frame = TRUE, 
                        as.sf = FALSE,
                        server = "http://nominatim.openstreetmap.org")

## initialize map
map <- leaflet() %>%  # run leaflet
    
    ## basemap group
    addProviderTiles(providers$CartoDB.DarkMatter, 
                     group = "Dark Basemap") %>%       # default basemap style 0
    addProviderTiles(providers$CartoDB.DarkMatter, 
                     group = "Hybridized Basemap") %>%
    addProviderTiles(providers$Stamen.TonerBackground,
                     group = "Hybridized Basemap",     # basemap style option 1
                     options = providerTileOptions(opacity = 0.15)) %>%  
    addProviderTiles(providers$Stamen.TonerBackground, 
                     group = "Toner Basemap") %>%      # basemap style option 2
    
    ## default display location
    setView(lng = map_view$x,  # longitude coord
            lat = map_view$y,  # latitude coord
            zoom = 11) %>%     # geographic scale
    
    ## color markers (cluster classified)
    addCircleMarkers(data = dbscan_clusters_2016,
                     ~LONGITUDE, ~LATITUDE,  # geolocation marker
                     fill = TRUE,  # marker fill option
                     radius = 1,  # marker size
                     fillColor = ~colors(CLUSTERID),  # marker fill color
                     fillOpacity = 1,  # marker fill, graphic transparency
                     weight = 0,  # marker stroke weight (outline)
                     opacity = 1,  # marker stroke (outline), graphic transparency
                     color = ~colors(CLUSTERID)) %>%
    
    ## color markers
    addCircleMarkers(data = permits_queens,
                     ~LONGITUDE, ~LATITUDE,  # geolocation marker
                     fill = TRUE,  # marker fill option
                     radius = 1,  # marker size
                     fillColor = "white",  # marker fill color
                     fillOpacity = 0.05,  # marker fill, graphic transparency
                     weight = 0,  # marker stroke weight (outline)
                     opacity = 1,  # marker stroke (outline), graphic transparency
                     color = "white") %>%
    
    ## basemap control toggle
    addLayersControl(baseGroups = c("Dark Basemap",       # default basemap style 0
                                    "Hybridized Basemap",  # optional basemap style 1
                                    "Toner Basemap"),  # optional basemap style 2
                     options = layersControlOptions(collapsed = FALSE)) %>%
    
    ## add scale bar
    addScaleBar(position = "bottomleft")

## display map
map

## export data

## export sales points to csv
write.csv(sales_pts, file = "sales_pts.csv")

## export permits points to csv
write.csv(permits_queens_pts, file = "permits_pts.csv")

## export dbscan results to csv - 2015
write.csv(dbscan_clusters_2015, file = "dbscan_clusters_2015.csv")

## export dbscan results to csv - 2016
write.csv(dbscan_clusters_2016, file = "dbscan_clusters_2016.csv")

## export dbscan results to csv - 2017
write.csv(dbscan_clusters_2017, file = "dbscan_clusters_2017.csv")

## export dbscan results to csv - 2018
write.csv(dbscan_clusters_2018, file = "dbscan_clusters_2018.csv")

## exported data utilized in ArcGIS in order to conduct the remaining spatial analysis