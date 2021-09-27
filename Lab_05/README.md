Lab 5
================
Christina Lin
9/24/2021

# Step 1: Read the Data

``` r
library(data.table)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

``` r
if (!file.exists("met_all.gz")){
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)}

met <- data.table::fread("met_all.gz")
```

Merging the data

``` r
met <- merge(
  x = met,
  y = stations,
  all.x = TRUE, all.y = FALSE,
  by.x = "USAFID", by.y = "USAF"
)
```

# Question 1: Representative station for the US

Collapse the data set so there is one representative measurement per
station using averages

``` r
station_averages <- met[,.(
  temp = mean(temp, na.rm=TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE),
  STATE = STATE,
  lat = lat,
  lon = lon
), by = USAFID]
```

Now we need to identify the median per variable

``` r
medians <- station_averages[,.(
  temp_50 = quantile(temp, probs = .5, na.rm = TRUE),
  wind.sp_50 = quantile(wind.sp, probs = .5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs = .5, na.rm=TRUE),
  lat = lat,
  lon = lon,
  STATE = STATE
)]

medians
```

    ##           temp_50 wind.sp_50 atm.press_50    lat      lon STATE
    ##       1: 23.42556   2.309704     1014.734 34.300 -116.166    CA
    ##       2: 23.42556   2.309704     1014.734 34.300 -116.166    CA
    ##       3: 23.42556   2.309704     1014.734 34.300 -116.166    CA
    ##       4: 23.42556   2.309704     1014.734 34.300 -116.166    CA
    ##       5: 23.42556   2.309704     1014.734 34.300 -116.166    CA
    ##      ---                                                       
    ## 2377339: 23.42556   2.309704     1014.734 43.650 -116.633    ID
    ## 2377340: 23.42556   2.309704     1014.734 43.650 -116.633    ID
    ## 2377341: 23.42556   2.309704     1014.734 43.650 -116.633    ID
    ## 2377342: 23.42556   2.309704     1014.734 43.642 -116.636    ID
    ## 2377343: 23.42556   2.309704     1014.734 43.642 -116.636    ID

Now we can find the stations that are closest to these \#which.min()
returns the index of the observation

``` r
station_averages[, temp_dist := abs(temp - medians$temp_50)]
median_temp_station <- station_averages[order(temp_dist)][1]
median_temp_station
```

    ##    USAFID     temp  wind.sp atm.press STATE    lat     lon temp_dist
    ## 1: 723010 23.42556 1.641749  1014.729    NC 35.742 -81.382         0

The station that is closest to the median temperature is 723010.

``` r
station_averages[, wind.sp_dist := abs(wind.sp - medians$wind.sp_50)]
median_wind_station <- station_averages[order(wind.sp_dist)][1]
median_wind_station
```

    ##    USAFID     temp  wind.sp atm.press STATE    lat     lon temp_dist
    ## 1: 722143 29.54605 2.309704       NaN    TX 32.579 -96.719  6.120484
    ##    wind.sp_dist
    ## 1:            0

The station that is closest to the median wind speed is 722143.

``` r
station_averages[, atm.press_dist := abs(atm.press - medians$atm.press_50)]
median_atm_station <- station_averages[order(atm.press_dist)][1]
median_atm_station
```

    ##    USAFID     temp  wind.sp atm.press STATE    lat     lon temp_dist
    ## 1: 722316 27.32591 2.014424  1014.734    LA 29.817 -90.017  3.900351
    ##    wind.sp_dist atm.press_dist
    ## 1:    0.2952794              0

The station that is closest to the median atmospheric pressure is
722316.

The representative/median stations for temperature, wind speed, and
atmospheric pressure do no coincide.

# Question 2:

\#First, need to recover the state variable by merging

``` r
#station_averages <- merge(x = station_averages, y = stations, by.x = "USAFID", by.y = "USAF", all.x = #TRUE, all.y = FALSE)
```

``` r
station_averages[, temp_50 := quantile(temp, probs= .5, na.rm = TRUE), by = STATE]
station_averages[, wind_50 := quantile(wind.sp, probs= .5, na.rm = TRUE), by = STATE]
station_averages[, atm_50 := quantile(atm.press, probs= .5, na.rm = TRUE), by = STATE]
```

``` r
station_averages[, eudist := sqrt(
  (temp - temp_50)^2 + (wind.sp - wind_50)^2 + (atm.press - atm_50)^2
)]

state_median_station <- station_averages[, .SD[which.min(eudist)], by = STATE]
state_median_station[, .(STATE, USAFID, lat, lon)]
```

    ##     STATE USAFID    lat      lon
    ##  1:    CA 722977 33.680 -117.866
    ##  2:    TX 722540 30.300  -97.700
    ##  3:    MI 726355 42.126  -86.428
    ##  4:    SC 723190 34.498  -82.710
    ##  5:    IL 725440 41.450  -90.500
    ##  6:    MO 723495 37.152  -94.495
    ##  7:    AR 723407 35.831  -90.646
    ##  8:    OR 720365 42.070 -124.290
    ##  9:    GA 723160 31.536  -82.507
    ## 10:    MN 726550 45.543  -94.051
    ## 11:    AL 722286 33.212  -87.616
    ## 12:    IN 725330 40.983  -85.200
    ## 13:    NC 723174 36.047  -79.477
    ## 14:    VA 724016 38.137  -78.455
    ## 15:    IA 725480 42.550  -92.400
    ## 16:    PA 725130 41.333  -75.717
    ## 17:    NE 725527 41.764  -96.178
    ## 18:    ID 722142 44.523 -114.215
    ## 19:    WI 726416 43.212  -90.181
    ## 20:    WV 724176 39.643  -79.916
    ## 21:    MD 724057 39.472  -76.170
    ## 22:    AZ 723740 35.017 -110.733
    ## 23:    OK 723537 35.852  -97.414
    ## 24:    WY 725645 41.317 -105.683
    ## 25:    LA 722486 32.516  -92.041
    ## 26:    KY 724243 37.087  -84.077
    ## 27:    FL 722210 30.483  -86.517
    ## 28:    CO 724665 39.275 -103.666
    ## 29:    OH 725254 41.338  -84.429
    ## 30:    NJ 724075 39.366  -75.078
    ## 31:    NM 723650 35.033 -106.617
    ## 32:    KS 724580 39.550  -97.650
    ## 33:    VT 726114 44.534  -72.614
    ## 34:    MS 723306 33.650  -88.450
    ## 35:    CT 725087 41.736  -72.651
    ## 36:    NV 725830 40.900 -117.800
    ## 37:    UT 725720 40.783 -111.950
    ## 38:    SD 726590 45.450  -98.417
    ## 39:    TN 723346 35.593  -88.917
    ## 40:    NY 725194 42.643  -77.056
    ## 41:    RI 725079 41.533  -71.283
    ## 42:    MA 725064 41.910  -70.729
    ## 43:    DE 724180 39.674  -75.606
    ## 44:    NH 726050 43.200  -71.500
    ## 45:    ME 726077 44.450  -68.367
    ## 46:    MT 726797 45.788 -111.160
    ##     STATE USAFID    lat      lon

This table shows the representative station of each state, calculated by
using the lowest euclidean distance for temperature, wind speed, and
atmospheric pressure.

# Question 3

Finding the station at the middle of the state can be done by finding
the station closest to the median latitude and longitude in each state.

``` r
station_averages[, lat_50 := quantile(lat, probs = .5, na.rm = TRUE), by = STATE]
station_averages[, lon_50 := quantile(lon, probs = .5, na.rm = TRUE), by = STATE]

station_averages[, eudist_location := sqrt(
  (lat - lat_50)^2 + (lon - lon_50)^2)]


state_middle_station <- station_averages[, .SD[which.min(eudist_location)], by = STATE]
state_middle_station[, .(STATE, USAFID, lat, lon)]
```

    ##     STATE USAFID    lat      lon
    ##  1:    CA 724815 37.285 -120.512
    ##  2:    TX 722570 31.150  -97.717
    ##  3:    MI 725405 43.322  -84.688
    ##  4:    SC 720603 34.283  -80.567
    ##  5:    IL 724397 40.477  -88.916
    ##  6:    MO 724453 38.704  -93.183
    ##  7:    AR 723429 35.259  -93.093
    ##  8:    OR 725975 42.600 -123.364
    ##  9:    WA 720388 47.104 -122.287
    ## 10:    GA 722217 32.564  -82.985
    ## 11:    MN 726583 45.097  -94.507
    ## 12:    AL 722300 33.177  -86.783
    ## 13:    IN 720961 40.711  -86.375
    ## 14:    NC 722201 35.584  -79.101
    ## 15:    VA 720498 37.400  -77.517
    ## 16:    IA 725466 41.691  -93.566
    ## 17:    PA 725118 40.217  -76.851
    ## 18:    NE 725513 40.893  -97.997
    ## 19:    ID 726810 43.567 -116.240
    ## 20:    WI 726465 44.778  -89.667
    ## 21:    WV 720328 39.000  -80.274
    ## 22:    MD 724060 39.173  -76.684
    ## 23:    AZ 723745 34.257 -111.339
    ## 24:    OK 723540 35.417  -97.383
    ## 25:    WY 726720 43.064 -108.458
    ## 26:    LA 720468 30.558  -92.099
    ## 27:    KY 720448 37.578  -84.770
    ## 28:    FL 721042 28.228  -82.156
    ## 29:    CO 722061 39.467 -106.150
    ## 30:    OH 720928 40.280  -83.115
    ## 31:    NJ 724090 40.033  -74.353
    ## 32:    NM 722683 33.463 -105.535
    ## 33:    KS 724509 38.068  -97.275
    ## 34:    ND 720867 48.390 -100.024
    ## 35:    VT 726114 44.535  -72.614
    ## 36:    MS 725023 33.761  -90.758
    ## 37:    CT 720545 41.384  -72.506
    ## 38:    NV 724770 39.600 -116.010
    ## 39:    UT 724750 38.427 -113.012
    ## 40:    SD 726530 43.767  -99.318
    ## 41:    TN 721031 35.380  -86.246
    ## 42:    NY 725150 42.209  -75.980
    ## 43:    RI 725074 41.597  -71.412
    ## 44:    MA 725068 41.876  -71.021
    ## 45:    DE 724088 39.133  -75.467
    ## 46:    NH 726050 43.205  -71.503
    ## 47:    ME 726185 44.316  -69.797
    ## 48:    MT 726798 45.699 -110.448
    ##     STATE USAFID    lat      lon

Mapping stations from questions 1 - 3 on the same map.

First, preparing data from question 1 for merging. Data points from each
question will be labelled under variable “Q” to indicate which question
it came from.

``` r
median_stations <- rbindlist(list(median_temp_station, median_wind_station, median_atm_station), fill=TRUE)

median_stations[, Q := 1]

state_median_station[, Q := 2]

state_middle_station[, Q:= 3]

all_stations <- rbindlist(list(median_stations, state_median_station, state_middle_station), fill=TRUE)
```

``` r
library(leaflet)

temp.pal <- colorFactor(palette=c("green","red","blue"), domain=all_stations$Q)

leaflet(all_stations) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(
     lat = ~lat, lng=~lon,
     label = ~USAFID, color = ~temp.pal(Q),
     opacity = 1, fillOpacity = 1, radius = 500
     ) %>%
  addLegend('bottomleft', pal= temp.pal, values=all_stations$Q,
          title='Representative Stations from Questions', opacity=1)
```

<div id="htmlwidget-9a4e9654789edee80fc6" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-9a4e9654789edee80fc6">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[35.742,32.579,29.817,33.68,30.3,42.126,34.498,41.45,37.152,35.831,42.07,31.536,45.543,33.212,40.983,36.047,38.137,42.55,41.333,41.764,44.523,43.212,39.643,39.472,35.017,35.852,41.317,32.516,37.087,30.483,39.275,41.338,39.366,35.033,39.55,44.534,33.65,41.736,40.9,40.783,45.45,35.593,42.643,41.533,41.91,39.674,43.2,44.45,45.788,37.285,31.15,43.322,34.283,40.477,38.704,35.259,42.6,47.104,32.564,45.097,33.177,40.711,35.584,37.4,41.691,40.217,40.893,43.567,44.778,39,39.173,34.257,35.417,43.064,30.558,37.578,28.228,39.467,40.28,40.033,33.463,38.068,48.39,44.535,33.761,41.384,39.6,38.427,43.767,35.38,42.209,41.597,41.876,39.133,43.205,44.316,45.699],[-81.382,-96.719,-90.017,-117.866,-97.7,-86.428,-82.71,-90.5,-94.495,-90.646,-124.29,-82.507,-94.051,-87.616,-85.2,-79.477,-78.455,-92.4,-75.717,-96.178,-114.215,-90.181,-79.916,-76.17,-110.733,-97.414,-105.683,-92.041,-84.077,-86.517,-103.666,-84.429,-75.078,-106.617,-97.65,-72.614,-88.45,-72.651,-117.8,-111.95,-98.417,-88.917,-77.056,-71.283,-70.729,-75.606,-71.5,-68.367,-111.16,-120.512,-97.717,-84.688,-80.567,-88.916,-93.183,-93.093,-123.364,-122.287,-82.985,-94.507,-86.783,-86.375,-79.101,-77.517,-93.566,-76.851,-97.997,-116.24,-89.667,-80.274,-76.684,-111.339,-97.383,-108.458,-92.099,-84.77,-82.156,-106.15,-83.115,-74.353,-105.535,-97.275,-100.024,-72.614,-90.758,-72.506,-116.01,-113.012,-99.318,-86.246,-75.98,-71.412,-71.021,-75.467,-71.503,-69.797,-110.448],500,null,null,{"interactive":true,"className":"","stroke":true,"color":["#00FF00","#00FF00","#00FF00","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF"],"weight":5,"opacity":1,"fill":true,"fillColor":["#00FF00","#00FF00","#00FF00","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF"],"fillOpacity":1},null,null,["723010","722143","722316","722977","722540","726355","723190","725440","723495","723407","720365","723160","726550","722286","725330","723174","724016","725480","725130","725527","722142","726416","724176","724057","723740","723537","725645","722486","724243","722210","724665","725254","724075","723650","724580","726114","723306","725087","725830","725720","726590","723346","725194","725079","725064","724180","726050","726077","726797","724815","722570","725405","720603","724397","724453","723429","725975","720388","722217","726583","722300","720961","722201","720498","725466","725118","725513","726810","726465","720328","724060","723745","723540","726720","720468","720448","721042","722061","720928","724090","722683","724509","720867","726114","725023","720545","724770","724750","726530","721031","725150","725074","725068","724088","726050","726185","726798"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]},{"method":"addLegend","args":[{"colors":["#00FF00","#FF0000","#0000FF"],"labels":["1","2","3"],"na_color":null,"na_label":"NA","opacity":1,"position":"bottomleft","type":"factor","title":"Representative Stations from Questions","extra":null,"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[28.228,48.39],"lng":[-124.29,-68.367]}},"evals":[],"jsHooks":[]}</script>

# Question 4

Going back to the met data set

``` r
met[, state_temp := mean(temp, na.rm=TRUE), by = STATE]
met[, temp_cat := fifelse(state_temp < 20, "low-temp",
                          fifelse(state_temp < 25, "mid-temp","high-temp"))]
```

Let’s make sure that we don’t have NAs

``` r
table(met$temp_cat, useNA= "always")
```

    ## 
    ## high-temp  low-temp  mid-temp      <NA> 
    ##    811126    430794   1135423         0

Now let’s summarize

``` r
tab <- met[, .(
  N_entries = .N,
  N_NA = sum(is.na(temp)),
  N_stations = length(unique(USAFID)),
  N_states = length(unique(STATE)),
  temp_mean = mean(temp, na.rm = TRUE),
  wind.sp_mean = mean(wind.sp, na.rm = TRUE),
  atm.press_mean = mean(atm.press, na.rm = TRUE)
),by = temp_cat]


#this prints out a pretty table depending on the format of the document (prints a markdown table)
knitr::kable(tab)
```

| temp\_cat | N\_entries | N\_NA | N\_stations | N\_states | temp\_mean | wind.sp\_mean | atm.press\_mean |
|:----------|-----------:|------:|------------:|----------:|-----------:|--------------:|----------------:|
| mid-temp  |    1135423 | 29252 |         781 |        25 |   22.39909 |      2.352712 |        1014.383 |
| high-temp |     811126 | 23468 |         555 |        12 |   27.75066 |      2.514644 |        1013.738 |
| low-temp  |     430794 |  7369 |         259 |        11 |   18.96446 |      2.637410 |        1014.366 |
