---
title: "Reproducible Research Week 4 Course Project 2"
author: "Erwin Lo"
date: "10/22/2020"
output: 
     html_document:
          keep_md: true
---



# Title: Analysis of the effect of severe weather events to public health and economy.

# Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.  

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.  

Our analysis found that TORNADO is the weather event with the highest damage to public health and economy.  

# Data Processing
Load libraries and download dataset

```r
library(dplyr)
library(ggplot2)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "StormData.csv.bz2", method = "curl")
```

Read dataset into R object and select data that are needed.

```r
data <-
     read.table(
          "StormData.csv.bz2",
          header = TRUE,
          sep = ",",
          na.strings = "",
          nrows = 368797
     )
data <- select(data, EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG)
```

Create a new dataset grouped by event type for the harmful effects to population health.
We will use FATALITIES and INJURIES variables.

```r
harmfulevents <-
     data %>% group_by(EVTYPE) %>% summarise(
          fatalities = sum(FATALITIES, na.rm = TRUE),
          injuries = sum(INJURIES, na.rm = TRUE),
          total = fatalities + injuries
     )
```

Create another dataset for greatest economic consequences. We will use PROPDMG and CROPDMG variables as the proxy for economic damage.

```r
economicdamage <- data %>%
     group_by(EVTYPE) %>% summarise(
          property = sum(PROPDMG, na.rm = TRUE),
          crop = sum(CROPDMG, na.rm = TRUE),
          total = property + crop
     )
```

# Results
#### 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
Let's check the top 10 most harmful events to population health

```r
mostharmful <- head(arrange(harmfulevents, desc(total)), 10) %>% print
```

```
## # A tibble: 10 x 4
##    EVTYPE             fatalities injuries total
##    <chr>                   <dbl>    <dbl> <dbl>
##  1 TORNADO                  4378    75024 79402
##  2 FLOOD                     203     6473  6676
##  3 TSTM WIND                 378     5130  5508
##  4 EXCESSIVE HEAT            736     2582  3318
##  5 LIGHTNING                 341     2172  2513
##  6 ICE STORM                  58     1860  1918
##  7 HEAT                      706      878  1584
##  8 FLASH FLOOD               367      917  1284
##  9 THUNDERSTORM WINDS         64      908   972
## 10 BLIZZARD                   82      777   859
```

**TORNADO** is the most harmful event with the most fatalities and the most injuries.

```r
mostharmful %>% 
     ggplot(aes(x = reorder(EVTYPE, -total), y = total, fill = EVTYPE)) + 
     geom_bar(stat="identity") + 
     ggtitle("Top 10 most harmful weather events to public health") +
     xlab("Event Type") + 
     ylab("Total Harm to population") +
     theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Assignment_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


#### 2. Across the United States, which types of events have the greatest economic consequences?
Let's check the top 10 events with the greatest economic consequences.

```r
mosteconomicdamage <- head(arrange(economicdamage, desc(total)), 10) %>% print
```

```
## # A tibble: 10 x 4
##    EVTYPE             property    crop    total
##    <chr>                 <dbl>   <dbl>    <dbl>
##  1 TORNADO            2286231.  24353. 2310583.
##  2 TSTM WIND           487441.  54124.  541565.
##  3 THUNDERSTORM WINDS  446293.  18685.  464978.
##  4 FLASH FLOOD         411087.  45445.  456532.
##  5 HAIL                226423. 203670.  430093.
##  6 FLOOD               217803.  44517.  262320.
##  7 LIGHTNING           201038.   2134.  203172.
##  8 HIGH WIND            72992.   5104.   78095.
##  9 HEAVY SNOW           56390.   1836.   58225.
## 10 HIGH WINDS           55625    1760.   57385.
```

Again, we see **TORNADO** has the greatest economic damage.

```r
mosteconomicdamage %>%
     ggplot(aes(x = reorder(EVTYPE, -total), y = total, fill = EVTYPE)) + 
     geom_bar(stat="identity") + 
     ggtitle("Top 10 most damaging weather events to economy") +
     xlab("Event Type") + 
     ylab("Total Economic Damage") +
     theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Assignment_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
