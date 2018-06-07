pts <- read.csv("sitesfortemp.csv", stringsAsFactors = FALSE)

## only works on our systems https://github.com/ropensci/bowerbird
library(raadfiles)
files <- oisst_daily_files()
sst <- raster(files$fullname[1], varname = "sst")

library(raster)
## 26 points are in missing value cells, so find nearest non-missing
bad <- is.na(values(sst))
library(RANN)
nearest <- nn2(coordinates(sst)[!bad, ], query = cbind(pts$long, pts$latt), 
               k = 1)
## index of nearest cell in non-missing 
cells_nn <- as.vector(nearest$nn.idx)
## coordinates of nearest cell
xy <- coordinates(sst)[!bad, ][cells_nn, ]
pts$gridx <- xy[,1]
pts$gridy <- xy[,2]
## index of target cell
cells <- cellFromXY(sst, xy)

## now we can get going
months <- format(files$date, "%Y-%m")
filelist <- split(files, months)[unique(months)]
l <- vector("list", length(filelist))
for (i in seq_along(filelist)) {
  sst <- stack(filelist[[i]]$fullname, varname = "sst", quick = TRUE)
  allsst <- extract(sst, cells)
  l[[i]] <- data.frame(pts, minsst = apply(allsst, 1, min),
  maxsst =  apply(allsst, 1, max),
  meansst = apply(allsst, 1, mean), date = filelist[[i]]$date[1])
  print(i)
}

d <- do.call(rbind, l)
write.csv(d, "sitesfortemp_extracted.csv", row.names = FALSE)
file.copy(files$fullname[nrow(files)], ".")
file.copy(files$fullname[findInterval(as.POSIXct("1992-01-01"), files$date)], ".")
saveRDS(d, "sitesfortemp_extracted.rds")
library(ggplot2)
ggplot(d, aes(x = date, y = meansst, group = site_ID, colour = factor(site_ID))) + geom_line() + guides(colour = FALSE)
ggsave("ggplot_sst.png")
ggplot(d %>% group_by(gridx, gridy) %>% summarize(mn = mean(meansst)), 
        aes(gridx,gridy,colour = mn)) + geom_point()
ggsave("ggplot_sst_map.png")
devtools::session_info()

# Session info -----------------------------------------------------------------------------------------------------------------------------
#   setting  value                       
# version  R version 3.5.0 (2018-04-23)
# system   x86_64, linux-gnu           
# ui       RStudio (1.1.442)           
# language (EN)                        
# collate  en_AU.UTF-8                 
# tz       Etc/UTC                     
# date     2018-06-07                  
# 
# Packages ---------------------------------------------------------------------------------------------------------------------------------
#   package     * version    date       source                                                
# assertthat    0.2.0      2017-04-11 CRAN (R 3.5.0)                                        
# base        * 3.5.0      2018-04-23 local                                                 
# bindr         0.1.1      2018-03-13 CRAN (R 3.5.0)                                        
# bindrcpp    * 0.2.2      2018-03-29 CRAN (R 3.5.0)                                        
# codetools     0.2-15     2016-10-05 cran (@0.2-15)                                        
# colorspace    1.3-2      2016-12-14 CRAN (R 3.5.0)                                        
# compiler      3.5.0      2018-04-23 local                                                 
# datasets    * 3.5.0      2018-04-23 local                                                 
# devtools      1.13.5     2018-02-18 CRAN (R 3.5.0)                                        
# digest        0.6.15     2018-01-28 CRAN (R 3.5.0)                                        
# dplyr         0.7.5      2018-05-19 CRAN (R 3.5.0)                                        
# ggplot2     * 2.2.1.9000 2018-05-18 Github (tidyverse/ggplot2@54de616)                    
# glue          1.2.0      2017-10-29 CRAN (R 3.5.0)                                        
# graphics    * 3.5.0      2018-04-23 local                                                 
# grDevices   * 3.5.0      2018-04-23 local                                                 
# grid          3.5.0      2018-04-23 local                                                 
# gtable        0.2.0      2016-02-26 CRAN (R 3.5.0)                                        
# labeling      0.3        2014-08-23 CRAN (R 3.5.0)                                        
# lattice       0.20-35    2017-03-25 cran (@0.20-35)                                       
# lazyeval      0.2.1      2017-10-29 CRAN (R 3.5.0)                                        
# magrittr      1.5        2014-11-22 CRAN (R 3.5.0)                                        
# memoise       1.1.0      2017-04-21 CRAN (R 3.5.0)                                        
# methods     * 3.5.0      2018-04-23 local                                                 
# munsell       0.4.3      2016-02-13 CRAN (R 3.5.0)                                        
# ncdf4         1.16       2017-04-01 CRAN (R 3.5.0)                                        
# parallel      3.5.0      2018-04-23 local                                                 
# pillar        1.2.3      2018-05-25 CRAN (R 3.5.0)                                        
# pkgconfig     2.0.1      2017-03-21 CRAN (R 3.5.0)                                        
# plyr          1.8.4      2016-06-08 CRAN (R 3.5.0)                                        
# prettyunits   1.0.2      2015-07-13 CRAN (R 3.5.0)                                        
# progress      1.1.2      2016-12-14 CRAN (R 3.5.0)                                        
# purrr         0.2.5      2018-05-29 CRAN (R 3.5.0)                                        
# R6            2.2.2      2017-06-17 CRAN (R 3.5.0)                                        
# raadfiles   * 0.0.1.9003 2018-05-09 Github (AustralianAntarcticDivision/raadfiles@183153e)
# RANN        * 2.5.1      2017-05-21 CRAN (R 3.5.0)                                        
# raster      * 2.7-8      2018-06-01 Github (rforge/raster@d454ce4)                        
# Rcpp          0.12.17    2018-05-18 CRAN (R 3.5.0)                                        
# rgdal         1.3-1      2018-06-03 CRAN (R 3.5.0)                                        
# rlang         0.2.1      2018-05-30 CRAN (R 3.5.0)                                        
# RNetCDF       1.9-1      2017-10-08 CRAN (R 3.5.0)                                        
# rstudioapi    0.7        2017-09-07 CRAN (R 3.5.0)                                        
# scales        0.5.0      2017-08-24 CRAN (R 3.5.0)                                        
# sp          * 1.3-1      2018-06-05 CRAN (R 3.5.0)                                        
# stats       * 3.5.0      2018-04-23 local                                                 
# stringi       1.2.2      2018-05-02 CRAN (R 3.5.0)                                        
# stringr       1.3.1      2018-05-10 CRAN (R 3.5.0)                                        
# tibble        1.4.2      2018-01-22 CRAN (R 3.5.0)                                        
# tidyselect    0.2.4      2018-02-26 CRAN (R 3.5.0)                                        
# tools         3.5.0      2018-04-23 local                                                 
# utils       * 3.5.0      2018-04-23 local                                                 
# withr         2.1.2      2018-03-15 CRAN (R 3.5.0)                                        
# yaml          2.1.19     2018-05-01 CRAN (R 3.5.0)  
# 

