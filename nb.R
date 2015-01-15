#### overview ####
# intro: automated neibor analysis from shapefile and data aggregator

#### set up ####

## workspace setup
dir.create('neighborhood.analysis') # create a directory 
path.folder<-paste(getwd(),'/neighborhood.analysis',sep="") # create folder
list.of.packages <- c('mailR','tcltk','spdep','dplyr','maptools','foreign')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

## load packages ##
require(tcltk)   # pop ups
require(spdep)   # neighbors
require(dplyr)   # pipes
require(maptools) # read shapefile
require(foreign) # read dbfs

## automation ##
# function that views data file into workbook 

# dialogue windows
# reminder to save shapefile
# reminder to place csv

# input variable 

file <- input.val[1]            # file names
id.var <- input.val[2]          # id variable
metric.control <- input.val[3]  # metric name

# transformation 
post           <- readShapePoly(file.choose(),IDvar = id.var) # read shp
post.nb        <- poly2nb(post)   # find neighbors list
table          <- nb2mat(post.nb, style = 'B') %>% data.frame  # create table of neighbors
attributes1    <- read.dbf(paste(path.folder,'/',file,'.dbf',sep=""))$dbf %>% lapply(as.character) %>% data.frame(stringsAsFactors=F)  

# coerce as dataframe with characters
attributes2    <- read.csv(paste(path.folder,'/',file,'.csv',sep=""),colClasses='character')  # read csv
attributes2    <- attributes2 %>% select(c(1,which(metric.control==names(attributes2))))      # grab the selected attribute
comb           <- left_join(attributes1, attributes2, by = id.var)                            # join two attribute tables
names(table)   <- row.names(table)          # add names to matrisx
idvec          <- names(table)              # create an id vector

# create a matrix of each geoids neighbor metric
a <- table     %>%     # assign this func chain to 'a'
  as.matrix    %>%     # transform to matrix to use next
  sweep(2,as.numeric(as.character(comb[,metric.control])),"*") %>% data.frame  # sweep variables

# transform back into data frame. create a matrix of, filled column wise by row geoid

b <-comb[,metric.control] %>%           # assign this func chain to 'a'  
  as.numeric() %>%                      # convert to numeric
  matrix(dim(comb)[1],dim(comb)[1]) %>% # create calculation matrix
  data.frame                            # create into dataframe 

c <- abs(b-a)*table                     # take the absolute value of differences.

e <- c %>%                              # build geoid and metric list
  rowMeans %>%                          # take row means
  round(2) %>%                          # round it to two places
  cbind(row.names(a)) %>%               # bind this with idnames
  data.frame %>%                        # coerce into dataframe
  select(c(2,1))                        # reorder

names(e) <- c(id.var,metric.control)    # rename columns
view(e)                                 # opens spreadsheet  

# plot
post$metric <- e[,2]                    # inject old metric list into .SHP
plot(post)                              # plot shapefile
text(coordinates(post), labels = post$metric, cex = .5)   # add text onto plot
title(metric.control," Neighborhood Comparison", sub = "Mean Absolute Difference", # add title / sub
      cex.main = .9,   font.main= 1, col.main= "black",
      cex.sub = 0.75, font.sub = 1, col.sub = "black")

out.name <- paste(path.folder,'/',file,'.new.dbf',sep="") # print path name
foreign::write.dbf(comb,out.name)                         # use foreign package to write dbf into dir
