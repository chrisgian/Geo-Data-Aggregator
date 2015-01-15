#### overview ####
# intro: automated neibor analysis from shapefile and data aggregator

#### set up ####

## workspace setup
dir.create('neighborhood.analysis') # create a directory 
path.folder      <-paste(getwd(),'/neighborhood.analysis',sep="") # create folder

## install packages
list.of.packages <- c('mailR', 'tcltk', 'spdep', 'dplyr', 'maptools', 'foreign', 'svDialogs')
new.packages     <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

## load packages ##
require(spdep)    # neighbors
require(dplyr)    # pipes
require(maptools) # read shapefile
require(foreign)  # read dbfs
require(svDialogs) # new dialogues

# transformation 
dlgMessage(paste('a folder has been created:',path.folder))$res
Sys.sleep(1) # slight pause


dlgMessage('select your shapefile')$res
Sys.sleep(.5)
 # read shp, added a find file option. 
post           <- readShapePoly(file.choose())

# input: id variable
id.var         <- dlgList(names(post), multiple = F,title = "Select an ID variable")$res # set id variable
Sys.sleep(.5)


# comput neighbors
post.nb        <- poly2nb(post)   # find neighbors list
table          <- nb2mat(post.nb, style = 'B') %>% data.frame  # create table of neighbors

#Input: dbf
dlgMessage('select your .dbf')$res
Sys.sleep(.5)
attributes1    <- read.dbf(file.choose()) %>% lapply(as.character) %>% data.frame(stringsAsFactors=F)  

# input csv


attributes2    <- read.csv(file.choose(),colClasses='character') 

metrics<-names(attributes2)
# input: metric

joiner<-id.var

run <- function(joiner){
  
  
  repeat{# read csv
  
    metric.control <- dlgList(metrics, multiple = F,title = "Select an metric")$res

    attributes2    <- attributes2 %>% select(c(1,which(metric.control==names(attributes2))))      # grab the selected attribute
    comb           <- left_join(attributes1, attributes2, by = joiner)                            # join two attribute tables
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
    # plot
    post$metric <- e[,2]                    # inject old metric list into .SHP
    plot(post)                              # plot shapefile
    text(coordinates(post), labels = post$metric, cex = .5)   # add text onto plot
    title(metric.control," Neighborhood Comparison", sub = "Mean Absolute Difference", # add title / sub
          cex.main = .9,   font.main= 1, col.main= "black",
          cex.sub = 0.75, font.sub = 1, col.sub = "black")
    
    
    
    
    rand              <-paste(letters[sample(25,1)],sample(9,1),letters[sample(25,1)],sample(9,1),letters[sample(25,1)],sample(9,1),sep="")
    out.name          <- paste(path.folder,'/',rand,'.new.dbf',sep="") #path
    comb[is.na(comb)] <-''  # remove na's
    foreign::write.dbf(comb,out.name) 
    write.dbf(comb,out.name,factor2char = T)                         # use foreign package to write dbf into dir
    
    rerun<-dlgList(c('yes','no'), multiple = F,title = "re-run?")$res
    Sys.sleep(3)
   if(rerun=='no'){break}
    
  }
  
}
run(joiner)
