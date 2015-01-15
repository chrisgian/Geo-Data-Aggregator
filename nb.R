#### overview ####
# intro: automated neibor analysis from shapefile and data aggregator

#### set up ####

## workspace setup
dir.create('neighborhood.analysis') # create a directory 
path.folder<-paste(getwd(),'/neighborhood.analysis',sep="") # create folder

## load packages ##
require(tcltk)   # pop ups
require(spdep)   # neighbors
require(dplyr)   # pipes
require(foreign) # read dbfs

## automation ##
# function that views data file into workbook 
  view  <- function (x,y=nrow(x)){   
    tempname <-tempfile("df", fileext = '.csv')
    write.csv(x[1:y,],tempname)
    shell.exec(tempname)
    print(paste(y,' rows. ',ncol(x),' columns.',sep=""))
    print(paste('filename: ',tempname))
    name.xl  <<-tempname
  }

# function check if packages exist, if not: download.
  checkdown  <- function(x){                 
    if(x %in% rownames(installed.packages()) == FALSE) {
      install.packages(x)}}

# pop dialogue windows
  pop  <- function(x) {
    tt       <- tktoplevel()
    tkpack( tkbutton(tt, text=x, command=function()tkdestroy(tt)),
            side='bottom')
    tkbind(tt,'<Key>', function()tkdestroy(tt) )
    tkfocus(tt)
    tkwait.window(tt)
  }

# input window for variables
inputs    <- function(){
  xvar <- tclVar("")
  yvar <- tclVar("")
  zvar <- tclVar("")
  tt <- tktoplevel()
  tkwm.title(tt,"Input Numbers")
  x.entry <- tkentry(tt, textvariable=xvar)
  y.entry <- tkentry(tt, textvariable=yvar)
  z.entry <- tkentry(tt, textvariable=zvar)
  reset <- function()
  {
    tclvalue(xvar)<-""
    tclvalue(yvar)<-""
    tclvalue(zvar)<-""
  }
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  submit <- function() {
    x <- as.character(tclvalue(xvar))
    y <- as.character(tclvalue(yvar))
    z <- as.character(tclvalue(zvar))
    e <- parent.env(environment())
    e$x <- x
    e$y <- y
    e$z <- z
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)
  tkgrid(tklabel(tt,text="Specify:"),columnspan=3)
  tkgrid(tklabel(tt,text="File"), x.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="ID Var"), y.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Metric"), z.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)
  tkwait.window(tt)
  return(c(x,y,z))
}


# dialogue windows
pop(paste('place shapefile in:',path.folder)) # reminder to save shapefile
pop(paste('place csv in:',path.folder))       # reminder to place csv

# input variable 
input.val  <- inputs()          # save dialouge inputs into input.val vector
file <- input.val[1]            # file names
id.var <- input.val[2]          # id variable
metric.control <- input.val[3]  # metric name
  
# transformation 
  post           <- readShapePoly(paste(path.folder,"/",file,sep="") ,IDvar = id.var) # read shp
  post.nb        <- poly2nb(post)   # find neighbors list
  table          <- nb2mat(post.nb, style = 'B') %>% data.frame  # create table of neighbors
  attributes1    <- read.dbf(paste(path.folder,'/',file,'.dbf',sep=""))$dbf %>% lapply(as.character) %>% data.frame(stringsAsFactors=F)   # coerce as dataframe with characters
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
