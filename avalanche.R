# DATA STEP
# Web scraper
library(XML)
library(RCurl)
avalanche.url <- "https://utahavalanchecenter.org/observations?page="
  all.pages<-0:202
  avalanche<-NULL
  for(page in all.pages){
  this.url<-paste(avalanche.url,page,sep="")
  this.webpage<-htmlParse(getURL(this.url))
  thispage.avalanche <- readHTMLTable(this.webpage,which=1,header=TRUE,
  stringsAsFactors=FALSE)
  avalanche<-rbind(avalanche,thispage.avalanche)
}

avalanche.sl<-subset(avalanche,avalanche$Region=="Salt Lake")

# create new cols to make adding up easier (loop that string splits the date
# in a row and then combines month and year to create mon-year column

# then create a loop that adds up how many observations were in each mon-year 

av.monyrs <- vector(mode="character",length=nrow(avalanche.sl))

for(i in 1:nrow(avalanche.sl)){
  av.monyrs[i] <- paste(toString(strsplit(avalanche.sl[i,1],"/")[[1]][1]),"/",toString(strsplit(avalanche.sl[i,1],"/")[[1]][3]),sep="")
}

# av.monyrs <- factor(av.monyrs,levels=unique(av.monyrs))
# library(dplyr)

avalcount <- c(rep(1,length(av.monyrs)))
aval <- NULL
aval$DATE <- av.monyrs 
aval$AVALS <- avalcount
aval <- as.data.frame(aval)

aval$DATE <- as.factor(aval$DATE)
avalanche <- aggregate(AVALS ~ DATE, data = aval, FUN = sum)

# colnames(aval) <- c("DATE","AVALS")

# reading weather data in
weather <- read.csv(text='
                    "STATION","NAME","DATE","DT32","SNOW","TMIN"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-01","20","15.0","27.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-02","16","5.1","31.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-03","17","6.2","32.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-04","1","0.0","42.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-05","0","0.5","49.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-06","0","0.0","58.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-07","0","0.0","66.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-08","0","0.0","66.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-09","0","0.0","52.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-10","0","0.0","42.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-11","28","17.0","24.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2000-12","30","12.9","22.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-01","31","6.6","21.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-02","23","13.1","26.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-03","12","5.3","35.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-04","5","9.9","40.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-05","0","0.0","50.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-06","0","0.0","57.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-07","0","0.0","66.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-08","0","0.0","65.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-09","0","0.0","56.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-10","3","0.0","42.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-11","12","18.8","33.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2001-12","29","20.0","19.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-01","30","16.1","17.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-02","27","0.7","18.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-03","17","14.4","28.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-04","4","3.6","40.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-05","1","0.0","46.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-06","0","0.0","57.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-07","0","0.0","67.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-08","0","0.0","61.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-09","0","0.0","54.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-10","4","0.0","38.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-11","25","0.2","27.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2002-12","27","3.4","27.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-01","23","0.0","28.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-02","21","9.0","26.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-03","12","5.4","34.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-04","5","4.4","40.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-05","0","0.0","49.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-06","0","0.0","58.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-07","0","0.0","69.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-08","0","0.0","67.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-09","0","0.0","52.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-10","3","1.0","44.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-11","16","10.7","30.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2003-12","28","31.2","26.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-01","30","9.9","16.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-02","28","19.8","19.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-03","11","3.9","37.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-04","1","1.0","42.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-05","0","0.0","48.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-06","0","0.0","57.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-07","0","0.0","65.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-08","0","0.0","61.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-09","0","0.0","52.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-10","1","0.1","44.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-11","19","3.3","30.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2004-12","25","3.7","25.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-01","26","6.7","28.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-02","25","11.3","26.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-03","11","2.8","33.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-04","2","0.0","39.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-05","0","0.0","47.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-06","0","0.0","54.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-07","0","0.0","66.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-08","0","0.0","63.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-09","0","0.0","52.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-10","0","0.0","43.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-11","19","1.1","32.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2005-12","28","6.9","22.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-01","27","9.3","26.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-02","22","13.5","23.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-03","16","13.3","32.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-04","4","2.2","42.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-05","0","0.0","50.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-06","0","0.0","58.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-07","0","0.0","69.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-08","0","0.0","63.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-09","0","0.0","50.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-10","6","0.0","39.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-11","20","8.1","30.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2006-12","27","9.9","22.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-01","31","10.8","11.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-02","22","13.1","27.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-03","7","2.5","35.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-04","2","0.0","41.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-05","0","0.0","49.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-06","0","0.0","58.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-07","0","0.0","70.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-08","0","0.0","67.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-09","0","0.0","53.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-10","1","0.1","40.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-11","19","0.8","30.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2007-12","29","29.8","20.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-01","31","17.1","15.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-02","27","17.4","24.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-03","18","8.3","30.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-04","12","0.0","34.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-05","2","0.0","45.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-06","0","0.0","56.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-07","0","0.0","67.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-08","0","0.0","63.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-09","0","0.0","53.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-10","5","0.5","41.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-11","16","3.0","33.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2008-12","28","20.8","21.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-01","28","10.3","23.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-02","23","6.3","27.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-03","20","6.0","31.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-04","5","1.4","38.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-05","0","0.0","49.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-06","0","0.0","54.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-07","0","0.0","65.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-08","0","0.0","61.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-09","0","0.0","57.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-10","4","0.5","39.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-11","18","2.8","30.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2009-12","30","17.1","16.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-01","27","2.3","21.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-02","22","2.3","28.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-03","19","5.7","32.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-04","7","5.6","38.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-05","2","0.2","42.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-06","0","0.0","55.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-07","0","0.0","65.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-08","0","0.0","62.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-09","0","0.0","53.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-10","2","1.8","45.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-11","15","20.9","30.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2010-12","24","11.5","27.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-01","30","5.9","20.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-02","24","11.0","25.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-03","11","9.1","34.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-04","8","8.2","36.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-05","1","0.0","42.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-06","0","0.0","53.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-07","0","0.0","66.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-08","0","0.0","65.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-09","0","0.0","56.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-10","4","0.0","43.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-11","22","5.5","29.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2011-12","30","0.1","20.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-01","28","6.9","23.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-02","19","7.4","28.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-03","11","4.0","38.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-04","3","1.0","41.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-05","0","0.0","49.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-06","0","0.0","58.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-07","0","0.0","69.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-08","0","0.0","69.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-09","0","0.0","57.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-10","1","0.3","43.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-11","14","15.2","35.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2012-12","18","13.1","28.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-01","30","23.8","12.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-02","28","14.7","21.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-03","12","2.7","34.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-04","6","0.1","39.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-05","0","0.0","50.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-06","0","0.0","60.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-07","0","0.0","71.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-08","0","0.0","70.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-09","0","0.0","59.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-10","0","0.0","41.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-11","11","3.5","34.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2013-12","28","19.2","16.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-01","29","10.1","21.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-02","10","2.2","33.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-03","2","0.5","38.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-04","3","0.0","40.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-05","0","0.0","49.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-06","0","0.0","56.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-07","0","0.0","68.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-08","0","0.0","62.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-09","0","0.0","58.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-10","0","0.0","44.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-11","15","0.8","31.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2014-12","18","5.7","29.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-01","26","0.3","26.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-02","14","0.0","34.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-03","9","2.8","37.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-04","3","5.8","41.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-05","0","0.0","50.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-06","0","0.0","64.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-07","0","0.0","65.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-08","0","0.0","65.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-09","0","0.0","58.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-10","0","0.0","50.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-11","18","1.4","30.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2015-12","26","15.9","24.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-01","30","10.6","24.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-02","21","3.8","28.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-03","5","2.7","37.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-04","0","0.0","44.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-05","0","0.0","50.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-06","0","0.0","64.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-07","0","0.0","70.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-08","0","0.0","66.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-09","0","0.0","55.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-10","0","0.0","46.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-11","7","5.7","37.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2016-12","31","11.1","22.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-01","28","23.2","21.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-02","17","6.7","31.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-03","6","3.6","39.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-04","0","1.0","40.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-05","0","0.1","51.1"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-06","0","0.0","62.8"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-07","0","0.0","72.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-08","0","0.0","69.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-09","0","0.0","56.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-10","1","0.0","40.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-11","9","0.0","37.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2017-12","29","7.7","25.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-01","20","9.6","30.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-02","18","14.3","30.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-03","8","10.5","37.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-04","2","1.4","43.3"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-05","0","0.0","53.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-06","0","0.0","61.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-07","0","0.0","70.7"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-08","0","0.0","65.2"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-09","0","0.0","57.0"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-10","2","0.0","43.5"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-11","19","1.7","30.6"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2018-12","29","13.2","25.9"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2019-01","30","6.6","24.4"
                    "USW00024127","SALT LAKE CITY INTERNATIONAL AIRPORT, UT US","2019-02","20","13.9","27.8"
                    ',header=TRUE)

weather <- weather[,-c(1,2)]
weather$DATE <- as.character(weather$DATE)
weather$dateFmt <- ""

# creating comparable date format to merge avalanches and weather
for(i in 1:nrow(weather)){
	weather[i,5] <- paste(as.character(as.numeric(strsplit(weather[i,1],"-")[[1]][2])),"/",as.character(strsplit(weather[i,1],"-")[[1]][1]),sep="")
}

weather$DATE <- weather$dateFmt
weather <- weather[,-5]

# merge the two datasets
slc <- merge(avalanche,weather,by="DATE")

# sort the dataset
slc$mon <- c(rep(NA, nrow(slc)))
slc$yr <- c(rep(NA, nrow(slc)))

slc$DATE <- as.character(slc$DATE)

for(i in 1:nrow(slc)){
	slc[i,6] <- as.numeric(strsplit(slc[i,1],"/")[[1]][1])
	slc[i,7] <- as.numeric(strsplit(slc[i,1],"/")[[1]][2])
}

slc <- slc[order(slc$yr,slc$mon),]
rownames(slc) <- NULL

# now drop months that are not Dec - Mar
slc <- subset(slc, mon < 4 | mon == 12)

# ensure data was imported correctly
str(slc)

# EDA STEP

plot(AVALS~SNOW,data=slc,main="AVALS~SNOW")
plot(AVALS~DT32,data=slc,main="AVALS~DT32")

# Analysis
# Response variable: Number of avalanches in a month
# Explanatory variables:
#     SNOW (inches)
#     DT32 (days in month with temp below 32)
#     TMIN (degrees of month's lowest temp)

# Model:
# Avalanches ~ Poisson( exp(beta0 + beta1*SNOW + beta2*DT32 + beta3*TMIN) )

avalanche.out <- glm(AVALS ~ SNOW + DT32 + TMIN, data = slc, family = "poisson")
summary(avalanche.out)
 
# For a one inch of additional snow we estimate an increase of 0.0099 in log mean avalanches, holding all else constant

# To better understand the effects
exp(coef(avalanche.out)[-1])

# For each additional inch of snow we estimate the mean avalanches increases by 1%, holding all else constant

# 95% CI
exp(confint(avalanche.out)[-1,])

# Does snowfall have an effect on avalanches

# H0: beat1 = 0
# Ha: bet1 =/= 0
# Test statistic = 3.648
# P-value = 0.0003
# We will reject H0 in favor of Ha of Beta1 at the alpha = 0.05 level

# Snowfall has a statistically significant effect on the number of avalanches (p-value is 0.0003)
# The amount of the snowfall we receive plays an influential role on the number of avalanches that occur.

# Does temperature have an effect on avalanches?
avalanche.red <- glm(AVALS ~ SNOW, data = slc, family = "poisson")
anova(avalanche.red, avalanche.out, test = "Chisq")
# H0: beta2 = beta3 = 0
#Ha: at least one has an effect
# Test statistic: chi^2 = 56.572
# p-value < 0.0001
# We reject H0 in favor of Ha at the alpha 0.05
# Temperature 
# Blue bird days may not be all they are cracked up to be in backcountry skiing, as increased temperature weakens
# the snow base and increases the likelihood of avalanches

# Estimate the number of avalanches in the typical December
summary(subset(slc, mon == 12))
predict(avalanche.out, newdata = data.frame(SNOW = 12.38, DT32 =28, TMIN = 22.9), type = "response")

# On average, the typical December has 74 avalanches
# 95% CI for December
logmu.hat1 <- predict(avalanche.out, newdata = data.frame(SNOW = 12.38, DT32 =28, TMIN = 22.9), type = "link", se.fit = TRUE)
logmu.L1 <-logmu.hat1$fit - 1.96*logmu.hat1$se.fit
logmu.U1 <-logmu.hat1$fit + 1.96*logmu.hat1$se.fit

exp(logmu.L1)
exp(logmu.U1)

# 95% CI for January
summary(subset(slc, mon == 1))
logmu.hat2 <- predict(avalanche.out, newdata = data.frame(SNOW = 9.636, DT32 = 27.64, TMIN = 23.15), type = "link", se.fit = TRUE)
logmu.L2 <-logmu.hat2$fit - 1.96*logmu.hat2$se.fit
logmu.U2 <-logmu.hat2$fit + 1.96*logmu.hat2$se.fit

exp(logmu.L2)
exp(logmu.U2)

# 95% CI for February
summary(subset(slc, mon == 2))
logmu.hat3 <- predict(avalanche.out, newdata = data.frame(SNOW = 9.092, DT32 = 20.31, TMIN = 27.99), type = "link", se.fit = TRUE)
logmu.L3 <-logmu.hat3$fit - 1.96*logmu.hat3$se.fit
logmu.U3 <-logmu.hat3$fit + 1.96*logmu.hat3$se.fit

exp(logmu.L3)
exp(logmu.U3)

# 95% CI for March
summary(subset(slc, mon == 3))
logmu.hat4 <- predict(avalanche.out, newdata = data.frame(SNOW = 5.6, DT32 = 10, TMIN = 35.86), type = "link", se.fit = TRUE)
logmu.L4 <-logmu.hat4$fit - 1.96*logmu.hat4$se.fit
logmu.U4 <-logmu.hat4$fit + 1.96*logmu.hat4$se.fit

exp(logmu.L4)
exp(logmu.U4)