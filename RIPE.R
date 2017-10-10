# RIPE PROJECT 

#### #### #### 
#### Prep #### 
#### #### ####  
library(stringr)  
library(readr)
library(plyr)
library(data.table)
library(ggplot2)
library(scales)
sectt <- function(secs) {
  as.POSIXct(as.integer(secs), tz="UTC", origin="1970-01-01 00:00");
}
ttsec <- function(time) {
  as.integer(as.POSIXct(time, tz="UTC", origin="1970-01-01 00:00"));
}
Sys.setenv(TZ='GMT')

letter = character()
letter[10001] = 'K'
letter[10004] = 'F'
letter[10005] = 'I'
letter[10006] = 'M'
letter[10008] = 'L'
letter[10009] = 'A'
letter[10010] = 'B'
letter[10011] = 'C'
letter[10012] = 'D'
letter[10013] = 'E'
letter[10014] = 'G'
letter[10015] = 'H'
letter[10016] = 'J'
#### #### #### 


#### #### #### #### 
## Read in data ##
#### #### #### #### 
SOAS <- fread('all')
#### #### #### #### 


### #### #### #### 
### Clean data ###
### #### #### #### 
SOAS <- as.data.table(SOAS)
SOAS[,ts:=sectt(timestamp)]
SOAS <- transform(SOAS, day = as.Date((ts), "%Y%m%d"))
SOAS[,root:=letter[msm_id]]

## Clean local non-response issues ## 
SOAS <- SOAS[order(prb_id,ts)]
k = 0 
final <- data.frame() 
probe_list <- unique(SOAS$prb_id)
st = 1494806400
lt = 1497484799
unique_probes <- (1:length(probe_list))
## CLEANER [ MAKE FASTER ] ##
for(k in seq_along(unique_probes)){
  i = st
  j = st + 1800
  test <- SOAS[SOAS$prb_id == probe_list[k],]
  while(j < lt){
    if(sum(is.na(test[test$timestamp > i & test$timestamp < j, rtt])) == 13){
      test <- test[!(test$timestamp > i & test$timestamp < j),]
    }
    i = i+1800
    j = j+1800
  }
  final <- rbind(final, test)
}
#fwrite(final, "final.txt")
#final <- fread("final.txt")
Probes <- fread("R_Probes.txt", na.strings=c("","N/A",""))
Probes <- Probes[,c("id", "day", "country_code","longitude","latitude", "first_connected", 
                    "total_uptime", "is_anchor", "status_name")]
colnames(Probes)[1] <- "prb_id"
Probes <- transform(Probes, day = as.character(day))
setkey(Probes, prb_id)

#### #### #### #### 
#### Merge Data #### 
#### #### #### #### 
r_data <- join(final, Probes, by = c("prb_id","day"), type = "left")
fwrite(r_data, "r_data")

### #### ### #### ### ## 
## Countries Subsets ## 
### ### #### ### ## ###

## create data_frame of all needed values
st = as.numeric(r_data[1,"timestamp"])
lt = as.numeric(r_data[nrow(r_data),"timestamp"])
r_data <- r_data[order(r_data$country_code),] 
country_list <- as.list(unique(r_data$country_code))
countries <- data.frame(matrix(unlist(country_list), nrow=length(country_list), byrow=T))
countries[,c(2:16)] <- NA
colnames(countries) <- c("Country", "Accessiblity Percentage", "Total Mean", "Total Median", "Total Fastest RS", "Week Mean", 
                         "Week Median", "Week Fastest RS", "Day Mean", "Day Median", "Day Fastest RS",
                         "First day", "Last day", "Probe mean", "Probe Median", "Fastest Probe RS")
week_time <- function(st,lt){
  w_lt = st + ((lt-st)/4)
  return(w_lt)
}
day_time <- function(st,lt){
  d_lt = st + ((lt-st)/30)
  return(d_lt)
}
country_check <- function(r_data){
  for(i in 1:length(country_list)){
    ## subset data of one country
    country <- r_data[r_data$country_code == country_list[i],] 
    ## timeframe 
    st = as.numeric(country[1,"timestamp"])
    lt = as.numeric(country[nrow(country),"timestamp"])
    w_lt <- week_time(st,lt)
    d_lt <- day_time(st,lt)
    ## Total accessibilty for that country  
    countries[i,2] <- (100-(((table(is.na(country$rtt))['TRUE'])/(table(is.na(country$rtt))['FALSE']))*100))
    ## results for entire month
    countries[i,3] <- mean(country$rtt, na.rm=T)
    countries[i,4] <- median(country$rtt, na.rm=T)
    roots <- as.data.frame(aggregate(country$rtt, list(country$root), mean, na.rm=T))
    if(!(is.na(roots[roots[,2]==min(roots[,2]),][,1]))){
      countries[i,5] <- roots[roots[,2]==min(roots[,2]),][,1]
    }  
    ## results for first week 
    week_country <- country[country$timestamp < w_lt,]
    countries[i,6] <- mean(week_country$rtt, na.rm=T)
    countries[i,7] <- median(week_country$rtt, na.rm=T)
    roots <- aggregate.data.frame(week_country$rtt, list(week_country$root), mean, na.rm=T)
    if(!(is.na(roots[roots[,2]==min(roots[,2]),][,1]))){
      countries[i,8] <- roots[roots[,2]==min(roots[,2]),][,1]
    }
    ## results for first day
    day_country <- country[country$timestamp < d_lt,]
    countries[i,9] <- mean(day_country$rtt, na.rm=T)
    countries[i,10] <- median(day_country$rtt, na.rm=T)
    roots <- as.data.frame(aggregate(day_country$rtt, list(day_country$root), mean, na.rm=T))
    if(!(is.na(roots[roots[,2]==min(roots[,2]),][,1]))){
      countries[i,11] <- roots[roots[,2]==min(roots[,2]),][,1]
    }
    # first and last day of available data 
    countries[i,12] <- as.character.Date(country[country$day == min(country$day),][1,"day"])
    countries[i,13] <- as.character.Date(country[country$day == max(country$day),][1,"day"])
    # probe samples 
  }
}
country_check(r_data)
#write.csv(countries, "country_metrics.txt")


### Create consistency data 
letters <- as.list(sort(unique(r_data$root)))
country_list <- as.list(unique(r_data$country_code))
c_root <- data.frame(matrix(unlist(country_list), nrow=length(country_list), byrow=T))
c_root[,c(2:7)] <- NA
colnames(c_root) <- c("Country", "# of probes", "Number of consistent roots", "Precentage consistency", 
                      "1st Quartile","Median", "3rd Quartile")
t_countries <- data.frame()
consistency <- data.frame()

for(i in 1:length(country_list)){
  country <- r_data[(r_data$country_code == country_list[i]),] 
  if((length(unique(country$prb_id)))>10){
    roots <- as.data.frame(aggregate(country$rtt, list(country$root), median, na.rm=T))
    sdmax <- (median(roots[,2], na.rm=T)) + (roots[roots[,2]==min((roots[,2]), na.rm=T),][,2]) #rmin + rmedian
    thirdQ <- as.numeric(quantile(roots[,2],0.75, na.rm=T))
    consistency <- data.frame()
    for(j in 1:length(letters)){
      root_median <- median(as.numeric(unlist(country[country$root == letters[j],][,"rtt"])), na.rm=T)
      root_q <- as.numeric(quantile(as.numeric(unlist(country[country$root == letters[j],][,"rtt"])), 0.75, na.rm=T))
      if((root_median <= thirdQ) & (root_q <= sdmax)){
        consistency <- rbind(consistency, country[country$root == letters[j],])
      }
    }
    t_countries <- rbind(t_countries, consistency)
    c_root[i,2] <- length(unique(country$prb_id)) #total number of probes in country 
    c_root[i,3] <- length(unique(consistency$root)) #number of roots consistent 
    c_root[i,4] <- (c_root[i,3]/13)*100 #percentage consistent
    c_root[i,5] <- as.numeric(quantile(consistency$rtt,0.25, na.rm = T))
    c_root[i,6] <- median(consistency$rtt, na.rm = T)
    c_root[i,7] <- as.numeric(quantile(consistency$rtt,0.75, na.rm = T)) 
  }else{
    c_root[i,2] <- (length(unique(country$prb_id)))
    c_root[i,3:7] <- NA 
  }
}

## Clean c_root -> CLUSTERS 
clusters <- data.table()
for(i in 1:nrow(c_root)){
  if(!(is.na(c_root[i,3]))){
    clusters <- rbind(clusters, c_root[i,])
  }
}
setkey(clusters, Country)
#fwrite(clusters, "clusters.csv")



#### Subset by type of probe #### 
new_m_root <- function(probe_sub){
  if(!(is.na(probe_sub[1,1]))){
    roots <- as.data.frame(aggregate(probe_sub$rtt, list(probe_sub$root), median, na.rm=T))
    new_min_root <- (roots[roots[,2]==min((roots[,2]), na.rm=T),][,1])
    return(new_min_root)
  }else{
    return(NA)
  }
}
test_list <- unlist(as.character(clusters$Country))
probe_subsets <- data.frame(matrix(unlist(test_list), nrow=length(test_list), byrow=T))
probe_subsets[,c(2:7)] <- NA
colnames(probe_subsets) <- c("Country", "Fastest RS", "Not Anchors Subset", "Anchors Subset", 
                         "V1/V2 Subset", "V3 Subset", "Not V3 Subset")
for(i in 1:length(test_list)){
  country <- r_data[r_data$country_code == test_list[i],] 
  roots <- as.data.frame(aggregate(country$rtt, list(country$root), median, na.rm=T))
  probe_subsets[i,2] <- (roots[roots[,2]==min((roots[,2]), na.rm=T),][,1])
  probe_sub <- country[(country$is_anchor == "FALSE"),] 
  probe_subsets[i,3] <- new_m_root(probe_sub)
  probe_sub <- country[(country$is_anchor == "TRUE"),] 
  probe_subsets[i,4] <- new_m_root(probe_sub)
  probe_sub <- country[(country$prb_id < 5999),] 
  probe_subsets[i,5] <- new_m_root(probe_sub)
  probe_sub <- country[(country$prb_id > 10000),] 
  probe_subsets[i,6] <- new_m_root(probe_sub)
  probe_sub <- country[(country$prb_id < 10000),] 
  probe_subsets[i,7] <- new_m_root(probe_sub)
}

#fwrite(probe_subsets, "probe_subsets.csv")




## Subset by unquie probe for each country ## 
letters <- as.list(sort(unique(r_data$root)))
country_list <- as.list(unique(r_data$country_code))
servers <- data.frame(matrix(unlist(country_list), nrow=length(country_list), byrow=T))
servers[,c(2:14)] <- NA
colnames(servers) <- c("Country", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
for(i in 1:length(country_list)){
  country <- r_data[r_data$country_code == country_list[i],] 
  temp=country[,.(medrtt=median(rtt)),by=.(prb_id,msm_id)]
  setkey(temp,prb_id,msm_id)
  temp=temp[!is.na(medrtt)]
  temp[,minmedrtt:=min(medrtt),by=prb_id]
  temp=temp[medrtt==minmedrtt,]
  temp[,letter:=letter[msm_id]]
  for(j in 1:length(letters))
    if(!(is.na(table(temp$letter)[unlist(letters[j])]))){
      servers[i,j+1] <- as.numeric(table(temp$letter)[unlist(letters[j])])
    }else{
      servers[i,j+1] <- NA
    }
}

server_probes <- data.table()
for(i in 1:nrow(servers)){
  if((!(sum(servers[i,2:14], na.rm=T) < 10))){
    server_probes <- rbind(server_probes, servers[i,])
  }
}
# fwrite(server_probes, "servers.csv")


#### #### #### #### #### #### #### #### 



#### #### #### #### #### #### 
#### Graphics & Tables #### 
#### #### #### #### #### #### 

#1 - Inhomogeneous Vantage Points Example part 1
probe.classes <- fread("probe_subsets.csv")
probe.classes <- subset(probe.classes, select = -c(`Not Anchors Subset`, `V3 Subset`))
colnames(probe.classes)[c(2,5)] <- c("Entire Data", "No V3 Probes")
probe.classes.sub <- probe.classes[probe.classes$Country %in% c("AR","CA","RO","RS","SG","SI","US","ZA")]
probe.classes.sub[1,1] <- "Argentina" 
probe.classes.sub[2,1] <- "Canada" 
probe.classes.sub[3,1] <- "Romania" 
probe.classes.sub[4,1] <- "Serbia" 
probe.classes.sub[5,1] <- "Singapore" 
probe.classes.sub[6,1] <- "Slovenia" 
probe.classes.sub[7,1] <- "United States" 
probe.classes.sub[8,1] <- "South Africa" 
kable(probe.classes.sub, caption = "Fastest root server amongst different classifications")

#2 - Inhomogeneous Vantage Points Example part 2 
probe.unique <- fread("servers.csv")
probe.unique.sub <- probe.unique[probe.unique$Country %in% c("DE","US","AU")]
probe.unique.sub[1,1] <- "United States"
probe.unique.sub[2,1] <- "Germany"
probe.unique.sub[3,1] <- "Australia"
unique <- melt(probe.unique.sub,id.vars = 1)
colnames(unique)[2:3] <- c("Roots", "Number of Unique Probes")
probe.dist <- ggplot(unique, aes(x = Country,y = `Number of Unique Probes`)) + 
  geom_bar(aes(fill = Roots), stat = "identity", position = "dodge") + scale_y_sqrt()
probe.dist


#3 - Time Intervals Example 
probe.intervals <- fread("country_metrics.txt")
probe.intervals <- subset(probe.intervals, select = -c(V1, `Accessiblity Percentage`, `Total Median`, `Total Fastest RS`,
                                                       `Week Median`, `Week Fastest RS`,`Day Median`,`Day Fastest RS`))
probe.intervals.sub <- probe.intervals[probe.intervals$Country %in% c("DE","US","MX","GB","NL","RU","JP","CH","AU","ZA")]
probe.intervals.sub[1,1] <- "Australia"
probe.intervals.sub[2,1] <- "Switzerland"
probe.intervals.sub[3,1] <- "Germany"
probe.intervals.sub[4,1] <- "Great Britain"
probe.intervals.sub[5,1] <- "Japan"
probe.intervals.sub[6,1] <- "Mexico"
probe.intervals.sub[7,1] <- "Netherlands"
probe.intervals.sub[8,1] <- "Russia"
probe.intervals.sub[9,1] <- "United States"
probe.intervals.sub[10,1] <- "South Africa"
probe.intervals.sub <- as.data.frame(probe.intervals.sub)
intervals <- melt(probe.intervals.sub[,c('Country','Total Mean','Week Mean', 'Day Mean')],id.vars = 1)
colnames(intervals)[2:3] <- c("Intervals","RTT (ms)")
time.line <- ggplot(intervals,aes(x = Intervals, y = `RTT (ms)`, group=Country, colour = Country)) + 
  geom_line(lwd=1.5) + scale_y_sqrt(name = "Response Time [ms]")
time.line
kable(probe.intervals.sub, caption = "Mean Country Response time (ms) by Month, Week, and Day subsets")


#4 - Country Levels Example 
prb.count[,"c.levels"] <- cut(prb.count$Probe_Count, breaks=c(0,10,50,125,500,1000,Inf),                labels=c("1-10","10-50","50-125","125-500","500-1000","1000+")) 
## Use Factors 
prb.count$region <- as.character(prb.count$Country)
map.world <- map_data(map='world') # create map data #fig.height=16, fig.width=12
map.world <- map.world[map.world$region != "Antarctica",]
map.world$region <- countrycode(map.world$region, "country.name", "iso2c")
map.world <- fortify(map.world)
map.world <- merge(prb.count, map.world, by='region',all.y=TRUE)
map.world <- map.world[order(map.world$order), ] # <---
p.map <- ggplot() + geom_map(data = map.world,map = map.world, 
                             aes(map_id = region, x=long, y=lat, fill=c.levels)) 
p.map <- p.map + labs(fill="Probe Count", title="Number of probes per Country", x="", y="")
p.map <- p.map + scale_fill_brewer(type = "seq", direction = -1, na.value="gray30") + theme_dark()
p.map <- p.map + coord_map(xlim=c(-180,180))
p.map <- p.map + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p.map 

#5 - Mean v. Median Example 
fill <- "#4271AE"
line <- "#1F3552"
rtt.hist <- ggplot(r_data, aes(rtt)) +
  geom_histogram(binwidth = 20, col=line, fill=fill) + 
  xlim(c(0,1000)) +
  scale_y_sqrt(labels = comma) + 
  labs(x="Response Time [ms]", y="Number of Queries") + 
  ggtitle("Distribution of Return Times")
rtt.hist
invisible(dev.off())

#6 - Top-N Example 
country <- r_data[(r_data$country_code == "DE"),] 
fill <- "#4271AE"
line <- "#1F3552"
bplot <- ggplot(country, aes(x = root, y = rtt)) +
  geom_boxplot(fill = fill, colour = line ,outlier.shape = NA) + 
  scale_y_continuous(name = "Response Time [ms]", breaks = seq(0, 250, 25),
                     limits=c(0, 250)) + scale_x_discrete(name = "Root Name Server Service Address") +
  ggtitle("Measured Response Time for Roots in Germany")
bplot


