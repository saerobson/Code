library(ggplot2)
library(dplyr)
library(lubridate)

initial_mob_2020=read.csv("2020_GB_Region_Mobility_Report.csv")
initial_mob_2020_1=initial_mob_2020
initial_mob_2021=read.csv("2021_GB_Region_Mobility_Report.csv")
initial_mob_2021_1=initial_mob_2021

#The last 6 are all '% change from baseline'
names(initial_mob_2020_1)=c("CountryRegionCode","CountryRegion","SubRegion1","SubRegion2","MetroArea","iso_3166_2_code","CensusFipsCode","PlaceID","Date","RetailRec",
                        "GroceryPharmacy","Parks","TransitStation","Workplaces","Residential")
names(initial_mob_2021_1)=c("CountryRegionCode","CountryRegion","SubRegion1","SubRegion2","MetroArea","iso_3166_2_code","CensusFipsCode","PlaceID","Date","RetailRec",
                            "GroceryPharmacy","Parks","TransitStation","Workplaces","Residential")
mob_2020_1=initial_mob_2020_1
mob_2020_1$Date=as.Date(mob_2020_1$Date)
mob_2021_1=initial_mob_2021_1
mob_2021_1$Date=as.Date(mob_2021_1$Date)

length(unique(mob_2020_1$PlaceID)) #419 unique places

mob_comb=rbind(mob_2020_1,mob_2021_1)

#### Separate into those with data for each of the 6 reasons to leave house ####

# 1. Retail and Recreation

retail_rec=mob_comb[is.na(mob_comb$RetailRec)==FALSE,]

# 2. Grocery and Pharmacy

grocery_pharm=mob_comb[is.na(mob_comb$GroceryPharmacy)==FALSE,]

# 3. Parks

parks=mob_comb[is.na(mob_comb$Parks)==FALSE,]

# 4. Transit Stations

transit=mob_comb[is.na(mob_comb$TransitStation)==FALSE,]

# 5. Workplaces

workplace=mob_comb[is.na(mob_comb$Workplaces)==FALSE,]

# 6. Residential

residential=mob_comb[is.na(mob_comb$Residential)==FALSE,]

# Take mean over all counties
retail_rec_mean=aggregate(retail_rec[, 10], list(retail_rec$Date), mean)
names(retail_rec_mean)=c("Date","RetailRec")

grocery_pharm_mean=aggregate(grocery_pharm[, 11], list(grocery_pharm$Date), mean)
names(grocery_pharm_mean)=c("Date","GroceryPharmacy")

parks_mean=aggregate(parks[, 12], list(parks$Date), mean)
names(parks_mean)=c("Date","Parks")

transit_mean=aggregate(transit[, 13], list(transit$Date), mean)
names(transit_mean)=c("Date","TransitStation")

workplace_mean=aggregate(workplace[, 14], list(workplace$Date), mean)
names(workplace_mean)=c("Date","Workplaces")

residential_mean=aggregate(residential[, 15], list(residential$Date), mean)
names(residential_mean)=c("Date","Residential")


# Example Codes
plot(GroceryPharmacy~Date,grocery_pharm[grocery_pharm$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ",],ylim=c(-100,100))
plot(GroceryPharmacy~Date,grocery_pharm_mean,ylim=c(-100,100))
grocery_pharm[grocery_pharm$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ" & grocery_pharm$Date=="2020-06-20",]

plot(Workplaces~Date,workplace[workplace$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ",],ylim=c(-100,100))
workplace[workplace$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ" & workplace$Date=="2020-06-20",]

plot(Residential~Date,residential[residential$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ",],ylim=c(-100,100))
plot(Residential~Date,residential_mean,ylim=c(-100,100))
residential[residential$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ" & residential$Date=="2020-06-20",]

# Example plots
Mobplot_R=ggplot(residential_mean)
Mobplot_R=Mobplot_R + geom_point(aes(x=Date,y=Residential),color="magenta",size=1)
Mobplot_R=Mobplot_R+labs(title="Residential")
Mobplot_R=Mobplot_R+ylab("% Change from Baseline")
Mobplot_R

Mobplot_WP=ggplot(workplace_mean)
Mobplot_WP=Mobplot_WP + geom_point(aes(x=Date,y=Workplaces),color="magenta",size=1)
Mobplot_WP=Mobplot_WP+labs(title="Workplace")
Mobplot_WP=Mobplot_WP+ylab("% Change from Baseline")
Mobplot_WP

Mobplot_T=ggplot(transit_mean)
Mobplot_T=Mobplot_T + geom_point(aes(x=Date,y=TransitStation),color="magenta",size=1)
Mobplot_T=Mobplot_T+labs(title="Transit Stations")
Mobplot_T=Mobplot_T+ylab("% Change from Baseline")
Mobplot_T

Mobplot_P=ggplot(parks_mean)
Mobplot_P=Mobplot_P + geom_point(aes(x=Date,y=Parks),color="magenta",size=1)
Mobplot_P=Mobplot_P+labs(title="Parks")
Mobplot_P=Mobplot_P+ylab("% Change from Baseline")
Mobplot_P

Mobplot_GP=ggplot(grocery_pharm_mean)
Mobplot_GP=Mobplot_GP + geom_point(aes(x=Date,y=GroceryPharmacy),color="magenta",size=1)
Mobplot_GP=Mobplot_GP+labs(title="Grocery and Pharmacy")
Mobplot_GP=Mobplot_GP+ylab("% Change from Baseline")
Mobplot_GP

Mobplot_RR=ggplot(retail_rec_mean)
Mobplot_RR=Mobplot_RR + geom_point(aes(x=Date,y=RetailRec),color="magenta",size=1)
Mobplot_RR=Mobplot_RR+labs(title="Retail and Recreation")
Mobplot_RR=Mobplot_RR+ylab("% Change from Baseline")
Mobplot_RR

### The combo and average over all ###########
mob_comb_needed=mob_comb[,c(9:15)]
mob_comb_rem=na.omit(mob_comb_needed)
mob_comb_rem$average=(mob_comb_rem$RetailRec+mob_comb_rem$GroceryPharmacy +mob_comb_rem$Parks +
                        mob_comb_rem$TransitStation +mob_comb_rem$Workplaces +mob_comb_rem$Residential)/6
mobility_average=mob_comb_rem[,c(1,8)]

mobility_average=aggregate(mob_comb_rem$average, list(mob_comb_rem$Date), mean)
names(mobility_average)=c("Date","Average")

Mobplot_Average=ggplot(mobility_average)
Mobplot_Average=Mobplot_Average + geom_point(aes(x=Date,y=Average),color="magenta",size=1)
Mobplot_Average=Mobplot_Average+labs(title="Average % Change in Mobility Across all Factors")
Mobplot_Average=Mobplot_Average+ylab("% Change from Baseline")
Mobplot_Average

### Just Transit and Workplace
transit_work=mob_comb[,c(9,13:14)]
transit_work_rem=na.omit(transit_work)
transit_work_rem$average=(transit_work_rem$TransitStation+transit_work_rem$Workplaces)/2


transit_work_average=aggregate(transit_work_rem$average, list(transit_work_rem$Date), mean)
names(transit_work_average)=c("Date","Average")

transit_work_average$Day <- weekdays(as.Date(transit_work_average$Date))
transit_work_average$Weekend=ifelse(transit_work_average$Day=="Saturday" | (transit_work_average$Day=="Sunday"),"Weekend", ifelse("Weekday"))
transit_work_average$Weekend[is.na(transit_work_average$Weekend)==TRUE]="Weekday"

transit_work_average_weekday=transit_work_average[transit_work_average$Day!="Saturday" & transit_work_average$Day!="Sunday",]

TransitWorkplace_Average=ggplot(transit_work_average_weekday)
TransitWorkplace_Average=TransitWorkplace_Average + geom_point(aes(x=Date,y=Average),color="magenta",size=1)
TransitWorkplace_Average=TransitWorkplace_Average+labs(title="Average % Change in Mobility Across Transit and Workplace (Weekdays)")
TransitWorkplace_Average=TransitWorkplace_Average+ylab("% Change from Baseline")
TransitWorkplace_Average


#TransitWorkplace_Average=ggplot(transit_work_average)
#TransitWorkplace_Average=TransitWorkplace_Average + geom_point(aes(x=Date,y=Average,fill=(Weekend)),size=1)
#TransitWorkplace_Average=TransitWorkplace_Average+labs(title="Average % Change in Mobility Across Transit and Workplace (Weekdays)")
#TransitWorkplace_Average=TransitWorkplace_Average+ylab("% Change from Baseline")
#TransitWorkplace_Average

### Take lines 1-175
initial_mob_2021_176=rbind(mob_2021_1[1:175,c(9,13,14)],mob_2020_1[1:321,c(9,13,14)])
initial_mob_2021_176$Average=(initial_mob_2021_176$TransitStation+initial_mob_2021_176$Workplaces)/2
initial_mob_2021_176_1=initial_mob_2021_176[,c(1,4)]
names(initial_mob_2021_176_1)=c("Date","Average")
initial_mob_2021_176_1$Day <- weekdays(as.Date(initial_mob_2021_176_1$Date))
initial_mob_2021_176_1_weekday=initial_mob_2021_176_1[initial_mob_2021_176_1$Day!="Saturday" & initial_mob_2021_176_1$Day!="Sunday",]

TransitWorkplace_Average=ggplot(initial_mob_2021_176_1_weekday)
TransitWorkplace_Average=TransitWorkplace_Average + geom_point(aes(x=Date,y=Average),color="magenta",size=1)
TransitWorkplace_Average=TransitWorkplace_Average+labs(title="Average % Change in Mobility Across Transit and Workplace (Weekdays)")
TransitWorkplace_Average=TransitWorkplace_Average+ylab("% Change from Baseline")
TransitWorkplace_Average

######## Take 5 day average #########
initial_mob_2021_176_1_weekday$fiveday=0

for ( i in 1:dim(initial_mob_2021_176_1_weekday)[1]) {
  if (i==1){initial_mob_2021_176_1_weekday$fiveday[i]=sum(initial_mob_2021_176_1_weekday$Average[1:3])/3}
  else if (i==2) {initial_mob_2021_176_1_weekday$fiveday[i]=sum(initial_mob_2021_176_1_weekday$Average[1:4])/4}
  else if (i==(dim(initial_mob_2021_176_1_weekday)[1])-1) {initial_mob_2021_176_1_weekday$fiveday[i]=sum(initial_mob_2021_176_1_weekday$Average[(dim(initial_mob_2021_176_1_weekday)[1]-3):(dim(initial_mob_2021_176_1_weekday)[1])])/4}
  else if (i==(dim(initial_mob_2021_176_1_weekday)[1])) {initial_mob_2021_176_1_weekday$fiveday[i]=sum(initial_mob_2021_176_1_weekday$Average[(dim(initial_mob_2021_176_1_weekday)[1]-2):(dim(initial_mob_2021_176_1_weekday)[1])])/3}
  else {initial_mob_2021_176_1_weekday$fiveday[i]=sum(initial_mob_2021_176_1_weekday$Average[(i-2):(i+2)])/5}
}
initial_mob_2021_176_1_weekday55=initial_mob_2021_176_1_weekday[month(initial_mob_2021_176_1_weekday$Date)==12 | month(initial_mob_2021_176_1_weekday$Date)==1,]

MobilityFiveDayAverage=ggplot(initial_mob_2021_176_1_weekday)
MobilityFiveDayAverage=MobilityFiveDayAverage + geom_point(aes(x=Date,y=Average),color="magenta",size=1)
MobilityFiveDayAverage=MobilityFiveDayAverage+labs(title="5 Day Average % Change in Mobility Across Transit and Workplace (Weekdays)")
MobilityFiveDayAverage=MobilityFiveDayAverage+ylab("% Change from Baseline")
MobilityFiveDayAverage=MobilityFiveDayAverage+geom_line(aes(x=Date,y=fiveday))
MobilityFiveDayAverage

initial_mob_2021_176_1_weekday[month(initial_mob_2021_176_1_weekday$Date)==1,]

## Interpolate ####
head(initial_mob_2021_176_1)
initial_mob_2021_176_1$replace=0
#Find weekends and bank holidays
for (i in 1:(dim(initial_mob_2021_176_1)[1])) {
  if (initial_mob_2021_176_1$Day[i] == "Saturday" || initial_mob_2021_176_1$Day[i]=="Sunday") {initial_mob_2021_176_1$replace[i]=1}
  else if (initial_mob_2021_176_1$Date[i]=="2020-04-10" ||initial_mob_2021_176_1$Date[i]=="2020-05-04" || initial_mob_2021_176_1$Date[i]=="2020-05-08" ||
           initial_mob_2021_176_1$Date[i]=="2020-05-25" || initial_mob_2021_176_1$Date[i]=="2020-08-31" ||
           initial_mob_2021_176_1$Date[i]=="2020-12-25" || initial_mob_2021_176_1$Date[i]=="2020-12-28" ) {initial_mob_2021_176_1$replace[i]=1}
else if (initial_mob_2021_176_1$Date[i]=="2021-01-01" || initial_mob_2021_176_1$Date[i]=="2021-04-02" || initial_mob_2021_176_1$Date[i]=="2021-04-05" ||
         initial_mob_2021_176_1$Date[i]=="2021-05-03" || initial_mob_2021_176_1$Date[i]=="2021-05-31" ) {initial_mob_2021_176_1$replace[i]=1}
  }

initial_mob_2021_176_1=initial_mob_2021_176_1[order(as.Date(initial_mob_2021_176_1$Date,format="%Y/%m%d")),]
head(initial_mob_2021_176_1)
initial_mob_2021_176_1$interpolate=initial_mob_2021_176_1$Average

#Interpolate here, find how many days are 
for (i in 1:(dim(initial_mob_2021_176_1)[1])) {
  if (initial_mob_2021_176_1$replace[i]==1) {initial_mob_2021_176_1$interpolate[i]=0}
}

initial_mob_2021_176_1$dayinterpolate=0 #finding how many days next to each other need interpolating, houslnd't be more than 4 at a time

for (i in 1:(dim(initial_mob_2021_176_1)[1])) {
  if (i==1 || i==2) {initial_mob_2021_176_1$dayinterpolate[i]=0} #first two days are weekend, make the same as the first non weekend day
  else if (i>2 && i<(dim(initial_mob_2021_176_1)[1]) && initial_mob_2021_176_1$replace[i]==1 && initial_mob_2021_176_1$replace[i-1]==0 && initial_mob_2021_176_1$replace[i+1]==0){
    initial_mob_2021_176_1$dayinterpolate[i] =1}
  else if (i>2 && i<(dim(initial_mob_2021_176_1)[1]) && initial_mob_2021_176_1$replace[i]==1 && initial_mob_2021_176_1$replace[i-1]==1 && initial_mob_2021_176_1$replace[i-2]==0 && initial_mob_2021_176_1$replace[i+1]==0)
  {initial_mob_2021_176_1$dayinterpolate[i] =2.2}
  else if (i>2 && i<(dim(initial_mob_2021_176_1)[1]-1) && initial_mob_2021_176_1$replace[i]==1 && initial_mob_2021_176_1$replace[i+1]==1 && initial_mob_2021_176_1$replace[i+2]==0 && initial_mob_2021_176_1$replace[i-1]==0)
  {initial_mob_2021_176_1$dayinterpolate[i] =2.1}
  else if (i>2 && i<(dim(initial_mob_2021_176_1)[1]) && initial_mob_2021_176_1$replace[i]==1 && initial_mob_2021_176_1$replace[i-1]==1 && initial_mob_2021_176_1$replace[i-2]==1  && initial_mob_2021_176_1$replace[i-3]==0 && initial_mob_2021_176_1$replace[i+1]==0)
  {initial_mob_2021_176_1$dayinterpolate[i] =3.3}
  else if (i>2 && i<(dim(initial_mob_2021_176_1)[1]-2) &&  initial_mob_2021_176_1$replace[i]==1 && initial_mob_2021_176_1$replace[i+1]==1 && initial_mob_2021_176_1$replace[i+2]==1  && initial_mob_2021_176_1$replace[i+3]==0 && initial_mob_2021_176_1$replace[i-1]==0)
  {initial_mob_2021_176_1$dayinterpolate[i] =3.1}
  else if (i>2 && i<(dim(initial_mob_2021_176_1)[1]-1) && initial_mob_2021_176_1$replace[i]==1 && initial_mob_2021_176_1$replace[i-1]==1 && initial_mob_2021_176_1$replace[i-2]==0  && initial_mob_2021_176_1$replace[i+1]==1 && initial_mob_2021_176_1$replace[i+2]==0)
  {initial_mob_2021_176_1$dayinterpolate[i] =3.2}
  else if (initial_mob_2021_176_1$Date[i]=="2020-12-25" || initial_mob_2021_176_1$Date[i]=="2021-04-02"){initial_mob_2021_176_1$dayinterpolate[i] =4.1}
  else if (initial_mob_2021_176_1$Date[i]=="2020-12-26" || initial_mob_2021_176_1$Date[i]=="2021-04-03"){initial_mob_2021_176_1$dayinterpolate[i] =4.2}
  else if (initial_mob_2021_176_1$Date[i]=="2020-12-27" || initial_mob_2021_176_1$Date[i]=="2021-04-04"){initial_mob_2021_176_1$dayinterpolate[i] =4.3}
  else if (initial_mob_2021_176_1$Date[i]=="2020-12-28" || initial_mob_2021_176_1$Date[i]=="2021-04-05"){initial_mob_2021_176_1$dayinterpolate[i] =4.4}
}
initial_mob_2021_176_1$interpolate=initial_mob_2021_176_1$Average

for (i in 1:(dim(initial_mob_2021_176_1)[1])) {
  if (i==1 || i==2) {initial_mob_2021_176_1$interpolate[i]=-8} #first two days are weekend, make the same as the first non weekend day
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 1) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-1] + 0.5*(initial_mob_2021_176_1$interpolate[i+1]-initial_mob_2021_176_1$interpolate[i-1])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 2.1) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-1] + (1/3)*(initial_mob_2021_176_1$interpolate[i+2]-initial_mob_2021_176_1$interpolate[i-1])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 2.2) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-2] + (2/3)*(initial_mob_2021_176_1$interpolate[i+1]-initial_mob_2021_176_1$interpolate[i-2])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 3.1) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-1] + (1/4)*(initial_mob_2021_176_1$interpolate[i+3]-initial_mob_2021_176_1$interpolate[i-1])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 3.2) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-2] + (2/4)*(initial_mob_2021_176_1$interpolate[i+2]-initial_mob_2021_176_1$interpolate[i-2])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 3.3) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-3] + (3/4)*(initial_mob_2021_176_1$interpolate[i+1]-initial_mob_2021_176_1$interpolate[i-3])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 4.1) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-1] + (1/5)*(initial_mob_2021_176_1$interpolate[i+4]-initial_mob_2021_176_1$interpolate[i-1])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 4.2) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-2] + (2/5)*(initial_mob_2021_176_1$interpolate[i+3]-initial_mob_2021_176_1$interpolate[i-2])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 4.3) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-3] + (3/5)*(initial_mob_2021_176_1$interpolate[i+2]-initial_mob_2021_176_1$interpolate[i-3])}
  else if (initial_mob_2021_176_1$dayinterpolate[i] == 4.4) {initial_mob_2021_176_1$interpolate[i] = initial_mob_2021_176_1$interpolate[i-4] + (4/5)*(initial_mob_2021_176_1$interpolate[i+1]-initial_mob_2021_176_1$interpolate[i-4])}
  else {initial_mob_2021_176_1$interpolate[i]=initial_mob_2021_176_1$Average[i]} 
  }


###### Now Plot the Interpolated
Interpolated=ggplot(initial_mob_2021_176_1)
Interpolated=Interpolated + geom_point(aes(x=Date,y=interpolate),color="magenta",size=1) #interpolated mobility
#Interpolated=Interpolated + geom_point(aes(x=Date,y=Average),color="blue",size=1) #OG mobility
Interpolated=Interpolated+labs(title="% Change in Mobility Across Transit and Workplace (Interpolated Weekends & Bank Holidays)")
Interpolated=Interpolated+ylab("% Change from Baseline")
#Interpolated=Interpolated+geom_line(aes(x=Date,y=fiveday))
Interpolated

alpha_mobility=initial_mob_2021_176_1$interpolate