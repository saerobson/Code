library(lubridate)
library(ggplot2)

#Daily provisional non-cumulative figures on death occurrence where coronavirus 
#(COVID-19) was mentioned on the death certificate in the UK

death_occurence_2020=read.csv("2020 UK Daily Deaths.csv")
death_occurence_2020[,1]=as.Date(death_occurence_2020[,1],format = "%d/%m/%Y")
death_occurence_2021=read.csv("2021 UK Daily Deaths.csv")
death_occurence_2021[,1]=as.Date(death_occurence_2021[,1],format = "%d/%m/%Y")
head(death_occurence_2020)
head(death_occurence_2021)
death_occurence_2021$death_av=0
death_occurence_2021[1:10,]
death_occurence_2021_rem=na.omit(death_occurence_2021)

England=death_occurence_2021_rem[,c(1,4,17)]
Wales=death_occurence_2021_rem[,c(1,6,17)]
Scotland=death_occurence_2021_rem[,c(1,5,17)]

names(England)=c("Date","Deaths","Average")
names(Wales)=c("Date","Deaths","Average")
names(Scotland)=c("Date","Deaths","Average")

England$Average=England$Deaths
Wales$Average=Wales$Deaths
Scotland$Average=Scotland$Deaths

for (i in 4:dim(England)[1]) {
  if (i==4){England$Average[i]=sum(England$Deaths[4:7])/7
  Wales$Average[i]=sum(Wales$Deaths[4:7])/7
  Scotland$Average[i]=sum(Scotland$Deaths[4:7])/7}
  else if (i==5){England$Average[i]=sum(England$Deaths[4:8])/7
  Wales$Average[i]=sum(Wales$Deaths[4:8])/7
  Scotland$Average[i]=sum(Scotland$Deaths[4:8])/7}
  else if (i==6){England$Average[i]=sum(England$Deaths[4:9])/7
  Wales$Average[i]=sum(Wales$Deaths[4:9])/7
  Scotland$Average[i]=sum(Scotland$Deaths[4:9])/7}
  else if (i==(dim(England)[1]-2)){England$Average[i]=sum(England$Deaths[(dim(England)[1]-5):dim(England)[1]])/7
  Wales$Average[i]=sum(Wales$Deaths[(dim(England)[1]-5):dim(England)[1]])/7
  Scotland$Average[i]=sum(Scotland$Deaths[(dim(England)[1]-5):dim(England)[1]])/7}
  else if (i==(dim(England)[1]-1)){England$Average[i]=sum(England$Deaths[(dim(England)[1]-4):dim(England)[1]])/7
  Wales$Average[i]=sum(Wales$Deaths[(dim(England)[1]-4):dim(England)[1]])/7
  Scotland$Average[i]=sum(Scotland$Deaths[(dim(England)[1]-4):dim(England)[1]])/7}
  else if (i==dim(England)[1]){England$Average[i]=sum(England$Deaths[(dim(England)[1]-3):dim(England)[1]])/7
  Wales$Average[i]=sum(Wales$Deaths[(dim(England)[1]-3):dim(England)[1]])/7
  Scotland$Average[i]=sum(Scotland$Deaths[(dim(England)[1]-3):dim(England)[1]])/7}
  else {England$Average[i]=sum(England$Deaths[(i-3):(i+3)])/7
  Wales$Average[i]=sum(Wales$Deaths[(i-3):(i+3)])/7
  Scotland$Average[i]=sum(Scotland$Deaths[(i-3):(i+3)])/7}
}

plot(Deaths~Date ,England)
plot(Deaths~Date ,Wales)
plot(Deaths~Date ,Scotland)

EnglandDeaths=ggplot(England)
EnglandDeaths=EnglandDeaths + geom_point(aes(x=Date,y=Deaths),color="magenta",size=1)
EnglandDeaths=EnglandDeaths+labs(title="Number of Deaths Across England")
EnglandDeaths=EnglandDeaths+ylab("Number of Deaths")
EnglandDeaths=EnglandDeaths+geom_line(aes(x=Date,y=Average))
EnglandDeaths=EnglandDeaths+ylim(0,1500)
EnglandDeaths

WalesDeaths=ggplot(Wales)
WalesDeaths=WalesDeaths + geom_point(aes(x=Date,y=Deaths),color="magenta",size=1)
WalesDeaths=WalesDeaths+labs(title="Number of Deaths Across Wales")
WalesDeaths=WalesDeaths+ylab("Number of Deaths")
WalesDeaths=WalesDeaths+geom_line(aes(x=Date,y=Average))
WalesDeaths=WalesDeaths+ylim(0,100)
WalesDeaths

ScotlandDeaths=ggplot(Scotland)
ScotlandDeaths=ScotlandDeaths + geom_point(aes(x=Date,y=Deaths),color="magenta",size=1)
ScotlandDeaths=ScotlandDeaths+labs(title="Number of Deaths Across Scotland")
ScotlandDeaths=ScotlandDeaths+ylab("Number of Deaths")
ScotlandDeaths=ScotlandDeaths+geom_line(aes(x=Date,y=Average))
ScotlandDeaths=ScotlandDeaths+ylim(0,120)
ScotlandDeaths

