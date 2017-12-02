date.range<-seq.Date(from=as.Date(min(ufo.us$DateOccurred)),to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings<-strftime(date.range, "%Y-%m")
head(date.strings)

states.dates<-lapply(us.states, function(s) cbind(s,date.strings))
states.dates<-data.frame(do.call(rbind, states.dates), stringsAsFactors=FALSE)
head(states.dates)

all.sightings<-merge(states.dates, sightings.counts, by.x=c("s","date.strings"),by.y=c("USState","YearMonth"), all=TRUE)
head(all.sightings)

names(all.sightings) <- c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
all.sightings$YearMonth<-as.Date(rep(date.range, length(us.states)))
all.sightings$State<-as.factor(toupper(all.sightings$State))
head(all.sightings)
