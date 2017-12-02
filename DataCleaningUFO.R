#Read in the ufo_awesome.tsv file into Rstudio and name it “ufo”.
ufo = read.delim(file.choose(), sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")

#Look at the first 6 rows:  
  head(ufo) 

#Note that there are not any headings…
names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")

#Some of the fields are goofed up…  Look at the first field of rows 1 and 756:
  
ufo$DateOccurred[1]
ufo$DateOccurred[756] 
nchar(ufo$DateOccurred[1])
nchar(ufo$DateOccurred[756])

#Having 8 characters doesn’t mean it is a date, but having 20 means it is not…

#How can we eliminate the rows which have dates not of length 8?

which(nchar(ufo$DateOccurred)!=8)  # which are the troubled rows?
length(which(nchar(ufo$DateOccurred)!=8))  # how many are there?

#Some date fields are bad.  Let’s find some of them & print the first 6:
  
head(
    ufo[which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8),1]
  )

#How many are bad?
good.rows <- ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8, FALSE, TRUE)
length(which(!good.rows))
length(good.rows)

#Keep only the good rows… Should have 61182 left
ufo<-ufo[good.rows,]

#Finally we can get the dates set as Date objects!
  
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported, format="%Y%m%d")

s1 = "no comma here"
s2 = "comma, here"
strsplit(s1,",")[[1]]
strsplit(s2,",")[[1]]

#Define a function that will find some errors for us…  Split the string with a “,” .
get.location <- function(s) {
  split.location<-tryCatch(strsplit(s,",")[[1]],error=function(e) return(c(NA, NA)))
  clean.location <- gsub("^ ","",split.location)
  if (length(clean.location)>2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

#Apply the function “get.location” to the column of “ufo$Location”, save it in “city.state”

city.state<-lapply(ufo$Location, get.location)
head(city.state)

#Here we convert our city.state from a list to a matrix:
location.matrix<-do.call(rbind, city.state)
#Next we transform ufo with these new columns:
ufo<-transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]), stringsAsFactors=FALSE)
#ufo should now have 61182 rows and 8 columns

us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il","in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh","nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt","wa","wi","wv","wy")

head(ufo$USState)					#show the first 6 states
match(ufo$USState,us.states)  			#produces a lot of output
head(match(ufo$USState, us.states)) 		# just the first 6 is enough

#Note the “NA” from the second statement.
#First, 
ufo$USState<-us.states[match(ufo$USState,us.states)]
ufo$USCity[is.na(ufo$USState)]<-NA
head(ufo$USCity)

#We just want the subset that is clean…
ufo.us<-subset(ufo, !is.na(USState))  		# we lose about 10K rows!
head(ufo.us)

summary(ufo.us$DateOccurred)

ufo.us <- subset(ufo.us, !is.na(ufo.us$DateOccurred))

#Take a subset of recent citings from 1990
ufo.us<-subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))
nrow(ufo.us)

#Should leave us with 46347 rows.

#Create a new column for YearMonth:
  
  ufo.us$YearMonth<-strftime(ufo.us$DateOccurred, format="%Y-%m")
head(ufo.us$YearMonth)

#Install & load the plyr library
sightings.counts<-ddply(ufo.us,.(USState,YearMonth),nrow)
head(sightings.counts)

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

fl = subset(all.sightings,State=="FL")
plot(fl$Sightings~fl$YearMonth, col="blue")

tx = subset(all.sightings,State=="TX")
points(tx$YearMonth, tx$Sightings, col="red")

ca = subset(all.sightings,State=="CA")
points(ca$YearMonth, ca$Sightings, col="green")

library(sqldf)
allsightings <- all.sightings
names(allsightings)
resultSet = sqldf('select YearMonth, sum(sightings) as sightings from allsightings group by YearMonth')
plot(resultSet$sightings ~ resultSet$YearMonth)





