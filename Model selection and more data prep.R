setwd("C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2")

#Importing the woodcock data
Data1859 <- read.csv("C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/stopover_locations1859Take2.csv")
View(Data1859)

Data3900 <- read.csv("C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/stopover_locations3900Take2.csv")
View(Data3900)

#subsetting the full migration data to include only the id, age, and sex from the fall
data.1859 <- data.frame(Data1859)
data.3900 <- data.frame(Data3900)

data.formerge.1859 <- data.1859 [ , c("uniqueID", "mean_imp_cover_1859")]
View(data.formerge.1859)

#Using the id field to merge the two dataframes and viewing them to check accuracy
WoodcockData <- merge(data.formerge.1859, data.3900, by.x="uniqueID", by.y="uniqueID")
View(WoodcockData)


##########Visualizing the data in different ways (not necessary code)################################################
hist(Woodcock.spring$mean_imp_cover_1859, breaks=100)
hist(Woodcock.spring$mean_imp_cover_3900, breaks=100)

hist(Woodcock.fall$mean_imp_cover_1859, breaks=100)
hist(Woodcock.fall$mean_imp_cover_3900, breaks=100)

plot(x=Woodcock.spring$mean_imp_cover_1859, y=Woodcock.spring$duration_h)
plot(x=Woodcock.spring$mean_imp_cover_3900, y=Woodcock.spring$duration_h)

plot(x=Woodcock.fall$mean_imp_cover_1859, y=Woodcock.fall$duration_h)
plot(x=Woodcock.fall$mean_imp_cover_3900, y=Woodcock.fall$duration_h)

############################################################################################
#########################Preparing the data for analysis####################################

#Subsetting the data into stopovers from the spring migration
Woodcock.spring <- subset(WoodcockData, season=="spring")
View(Woodcock.spring)
#and stopovers from the Fall migration
Woodcock.fall <- subset(WoodcockData, season=="fall")
View(Woodcock.fall)

#Subsetting the data to include only the longer stopovers, not the shorter stops
Woodcock.spring.stopover <- subset(Woodcock.spring, stop_type =="stopover")
View(Woodcock.spring.stopover)
#and for the fall
Woodcock.fall.stopover <- subset(Woodcock.fall, stop_type == "stopover")
View(Woodcock.fall.stopover)

#Aggregating the BCRs
Woodcock.fall.stopover$BCR2 <- "NA"
View(Woodcock.fall.stopover)

for (i in 1:nrow(Woodcock.fall.stopover)){
  
  Woodcock.fall.stopover$BCR2[i]<- ifelse(test=Woodcock.fall.stopover$BCR[i]=="12", yes="13", no=
                                            ifelse(test=Woodcock.fall.stopover$BCR[i]=="22"|Woodcock.fall.stopover$BCR[i]=="23", yes="24", no=
                                                     ifelse(test=Woodcock.fall.stopover$BCR[i]=="25" |
                                                              Woodcock.fall.stopover$BCR[i]=="26" |
                                                              Woodcock.fall.stopover$BCR[i]=="37", yes= "27",  
                                                            no = Woodcock.fall.stopover$BCR[i])))
  
}
View(Woodcock.fall.stopover)

#And for the spring
Woodcock.spring.stopover$BCR2 <- "NA"
View(Woodcock.spring.stopover)

for (i in 1:nrow(Woodcock.spring.stopover)){
  
  Woodcock.spring.stopover$BCR2[i]<- ifelse(test=Woodcock.spring.stopover$BCR[i]=="12", yes="13", no=
                                              ifelse(test=Woodcock.spring.stopover$BCR[i]=="22"|Woodcock.spring.stopover$BCR[i]=="23", yes="24", no=
                                                       ifelse(test=Woodcock.spring.stopover$BCR[i]=="25" |
                                                                Woodcock.spring.stopover$BCR[i]=="26" |
                                                                Woodcock.spring.stopover$BCR[i]=="37", yes= "27",  
                                                              no = Woodcock.spring.stopover$BCR[i])))
  
}
View(Woodcock.spring.stopover)


#################################################################################################################################################
############## running random effects models on % impervious cover vs stopover duration ################################### 
#Starting with a model that incorporates id as a random effect

#making sure that the Matrix and lmer packages are up to date 
#so that I don't get an error running them
oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)
update.packages("lme4")
update.packages("Matrix")
library(lme4)
library(Matrix)

#running a random effects regression on the spring data for percent impervious cover vs. duration in hours for each radius
#correcting for each bird (distinguished by id) having multiple stops
Woodcock.spring.stopover$id <- as.factor(Woodcock.spring.stopover$id)
duration.spring.1859.lmer <- lmer(data=Woodcock.spring.stopover, duration_h~mean_imp_cover_1859+(1|id), REML=FALSE)
summary(duration.spring.1859.lmer)
duration.spring.3900.lmer <- lmer(data=Woodcock.spring.stopover, duration_h~mean_imp_cover_3900+(1|id), REML=FALSE)
summary(duration.spring.3900.lmer)

#And running the random effects regressions for the Fall
Woodcock.fall.stopover$id <- as.factor(Woodcock.fall.stopover$id)
duration.fall.1859.lmer <-lmer(data=Woodcock.fall.stopover, duration_h~mean_imp_cover_1859+(1|id), REML=FALSE)
summary(duration.fall.1859.glm)
duration.fall.3900.glm <- lmer(data=Woodcock.fall.stopover, duration_h~mean_imp_cover_3900+(1|id), REML=FALSE)
summary(duration.fall.3900.lmer)

#running regressions to evaluate the effect of impervious surfaces on stop vs stopover
Woodcock.fall$stop_type <- as.factor(Woodcock.fall$stop_type)
stop_type.fall.1859.glm <- glm

#Because the random effects are shown by the results of these models not to have a significant effect, I'll run a regular glm 
#weighting the data by 1/the number of stopovers for each individual
## add weight term as 1/c where c is number of observations (stopovers) per bird.

#########################################################################################################################################
#########Running a weighted linear regression on % impervious cover vs. stopover duration##########

#First, adding a column with the weight for each stopover
##creating an empty column for n in the subset fall stopover data
data.fall.stopover<- data.frame(Woodcock.fall.stopover)
data.fall.stopover$n<- "NA"
View(data.fall.stopover)

##filling in the n column with the number of stopovers for that bird
for (i in 1:nrow(data.fall.stopover)){
  data.fall.stopover$n[i]<- length(which(data.fall.stopover$id==data.fall.stopover$id[i]))
}
View(data.fall.stopover)

## creating and filling in a weight term to = 1/the number of stopovers for that bird

data.fall.stopover$w<- 1/as.numeric(data.fall.stopover$n)
View(data.fall.stopover)

##And doing the same thing for the spring stopover data
data.spring.stopover<- data.frame(Woodcock.spring.stopover)
data.spring.stopover$n<- "NA"
View(data.spring.stopover)

for (i in 1:nrow(data.spring.stopover)){
  data.spring.stopover$n[i]<- length(which(data.spring.stopover$id==data.spring.stopover$id[i]))
}
View(data.spring.stopover)

data.spring.stopover$w<- 1/as.numeric(data.spring.stopover$n)
View(data.spring.stopover)

#Running the regressions with the calculated weights...
#For the 1859 m radius in the Fall
Fall.duration.1859.glm <- glm(duration_h ~ mean_imp_cover_1859, family=gaussian, 
                              data=data.fall.stopover, weights=w)
summary(Fall.duration.1859.glm)
#What is a reasonable target p value?
#For the 3900 m radius in the Fall
Fall.duration.3900.glm <- glm(duration_h ~ mean_imp_cover_3900, family=gaussian, 
                              data=data.fall.stopover, weights=w)
summary(Fall.duration.3900.glm)
#For the 1859 m radius in the Spring
Spring.duration.1859.glm <- glm(duration_h ~ mean_imp_cover_1859, family=gaussian, 
                                data=data.spring.stopover, weights=w)
summary(Spring.duration.1859.glm)
#And for the 3900 m radius in the Spring
Spring.duration.3900.glm <- glm(duration_h ~ mean_imp_cover_3900, family=gaussian, 
                                data=data.spring.stopover, weights=w)
summary(Spring.duration.3900.glm)


#########################################################################################################
##########Running models that incorporate centroid lat and long#######################
as.numeric()
Fall.duration.1859.latlong.glm <- glm(duration_h ~ log(mean_imp_cover_1859)+as.numeric(centroid_l)+as.numeric(centroid_1), 
                                      family=gaussian, data=data.fall.stopover, weights=w)
summary(Fall.duration.1859.latlong.glm)
#That doesn't look right 
str(data.fall.stopover)
head(data.fall.stopover)
hist(log(data.fall.stopover$mean_imp_cover_1859), breaks=20)
df.cor<-rbind(c(data.fall.stopover[,13:14], data.fall.stopover[,21:23]))
cor(df.cor)
head(df.cor)
######################################################################################################################
############################Running models that incorporate BCR############################################

Fall.duration.1859.BCR.glm <- glm(duration_h~log(mean_imp_cover_1859)+BCR2, family=gaussian, 
                                  data=data.fall.stopover, weights=w)
summary(Fall.duration.1859.BCR.glm)

##################################################################################################################
###############Adding in age and sex data from the full migration datasheet#############################
FullMigration <- read.csv("C:/Users/zoepa/OneDrive/Desktop/Honors thesis/migration_data_all_seasons_for_ZP_101323.csv")
View(FullMigration)

#subsetting the full migration data to include only the id, age, and sex from the fall
data.fullmigration <- data.frame(FullMigration)
data.fall.fullmigration <- subset(data.fullmigration, season=="fall")
View(data.fall.fullmigration)
data.agesex.fullmigration.fall <- data.fall.fullmigration [ , c("id", "age", "sex")]
View(data.agesex.fullmigration.fall)

#Using the id field to merge the two dataframes and viewing them to check accuracy
data.fall.stopover.agesex <- merge(data.fall.stopover, data.agesex.fullmigration.fall, by.x="id", by.y="id")
View(data.fall.stopover.agesex)
View(data.fall.stopover)

#And doing the same for the spring
data.spring.fullmigration <-subset(data.fullmigration, season=="spring")
View(data.spring.fullmigration)
data.agesex.fullmigration.spring <- data.spring.fullmigration [, c("id", "age", "sex")]
View(data.agesex.fullmigration.spring)

data.spring.stopover.agesex <- merge(data.spring.stopover, data.agesex.fullmigration.spring,
                                     by.x="id", by.y="id")
View(data.spring.stopover.agesex)
View(data.spring.stopover)
####################################################################################################
################Adding in a variable for date relative to October 8th###############
#install.packages("anytime")
library(anytime)
#install.packages("lubridate")
library(lubridate)
#yday() returns the ordinal day 

#First, converting the start_time column into the correct date format
data.fall.stopover.agesex$date<- anydate(data.fall.stopover.agesex$start_time)
View(data.fall.stopover.agesex)
#Converting to ordinal date
data.fall.stopover.agesex$ord<- yday(data.fall.stopover.agesex$date)
View(data.fall.stopover.agesex)
## and study date relative to October 8th (first day of migration year)
data.fall.stopover.agesex$date.ord<- ifelse(data.fall.stopover.agesex$ord>279, 
                                            data.fall.stopover.agesex$ord-280, 
                                            data.fall.stopover.agesex$ord+85)
View(data.fall.stopover.agesex)

#And the same for the spring
#formatting
data.spring.stopover.agesex$date<- anydate(data.spring.stopover.agesex$start_time)
View(data.spring.stopover.agesex)
#ordinal date
data.spring.stopover.agesex$ord<- yday(data.spring.stopover.agesex$date)
View(data.spring.stopover.agesex)
#date relative to October 8th
data.spring.stopover.agesex$date.ord<- ifelse(data.spring.stopover.agesex$ord>279, 
                                              data.spring.stopover.agesex$ord-280, 
                                              data.spring.stopover.agesex$ord+85)
View(data.spring.stopover.agesex)
################################################################################################################
#######Testing the correlations between the potential confounding variables in the fall###############

#Using chi-square tests of independence to test for correlations among the
#categorical variables in the Fall
##Age and sex
fall.chisq.agesex <- chisq.test(data.fall.stopover.agesex$age, 
                                data.fall.stopover.agesex$sex)
fall.chisq.agesex
##Age and BCR
fall.chisq.ageBCR2<- chisq.test(data.fall.stopover.agesex$age, 
                                as.factor(data.fall.stopover.agesex$BCR2))
fall.chisq.ageBCR2
##Sex and BCR
fall.chisq.sexBCR2<- chisq.test(data.fall.stopover.agesex$sex, 
                                as.factor(data.fall.stopover.agesex$BCR2))
fall.chisq.sexBCR2
##Age and year
fall.chisq.ageyear<- chisq.test(data.fall.stopover.agesex$age, 
                                as.factor(data.fall.stopover.agesex$year))
fall.chisq.ageyear
##Sex and year
fall.chisq.sexyear<- chisq.test(data.fall.stopover.agesex$sex, 
                                as.factor(data.fall.stopover.agesex$year))
fall.chisq.sexyear
##BCR and year
fall.chisq.BCRyear<- chisq.test(as.factor(data.fall.stopover.agesex$BCR2), 
                                as.factor(data.fall.stopover.agesex$year))
fall.chisq.BCRyear
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
fall.fisher.BCRyear<- fisher.test(as.factor(data.fall.stopover.agesex$BCR2), 
                                  as.factor(data.fall.stopover.agesex$year))
####Removing objects from the workspace to try and fix an error
rm(Woodcock.fall.stopover)
rm(fall.chisq.ageBCR)
rm(fall.chisq.ageBCR2)
rm(fall.chisq.agesex)
rm(fall.chisq.ageyear)
rm(fall.chisq.BCRyear)
rm(fall.chisq.sexBCR2)
rm(fall.chisq.sexyear)
rm(data.agesex.fullmigration)
rm(Woodcock.spring)
rm(Woodcock.fall)

####Since that didn't fix the error, I tried using a simulated p-value
fall.fisher.BCRyear<- fisher.test(as.factor(data.fall.stopover.agesex$BCR2), 
                                  as.factor(data.fall.stopover.agesex$year),
                                  simulate.p.value=TRUE)
fall.fisher.BCRyear

##And now testing the continuous date, impervious, lat, and lon variables with the binary age and sex variables
#install.packages("ltm")
library(ltm)
###age and lat
fall.biserial.agelat<-biserial.cor(data.fall.stopover.agesex$centroid_lat, 
                                   data.fall.stopover.agesex$age)
fall.biserial.agelat
###age and lon
fall.biserial.agelon<- biserial.cor(data.fall.stopover.agesex$centroid_lon, 
                                    data.fall.stopover.agesex$age)
fall.biserial.agelon 
###sex and lat
fall.biserial.sexlat<-biserial.cor(data.fall.stopover.agesex$centroid_lat, 
                                   data.fall.stopover.agesex$sex)
fall.biserial.sexlat
###sex and lon
fall.biserial.sexlon<-biserial.cor(data.fall.stopover.agesex$centroid_lon, 
                                   data.fall.stopover.agesex$sex)
fall.biserial.sexlon
###Age and date
fall.biserial.agedate <- biserial.cor(data.fall.stopover.agesex$date.ord, 
                                      data.fall.stopover.agesex$age)
fall.biserial.agedate
###Age and imp 1859
fall.biserial.age1859 <-biserial.cor(data.fall.stopover.agesex$mean_imp_cover_1859, 
                                     data.fall.stopover.agesex$age)
fall.biserial.age1859
###Age and impervious 3900
fall.biserial.age3900 <-biserial.cor(data.fall.stopover.agesex$mean_imp_cover_3900, 
                                     data.fall.stopover.agesex$age)
fall.biserial.age3900
###Sex and date
fall.biserial.sexdate <-biserial.cor(data.fall.stopover.agesex$date.ord, 
                                     data.fall.stopover.agesex$sex)
fall.biserial.sexdate
###Sex and imp 1859m
fall.biserial.sex1859 <-biserial.cor(data.fall.stopover.agesex$mean_imp_cover_1859, 
                                     data.fall.stopover.agesex$sex)
fall.biserial.sex1859
###Sex and imp 3900
fall.biserial.sex3900 <-biserial.cor(data.fall.stopover.agesex$mean_imp_cover_3900, 
                                     data.fall.stopover.agesex$sex)
fall.biserial.sex3900

#And using a Pearson's correlation coefficient to test the correlations between each combination of continuous variables
###Latitude and date
fall.pearson.latdate <- cor(data.fall.stopover.agesex$centroid_lat, 
                            data.fall.stopover.agesex$date.ord)
fall.pearson.latdate
###Latitude and imp 1859
fall.pearson.lat1859 <- cor(data.fall.stopover.agesex$centroid_lat, 
                            data.fall.stopover.agesex$mean_imp_cover_1859)
fall.pearson.lat1859
###latitude and imp 3900
fall.pearson.lat3900 <- cor(data.fall.stopover.agesex$centroid_lat, 
                            data.fall.stopover.agesex$mean_imp_cover_3900)
fall.pearson.lat3900
###longitude and date
fall.pearson.londate <- cor(data.fall.stopover.agesex$centroid_lon, 
                            data.fall.stopover.agesex$date.ord)
fall.pearson.londate
###longitude and imp 1859
fall.pearson.lon1859 <- cor(data.fall.stopover.agesex$centroid_lon, 
                            data.fall.stopover.agesex$mean_imp_cover_1859)
fall.pearson.lon1859
###longitude and imp 3900
fall.pearson.lon3900 <- cor(data.fall.stopover.agesex$centroid_lon, 
                            data.fall.stopover.agesex$mean_imp_cover_3900)
fall.pearson.lon3900
###Date and imp 1859
fall.pearson.date1859 <- cor(data.fall.stopover.agesex$mean_imp_cover_1859, 
                             data.fall.stopover.agesex$date.ord)
fall.pearson.date1859
###Date and imp 3900
fall.pearson.date3900 <- cor(data.fall.stopover.agesex$mean_imp_cover_3900, 
                             data.fall.stopover.agesex$date.ord)
fall.pearson.date3900

fall.pearson.latlon <- cor(data.fall.stopover.agesex$centroid_lat, 
                           data.fall.stopover.agesex$centroid_lon)
fall.pearson.latlon
#Using linear regression to test the correlation between the multi-category variables 
#and the continuous ones
fall.yearlat.lm <- lm(centroid_lat~as.factor(year),
                      data=data.fall.stopover.agesex, weights=w)
summary(fall.yearlat.lm) 
fall.yearlat.glm


fall.yearlon.lm <- lm(centroid_lon~as.factor(year), 
                      data=data.fall.stopover.agesex, weights=w)
summary(fall.yearlon.lm) 

fall.yeardate.lm <- lm(date.ord~as.factor(year), 
                       data=data.fall.stopover.agesex, weights=w)
summary(fall.yeardate.lm) 

fall.year1859.lm <- lm(mean_imp_cover_1859~as.factor(year), 
                       data=data.fall.stopover.agesex, weights=w)
summary(fall.year1859.lm) 

fall.year3900.lm <- lm(mean_imp_cover_3900~as.factor(year),  
                       data=data.fall.stopover.agesex, weights=w)
summary(fall.year3900.lm) 


fall.BCRdate.lm <- lm(date.ord~as.factor(BCR2), 
                      data=data.fall.stopover.agesex, weights=w)
summary(fall.BCRdate.lm) 

fall.BCR1859.lm <- lm(mean_imp_cover_1859~as.factor(BCR2),  
                      data=data.fall.stopover.agesex, weights=w)
summary(fall.BCR1859.lm) 

fall.BCR3900.lm <- lm(mean_imp_cover_3900~as.factor(BCR2),  
                      data=data.fall.stopover.agesex, weights=w)
summary(fall.BCR3900.lm) 
#############################################################################################
############# Testing the correlations in the spring stopover dataset##########################
#Using chi-square tests of independence to test for correlations among the
#categorical variables 
#first, removing some points with no data for age and sex
data.spring.stopover.agesex.2<- subset(data.spring.stopover.agesex, age=="Adult"|age=="Young"&
                                         sex=="m"|sex=="f")
View(data.spring.stopover.agesex.2)
rm(data.spring.stopover)
rm(data.spring.stopover.agesex)
##Age and sex
spring.chisq.agesex <- chisq.test(as.factor(data.spring.stopover.agesex.2$age), 
                                  as.factor(data.spring.stopover.agesex.2$sex))
spring.chisq.agesex

##Age and BCR
spring.chisq.ageBCR2<- chisq.test(as.factor(data.spring.stopover.agesex.2$age), 
                                  as.factor(data.spring.stopover.agesex.2$BCR2))
spring.chisq.ageBCR2
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
spring.fisher.BCRage<- fisher.test(as.factor(data.spring.stopover.agesex.2$BCR2), 
                                   as.factor(data.spring.stopover.agesex.2$age))
spring.fisher.BCRage

##Sex and BCR
spring.chisq.sexBCR2<- chisq.test(data.spring.stopover.agesex.2$sex, 
                                  as.factor(data.spring.stopover.agesex.2$BCR2))
spring.chisq.sexBCR2
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
spring.fisher.BCRsex<- fisher.test(as.factor(data.spring.stopover.agesex.2$sex), 
                                   as.factor(data.spring.stopover.agesex.2$BCR2))
spring.fisher.BCRsex
####Since there was an error, I tried using a simulated p-value
spring.fisher.BCRsex<- fisher.test(as.factor(data.spring.stopover.agesex.2$sex), 
                                   as.factor(data.spring.stopover.agesex.2$BCR2),
                                   simulate.p.value=TRUE)
spring.fisher.BCRsex
table(as.factor(data.spring.stopover.agesex.2$BCR2), 
      as.factor(data.spring.stopover.agesex.2$sex))

##Age and year
spring.chisq.ageyear<- chisq.test(data.spring.stopover.agesex.2$age, 
                                  as.factor(data.spring.stopover.agesex.2$year))
spring.chisq.ageyear
format(spring.chisq.ageyear, scientific=F)
##Sex and year
spring.chisq.sexyear<- chisq.test(data.spring.stopover.agesex.2$sex, 
                                  as.factor(data.spring.stopover.agesex.2$year))
spring.chisq.sexyear
##BCR and year
spring.chisq.BCRyear<- chisq.test(as.factor(data.spring.stopover.agesex.2$BCR2), 
                                  as.factor(data.spring.stopover.agesex.2$year))
spring.chisq.BCRyear
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
spring.fisher.BCRyear<- fisher.test(as.factor(data.spring.stopover.agesex.2$BCR2), 
                                    as.factor(data.spring.stopover.agesex.2$year))

####Since there was an error, I tried using a simulated p-value
spring.fisher.BCRyear<- fisher.test(as.factor(data.spring.stopover.agesex.2$BCR2), 
                                    as.factor(data.spring.stopover.agesex.2$year),
                                    simulate.p.value=TRUE)
spring.fisher.BCRyear

##And now testing the continuous date, impervious, lat, and lon variables with the 
###binary age and sex variables using a biserial correlation
#install.packages("ltm")
library(ltm)
###age and lat
spring.biserial.agelat<-biserial.cor(data.spring.stopover.agesex.2$centroid_lat, 
                                     data.spring.stopover.agesex.2$age)
spring.biserial.agelat
###age and lon
spring.biserial.agelon<- biserial.cor(data.spring.stopover.agesex.2$centroid_lon, 
                                      data.spring.stopover.agesex.2$age)
spring.biserial.agelon 
###sex and lat
spring.biserial.sexlat<-biserial.cor(data.spring.stopover.agesex.2$centroid_lat, 
                                     data.spring.stopover.agesex.2$sex)
spring.biserial.sexlat
###sex and lon
spring.biserial.sexlon<-biserial.cor(data.spring.stopover.agesex.2$centroid_lon, 
                                     data.spring.stopover.agesex.2$sex)
spring.biserial.sexlon
###Age and date
spring.biserial.agedate <- biserial.cor(as.numeric(data.spring.stopover.agesex.2$date.ord), 
                                        as.factor(data.spring.stopover.agesex.2$age))
spring.biserial.agedate
###Age and imp 1859
spring.biserial.age1859 <-biserial.cor(data.spring.stopover.agesex.2$mean_imp_cover_1859, 
                                       data.spring.stopover.agesex.2$age)
spring.biserial.age1859
###Age and impervious 3900
spring.biserial.age3900 <-biserial.cor(data.spring.stopover.agesex.2$mean_imp_cover_3900, 
                                       data.spring.stopover.agesex.2$age)
spring.biserial.age3900
###Sex and date
spring.biserial.sexdate <-biserial.cor(data.spring.stopover.agesex.2$date.ord, 
                                       data.spring.stopover.agesex.2$sex)
spring.biserial.sexdate
###Sex and imp 1859m
spring.biserial.sex1859 <-biserial.cor(data.spring.stopover.agesex.2$mean_imp_cover_1859, 
                                       data.spring.stopover.agesex.2$sex)
spring.biserial.sex1859
###Sex and imp 3900
spring.biserial.sex3900 <-biserial.cor(data.spring.stopover.agesex.2$mean_imp_cover_3900, 
                                       data.spring.stopover.agesex.2$sex)
spring.biserial.sex3900

#And using a Pearson's correlation coefficient to test the correlations between each 
#combination of continuous variables
###Latitude and date
spring.pearson.latdate <- cor(data.spring.stopover.agesex.2$centroid_lat, 
                              data.spring.stopover.agesex.2$date.ord)
spring.pearson.latdate
###Latitude and imp 1859
spring.pearson.lat1859 <- cor(data.spring.stopover.agesex.2$centroid_lat, 
                              data.spring.stopover.agesex.2$mean_imp_cover_1859)
spring.pearson.lat1859
###latitude and imp 3900
spring.pearson.lat3900 <- cor(data.spring.stopover.agesex.2$centroid_lat, 
                              data.spring.stopover.agesex.2$mean_imp_cover_3900)
spring.pearson.lat3900
###lat and lon
spring.pearson.latlon <- cor(data.spring.stopover.agesex.2$centroid_lat, 
                             data.spring.stopover.agesex.2$centroid_lon)
spring.pearson.latlon
###longitude and date
spring.pearson.londate <- cor(data.spring.stopover.agesex.2$centroid_lon, 
                              data.spring.stopover.agesex.2$date.ord)
spring.pearson.londate
###longitude and imp 1859
spring.pearson.lon1859 <- cor(data.spring.stopover.agesex.2$centroid_lon, 
                              data.spring.stopover.agesex.2$mean_imp_cover_1859)
spring.pearson.lon1859
###longitude and imp 3900
spring.pearson.lon3900 <- cor(data.spring.stopover.agesex.2$centroid_lon, 
                              data.spring.stopover.agesex.2$mean_imp_cover_3900)
spring.pearson.lon3900
###Date and imp 1859
spring.pearson.date1859 <- cor(data.spring.stopover.agesex.2$mean_imp_cover_1859, 
                               data.spring.stopover.agesex.2$date.ord)
spring.pearson.date1859
###Date and imp 3900
spring.pearson.date3900 <- cor(data.spring.stopover.agesex.2$mean_imp_cover_3900, 
                               data.spring.stopover.agesex.2$date.ord)
spring.pearson.date3900

#Using linear regression to test the correlation between the multi-category variables 
#and the continuous ones
spring.yearlat.lm <- lm(centroid_lat~as.factor(year),  
                        data=data.spring.stopover.agesex.2, weights=w)
summary(spring.yearlat.lm) 

spring.yearlon.lm <- lm(centroid_lon~as.factor(year),  
                        data=data.spring.stopover.agesex.2, weights=w)
summary(spring.yearlon.lm) 

spring.yeardate.lm <- lm(date.ord~as.factor(year), 
                         data=data.spring.stopover.agesex.2, weights=w)
summary(spring.yeardate.lm) 

spring.year1859.lm <- lm(mean_imp_cover_1859~as.factor(year), 
                         data=data.spring.stopover.agesex.2, weights=w)
summary(spring.year1859.lm) 

spring.year3900.lm <- lm(mean_imp_cover_3900~as.factor(year),  
                         data=data.spring.stopover.agesex.2, weights=w)
summary(spring.year3900.lm) 


spring.BCRdate.lm <- lm(date.ord~as.factor(BCR2), 
                        data=data.spring.stopover.agesex.2, weights=w)
summary(spring.BCRdate.lm) 

spring.BCR1859.lm <- lm(mean_imp_cover_1859~as.factor(BCR2),  
                        data=data.spring.stopover.agesex.2, weights=w)
summary(spring.BCR1859.lm) 

spring.BCR3900.lm <- lm(mean_imp_cover_3900~as.factor(BCR2),  
                        data=data.spring.stopover.agesex.2, weights=w)
summary(spring.BCR3900.lm) 

##############################################################################################
##############Model selection for the dependent variable duration_h in the fall###################
#install.packages("AICcmodavg")
library(AICcmodavg)

#Running models for the effects of each combination of individual variables (age and sex)
#on stopover duration
fall.duration.age.glm <-glm(duration_h ~ as.factor(age), family=gaussian, 
                            data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.age.glm)

fall.duration.sex.glm<-glm(duration_h ~ as.factor(sex), family=gaussian, 
                           data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.sex.glm)

fall.duration.ageplussex.glm <-glm(duration_h ~ as.factor(age)+as.factor(sex), family=gaussian, 
                                   data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.ageplussex.glm)

fall.duration.agexsex.glm <- glm(duration_h ~ as.factor(age)*as.factor(sex), family=gaussian, 
                                 data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.agexsex.glm)

fall.duration.null.glm <- glm(duration_h ~ 1, family=gaussian, 
                              data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.null.glm)

#AICc model selection on the above models
cand.set.fall.duration.1 <- list(fall.duration.age.glm, fall.duration.sex.glm, fall.duration.ageplussex.glm, 
                                 fall.duration.agexsex.glm, fall.duration.null.glm)
modnames.fall.duration.1 <- c("Age", "Sex", 
                              "Age+Sex", "Age*Sex", 
                              "Null")
aictab(cand.set=cand.set.fall.duration.1, modnames = modnames.fall.duration.1,  
       digits = 2, second.ord = T)
##Because the null model was the most supported(deltaAICc=0), I won't be including 
##age or sex as additive effects in the rest of my stopover duration analysis 

#Running models on the effects of temporal variables (year and date) on duration
#using the most supported individual effects model
#in this case, the null model
fall.duration.year.glm <-glm(duration_h ~ as.factor(year), family=gaussian, 
                             data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.year.glm)

fall.duration.date.glm <-glm(duration_h ~ date.ord, family=gaussian, 
                             data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.date.ord.glm)

fall.duration.yearplusdate.glm <-glm(duration_h ~ date.ord+as.factor(year), family=gaussian, 
                                     data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.yearplusdate.glm)

#Model selection on the above models
cand.set.fall.duration.2 <- list(fall.duration.year.glm, fall.duration.date.glm, 
                                 fall.duration.yearplusdate.glm,
                                 fall.duration.null.glm)
modnames.fall.duration.2 <- c("fall.duration.year.glm", "fall.duration.date.glm", 
                              "fall.duration.yearplusdate.glm",
                              "fall.duration.null.glm")
aictab(cand.set=cand.set.fall.duration.2, modnames = modnames.fall.duration.2,  
       digits = 2, second.ord = T)
##Because yearplusdate was the most supported model, I'll use that in the next step

#Running models on the spatial variables (lat, lon, and BCR) using the most supported
#temporal and individual model, 

fall.duration.lat.glm <-glm(duration_h ~ date.ord+as.factor(year)+centroid_lat, family=gaussian, 
                            data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.lat.glm)


fall.duration.lon.glm <-glm(duration_h ~ date.ord+as.factor(year)+centroid_lon, family=gaussian, 
                            data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.lon.glm)


fall.duration.latpluslon.glm <-glm(duration_h ~ date.ord+as.factor(year)+centroid_lat+centroid_lon,
                                   family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.latpluslon.glm)

#Not including year since previous analysis showed they're correlated
fall.duration.BCR.glm <-glm(duration_h ~ date.ord+as.factor(BCR2), family=gaussian, 
                            data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.BCR.glm)


#Model selection on the above models
cand.set.fall.duration.3 <- list(fall.duration.lat.glm, fall.duration.lon.glm, 
                                 fall.duration.latpluslon.glm,
                                 fall.duration.BCR.glm, fall.duration.yearplusdate.glm)
modnames.fall.duration.3 <- c("fall.duration.lat.glm", "fall.duration.lon.glm", 
                              "fall.duration.latpluslon.glm",
                              "fall.duration.BCR.glm", "fall.duration.yearplusdate.glm")
aictab(cand.set=cand.set.fall.duration.3, modnames = modnames.fall.duration.3,  
       digits = 2, second.ord = T)
##Because the lat plus lon model was the most supported, I'll use that one in the next step

#Testing the most supported model with both mean impervious radii

fall.duration.imp1859.glm <- glm(duration_h ~ date.ord+as.factor(year)+centroid_lat+centroid_lon+mean_imp_cover_1859, 
                                 family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859.glm)


fall.duration.imp3900.glm <- glm(duration_h ~ date.ord+as.factor(year)+centroid_lat+centroid_lon+mean_imp_cover_3900, 
                                 family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900.glm)


fall.duration.imp1859only.glm <- glm(duration_h ~ mean_imp_cover_1859, 
                                     family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859only.glm)


fall.duration.imp3900only.glm <- glm(duration_h ~ mean_imp_cover_3900, 
                                     family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900only.glm)


#Model selection on the above models
cand.set.fall.duration.4 <- list(fall.duration.imp1859.glm, fall.duration.imp3900.glm, 
                                 fall.duration.latpluslon.glm, fall.duration.imp1859only.glm, 
                                 fall.duration.imp3900only.glm)
modnames.fall.duration.4 <- c("fall.duration.imp1859.glm", "fall.duration.imp3900.glm", 
                              "fall.duration.latpluslon.glm", "fall.duration.imp1859only.glm", 
                              "fall.duration.imp3900only.glm")
aictab(cand.set=cand.set.fall.duration.4, modnames = modnames.fall.duration.4,  
       digits = 2, second.ord = T)
##All three models are supported

#Model selection with impervious surfaces and interactions

fall.duration.imp1859xage.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_1859*as.factor(age),
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xage.glm)

fall.duration.imp1859xsex.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_1859*as.factor(sex),
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xsex.glm)

fall.duration.imp1859xyear.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_1859*as.factor(year),
                                     family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xyear.glm)

fall.duration.imp1859xdate.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_1859*date.ord,
                                     family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.impx1859date.glm)sum

fall.duration.imp1859xlat.glm<- glm(duration_h ~ date.ord + as.factor(year) + centroid_lon+centroid_lat+mean_imp_cover_1859*centroid_lat,
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xlat.glm)

fall.duration.imp1859xlon.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_1859*centroid_lon,
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xlon.glm)
#not including year, latitude, or longitude
fall.duration.imp1859xBCR.glm<- glm(duration_h ~ date.ord+mean_imp_cover_1859*as.factor(BCR2),
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xBCR.glm)
#and for the larger radius as well

fall.duration.imp3900xage.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_3900*as.factor(age),
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xage.glm)


fall.duration.imp3900xsex.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_3900*as.factor(sex),
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xsex.glm)


fall.duration.imp3900xyear.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_3900*as.factor(year),
                                     family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xyear.glm)


fall.duration.imp3900xdate.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_3900*date.ord,
                                     family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xdate.glm)


fall.duration.imp3900xlat.glm<- glm(duration_h ~ date.ord + as.factor(year) + centroid_lon+centroid_lat+mean_imp_cover_3900*centroid_lat,
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xlat.glm)


fall.duration.imp3900xlon.glm<- glm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_3900*centroid_lon,
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xlon.glm)


fall.duration.imp3900xBCR.glm<- glm(duration_h ~ date.ord+mean_imp_cover_3900*as.factor(BCR2),
                                    family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xBCR.glm)


#Model selection on the above models
cand.set.fall.duration.5 <- list(fall.duration.imp1859xage.glm, fall.duration.imp1859xsex.glm, 
                                 fall.duration.imp1859xyear.glm, fall.duration.imp1859xdate.glm,
                                 fall.duration.imp1859xlat.glm, fall.duration.imp1859xlon.glm,
                                 fall.duration.imp1859xBCR.glm,fall.duration.imp3900xage.glm, 
                                 fall.duration.imp3900xsex.glm, fall.duration.imp3900xyear.glm, 
                                 fall.duration.imp3900xdate.glm, fall.duration.imp3900xlat.glm, 
                                 fall.duration.imp3900xlon.glm, fall.duration.imp3900xBCR.glm,
                                 fall.duration.imp1859.glm, fall.duration.imp3900.glm,
                                 fall.duration.latpluslon.glm)
modnames.fall.duration.5 <- c("fall.duration.imp1859xage.glm", "fall.duration.imp1859xsex.glm", 
                              "fall.duration.imp1859xyear.glm", "fall.duration.imp1859xdate.glm",
                              "fall.duration.imp1859xlat.glm", "fall.duration.imp1859xlon.glm",
                              "fall.duration.imp1859xBCR.glm", "fall.duration.imp3900xage.glm", 
                              "fall.duration.imp3900xsex.glm", "fall.duration.imp3900xyear.glm", 
                              "fall.duration.imp3900xdate.glm","fall.duration.imp3900xlat.glm", 
                              "fall.duration.imp3900xlon.glm","fall.duration.imp3900xBCR.glm",
                              "fall.duration.imp1859.glm", "fall.duration.imp3900.glm",
                              "fall.duration.latpluslon.glm")
aictab(cand.set=cand.set.fall.duration.5, modnames = modnames.fall.duration.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.fall.duration.6 <- list(fall.duration.age.glm, fall.duration.sex.glm, 
                                 fall.duration.ageplussex.glm, fall.duration.agexsex.glm,
                                 fall.duration.null.glm, fall.duration.year.glm, 
                                 fall.duration.date.glm, fall.duration.yearplusdate.glm,
                                 fall.duration.lat.glm, fall.duration.lon.glm, 
                                 fall.duration.latpluslon.glm, fall.duration.BCR.glm,
                                 fall.duration.imp1859xage.glm, fall.duration.imp1859xsex.glm, 
                                 fall.duration.imp1859xyear.glm, fall.duration.imp1859xdate.glm,
                                 fall.duration.imp1859xlat.glm, fall.duration.imp1859xlon.glm,
                                 fall.duration.imp1859xBCR.glm,fall.duration.imp3900xage.glm, 
                                 fall.duration.imp3900xsex.glm, fall.duration.imp3900xyear.glm, 
                                 fall.duration.imp3900xdate.glm, fall.duration.imp3900xlat.glm, 
                                 fall.duration.imp3900xlon.glm, fall.duration.imp3900xBCR.glm,
                                 fall.duration.imp1859.glm, fall.duration.imp3900.glm,
                                 fall.duration.imp1859only.glm, fall.duration.imp3900only.glm)
modnames.fall.duration.6 <- c("Age", "Sex",
                              "Age + Sex", "Age*Sex",
                              "Null", "Year",
                              "Date", "Year + Date",
                              "Year + Date + Lat", "Year + Date + Lon",
                              "Year + Date + Lat + Lon", "Date + BCR",
                              "Year + Date + Lat + Lon + Imp1.859*Age", "Year + Date + Lat + Lon + Imp1.859*Sex", 
                              "Year + Date + Lat + Lon + Imp1.859*Year", "Year + Date + Lat + Lon + Imp1.859*date",
                              "Year + Date + Lat + Lon + Imp1.859*Lat", "Year + Date + Lat + Lon + Imp1.859*Lon",
                              "Date + Imp1.859*BCR", "Year + Date + Lat + Lon + Imp3.900*Age", 
                              "Year + Date + Lat + Lon + Imp3.900*Sex", "Year + Date + Lat + Lon + Imp3.900*Year", 
                              "Year + Date + Lat + Lon + Imp3.900*Date","Year + Date + Lat + Lon + Imp3.900*Lat", 
                              "Year + Date + Lat + Lon + Imp3.900*Lon","Date + Imp3.900*BCR",
                              "Year + Date + Lat + Lon + Imp1.859", "Year + Date + Lat + Lon + Imp3.900",
                              "Imp1.859", "Imp3.900")
FallDurationFullAIC<- aictab(cand.set=cand.set.fall.duration.6, modnames = modnames.fall.duration.6,  
                             digits = 2, second.ord = T)
FallDurationFullAIC
write.csv(FallDurationFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/FallDurationFullAICSupp.csv")

#Running supported models as lm to generate R squared
fall.duration.imp1859xlat.lm<- lm(duration_h ~ date.ord + as.factor(year) + centroid_lon+centroid_lat+mean_imp_cover_1859*centroid_lat,
                                    data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xlat.lm)


fall.duration.imp1859.lm <- lm(duration_h ~ date.ord+as.factor(year)+centroid_lat+centroid_lon+mean_imp_cover_1859, 
                                  data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859.lm)


fall.duration.imp3900.lm <- lm(duration_h ~ date.ord+as.factor(year)+centroid_lat+centroid_lon+mean_imp_cover_3900, 
                                  data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900.lm)


fall.duration.imp3900xlat.lm<- lm(duration_h ~ date.ord + as.factor(year) + centroid_lon+centroid_lat+mean_imp_cover_3900*centroid_lat,
                                    data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xlat.lm)


fall.duration.latpluslon.lm <-lm(duration_h ~ date.ord+as.factor(year)+centroid_lat+centroid_lon,
                                   data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.latpluslon.lm)

fall.duration.imp1859xlon.lm<- lm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_1859*centroid_lon,
                                    data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp1859xlon.lm)

fall.duration.imp3900xlon.lm<- lm(duration_h ~ date.ord+as.factor(year)+centroid_lon+centroid_lat+mean_imp_cover_3900*centroid_lon,
                                    data=data.fall.stopover.agesex, weights=w)
summary(fall.duration.imp3900xlon.lm)

##########################################################################################################
##########Spring duration_h model selection############################
#install.packages("AICcmodavg")
library(AICcmodavg)

#Running models for the effects of each combination of individual variables (age and sex)
#on stopover duration
spring.duration.age.glm <-glm(duration_h ~ as.factor(age), family=gaussian, 
                              data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.age.glm)

spring.duration.sex.glm<-glm(duration_h ~ as.factor(sex), family=gaussian, 
                             data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.sex.glm)

spring.duration.ageplussex.glm <-glm(duration_h ~ as.factor(age)+as.factor(sex), family=gaussian, 
                                     data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.ageplussex.glm)

spring.duration.agexsex.glm <- glm(duration_h ~ as.factor(age)*as.factor(sex), family=gaussian, 
                                   data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.agexsex.glm)

spring.duration.null.glm <- glm(duration_h ~ 1, family=gaussian, 
                                data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.null.glm)

#AICc model selection on the above models
cand.set.spring.duration.1 <- list(spring.duration.age.glm, spring.duration.sex.glm, 
                                   spring.duration.ageplussex.glm, 
                                   spring.duration.agexsex.glm, spring.duration.null.glm)
modnames.spring.duration.1 <- c("spring.duration.age.glm", "spring.duration.sex.glm", 
                                "spring.duration.ageplussex.glm", "spring.duration.agexsex.glm", 
                                "spring.duration.null.glm")
aictab(cand.set=cand.set.spring.duration.1, modnames = modnames.spring.duration.1,  
       digits = 2, second.ord = T)
##I used the null model because the two models above it had non-significant effects

#Running models on the effects of temporal variables (year and date) on duration
spring.duration.year.glm <-glm(duration_h ~ as.factor(year), family=gaussian, 
                               data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.year.glm)


spring.duration.date.glm <-glm(duration_h ~ date.ord, family=gaussian, 
                               data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.date.glm)


spring.duration.yearplusdate.glm <-glm(duration_h ~ date.ord+as.factor(year), family=gaussian, 
                                       data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.yearplusdate.glm)

#Model selection on the above models
cand.set.spring.duration.2 <- list(spring.duration.year.glm, spring.duration.date.glm, 
                                   spring.duration.yearplusdate.glm,
                                   spring.duration.null.glm)
modnames.spring.duration.2 <- c("spring.duration.year.glm", "spring.duration.date.glm", 
                                "spring.duration.yearplusdate.glm",
                                "spring.duration.null.glm")
aictab(cand.set=cand.set.spring.duration.2, modnames = modnames.spring.duration.2,  
       digits = 2, second.ord = T)
##Null is the most supported

#Running models on the spatial variables (lat, lon, and BCR) 

spring.duration.lat.glm <-glm(duration_h ~ centroid_lat, family=gaussian, 
                              data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.lat.glm)


spring.duration.lon.glm <-glm(duration_h ~ centroid_lon, family=gaussian, 
                              data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.lon.glm)


spring.duration.latpluslon.glm <-glm(duration_h ~ centroid_lat+centroid_lon,
                                     family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.latpluslon.glm)


spring.duration.BCR.glm <-glm(duration_h ~ as.factor(BCR2), family=gaussian, 
                              data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.BCR.glm)


#Model selection on the above models
cand.set.spring.duration.3 <- list(spring.duration.lat.glm, spring.duration.lon.glm, 
                                   spring.duration.latpluslon.glm,
                                   spring.duration.BCR.glm, spring.duration.null.glm)
modnames.spring.duration.3 <- c("spring.duration.lat.glm", "spring.duration.lon.glm", 
                                "spring.duration.latpluslon.glm",
                                "spring.duration.BCR.glm", "spring.duration.null.glm")
aictab(cand.set=cand.set.spring.duration.3, modnames = modnames.spring.duration.3,  
       digits = 2, second.ord = T)
##lat, lon, and null supported (lat and lon non-significant)
#Testing the most supported null model with both mean impervious radii

spring.duration.imp1859.glm <- glm(duration_h ~ mean_imp_cover_1859, 
                                   family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859.glm)

spring.duration.imp3900.glm <- glm(duration_h ~ mean_imp_cover_3900, 
                                   family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900.glm)

#Model selection on the above models
cand.set.spring.duration.4 <- list(spring.duration.imp1859.glm, spring.duration.imp3900.glm, 
                                   spring.duration.null.glm)
modnames.spring.duration.4 <- c("spring.duration.imp1859.glm", "spring.duration.imp3900.glm", 
                                "spring.duration.null.glm")
aictab(cand.set=cand.set.spring.duration.4, modnames = modnames.spring.duration.4,  
       digits = 2, second.ord = T)
##All three models are supported

#Model selection with impervious surfaces and interactions
##age
spring.duration.imp1859xage.glm<- glm(duration_h ~ mean_imp_cover_1859*as.factor(age),
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859xage.glm)

##sex
spring.duration.imp1859xsex.glm<- glm(duration_h ~ mean_imp_cover_1859*as.factor(sex),
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859xsex.glm)

##year
spring.duration.imp1859xyear.glm<- glm(duration_h ~ mean_imp_cover_1859*as.factor(year),
                                       family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859xyear.glm)

##date
spring.duration.imp1859xdate.glm<- glm(duration_h ~ mean_imp_cover_1859*date.ord,
                                       family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.impx1859date.glm)

##lat
spring.duration.imp1859xlat.glm<- glm(duration_h ~ mean_imp_cover_1859*centroid_lat,
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859xlat.glm)

##lon
spring.duration.imp1859xlon.glm<- glm(duration_h ~ mean_imp_cover_1859*centroid_lon,
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859xlon.glm)

##BCR
spring.duration.imp1859xBCR.glm<- glm(duration_h ~ mean_imp_cover_1859*as.factor(BCR2),
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859xBCR.glm)
#and for the larger radius as well

##age
spring.duration.imp3900xage.glm<- glm(duration_h ~ mean_imp_cover_3900*as.factor(age),
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900xage.glm)

##sex
spring.duration.imp3900xsex.glm<- glm(duration_h ~ mean_imp_cover_3900*as.factor(sex),
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900xsex.glm)

##year
spring.duration.imp3900xyear.glm<- glm(duration_h ~ mean_imp_cover_3900*as.factor(year),
                                       family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900xyear.glm)

##date
spring.duration.imp3900xdate.glm<- glm(duration_h ~ mean_imp_cover_3900*date.ord,
                                       family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900xdate.glm)

##lat
spring.duration.imp3900xlat.glm<- glm(duration_h ~ mean_imp_cover_3900*centroid_lat,
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900xlat.glm)

##lon
spring.duration.imp3900xlon.glm<- glm(duration_h ~ mean_imp_cover_3900*centroid_lon,
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900xlon.glm)

##BCR
spring.duration.imp3900xBCR.glm<- glm(duration_h ~ mean_imp_cover_3900*as.factor(BCR2),
                                      family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp3900xBCR.glm)


#Model selection on the above models
cand.set.spring.duration.5 <- list(spring.duration.imp1859xage.glm, spring.duration.imp1859xsex.glm, 
                                   spring.duration.imp1859xyear.glm, spring.duration.imp1859xdate.glm,
                                   spring.duration.imp1859xlat.glm, spring.duration.imp1859xlon.glm,
                                   spring.duration.imp1859xBCR.glm, spring.duration.imp3900xage.glm, 
                                   spring.duration.imp3900xsex.glm, spring.duration.imp3900xyear.glm, 
                                   spring.duration.imp3900xdate.glm, spring.duration.imp3900xlat.glm, 
                                   spring.duration.imp3900xlon.glm, spring.duration.imp3900xBCR.glm,
                                   spring.duration.imp1859.glm, spring.duration.imp3900.glm,
                                   spring.duration.null.glm)
modnames.spring.duration.5 <- c("spring.duration.imp1859xage.glm", "spring.duration.imp1859xsex.glm", 
                                "spring.duration.imp1859xyear.glm", "spring.duration.imp1859xdate.glm",
                                "spring.duration.imp1859xlat.glm", "spring.duration.imp1859xlon.glm",
                                "spring.duration.imp1859xBCR.glm", "spring.duration.imp3900xage.glm", 
                                "spring.duration.imp3900xsex.glm", "spring.duration.imp3900xyear.glm", 
                                "spring.duration.imp3900xdate.glm","spring.duration.imp3900xlat.glm", 
                                "spring.duration.imp3900xlon.glm","spring.duration.imp3900xBCR.glm",
                                "spring.duration.imp1859.glm", "spring.duration.imp3900.glm",
                                "spring.duration.null.glm")
aictab(cand.set=cand.set.spring.duration.5, modnames = modnames.spring.duration.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.spring.duration.6 <- list(spring.duration.age.glm, spring.duration.sex.glm, 
                                   spring.duration.ageplussex.glm, spring.duration.agexsex.glm,
                                   spring.duration.null.glm, spring.duration.year.glm, 
                                   spring.duration.date.glm, spring.duration.yearplusdate.glm,
                                   spring.duration.lat.glm, spring.duration.lon.glm, 
                                   spring.duration.latpluslon.glm, spring.duration.BCR.glm,
                                   spring.duration.imp1859xage.glm, spring.duration.imp1859xsex.glm, 
                                   spring.duration.imp1859xyear.glm, spring.duration.imp1859xdate.glm,
                                   spring.duration.imp1859xlat.glm, spring.duration.imp1859xlon.glm,
                                   spring.duration.imp1859xBCR.glm,spring.duration.imp3900xage.glm, 
                                   spring.duration.imp3900xsex.glm, spring.duration.imp3900xyear.glm, 
                                   spring.duration.imp3900xdate.glm, spring.duration.imp3900xlat.glm, 
                                   spring.duration.imp3900xlon.glm, spring.duration.imp3900xBCR.glm,
                                   spring.duration.imp1859.glm, spring.duration.imp3900.glm)
modnames.spring.duration.6 <- c("Age", "Sex",
                                "Age + Sex", "Age*Sex",
                                "Null", "Year",
                                "Date", "Year + Date",
                                "Lat", "Lon",
                                "Lat + Lon", "BCR",
                                "Imp1.859*Age", "Imp1.859*Sex", 
                                "Imp1.859*Year", "Imp1.859*Date",
                                "Imp1.859*Lat", "Imp1.859*Lon",
                                "Imp1.859*BCR", "Imp3.900*Age", 
                                "Imp3.900*Sex", "Imp3.900*Year", 
                                "Imp3.900*Date","Imp3.900*Lat", 
                                "Imp3.900*Lon","Imp3.900*BCR",
                                "Imp1.859", "Imp3.900")
SpringDurationFullAIC<- aictab(cand.set=cand.set.spring.duration.6, modnames = modnames.spring.duration.6,  
                               digits = 2, second.ord = T)
SpringDurationFullAIC
write.csv(SpringDurationFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/SpringDuration2FullAICSupp.csv")

#Running supported models as lm to generate R squared
spring.duration.age.lm <-lm(duration_h ~ as.factor(age),  
                              data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.age.lm)


spring.duration.ageplussex.lm <-lm(duration_h ~ as.factor(age)+as.factor(sex),  
                                     data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.ageplussex.lm)


spring.duration.sex.lm<-lm(duration_h ~ as.factor(sex),  
                             data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.sex.lm)

spring.duration.imp1859.lm <- lm(duration_h ~ mean_imp_cover_1859, 
                                    data=data.spring.stopover.agesex.2, weights=w)
summary(spring.duration.imp1859.lm)

############################################################################################
#########################################################################################
###Model selection for within stop step length in the Fall####
library(AICcmodavg)

#Running models for the effects of each combination of individual variables (age and sex)
#on stopover duration
fall.withinstep.age.glm <-glm(mean_step_ ~ as.factor(age), family=gaussian, 
                              data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.age.glm)

fall.withinstep.sex.glm<-glm(mean_step_ ~ as.factor(sex), family=gaussian, 
                             data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.sex.glm)

fall.withinstep.ageplussex.glm <-glm(mean_step_ ~ as.factor(age)+as.factor(sex), family=gaussian, 
                                     data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.ageplussex.glm)

fall.withinstep.agexsex.glm <- glm(mean_step_ ~ as.factor(age)*as.factor(sex), family=gaussian, 
                                   data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.agexsex.glm)

fall.withinstep.null.glm <- glm(mean_step_ ~ 1, family=gaussian, 
                                data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.null.glm)

#AICc model selection on the above models
cand.set.fall.withinstep.1 <- list(fall.withinstep.age.glm, fall.withinstep.sex.glm, fall.withinstep.ageplussex.glm, 
                                   fall.withinstep.agexsex.glm, fall.withinstep.null.glm)
modnames.fall.withinstep.1 <- c("fall.withinstep.age.glm", "fall.withinstep.sex.glm", 
                                "fall.withinstep.ageplussex.glm", "fall.withinstep.agexsex.glm", 
                                "fall.withinstep.null.glm")
aictab(cand.set=cand.set.fall.withinstep.1, modnames = modnames.fall.withinstep.1,  
       digits = 2, second.ord = T)
##Because the null model was the most supported(deltaAICc=0), I won't be including 
##age or sex in the rest of my stopover duration analysis 

#Running models on the effects of temporal variables (year and date) on duration
#using the most supported individual effects model
#in this case, the null model
fall.withinstep.year.glm <-glm(mean_step_ ~ as.factor(year), family=gaussian, 
                               data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.year.glm)

fall.withinstep.date.glm <-glm(mean_step_ ~ date.ord, family=gaussian, 
                               data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.date.glm)

fall.withinstep.yearplusdate.glm <-glm(mean_step_ ~ date.ord+as.factor(year), family=gaussian, 
                                       data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.yearplusdate.glm)

#Model selection on the above models
cand.set.fall.withinstep.2 <- list(fall.withinstep.year.glm, fall.withinstep.date.glm, 
                                   fall.withinstep.yearplusdate.glm,
                                   fall.withinstep.null.glm)
modnames.fall.withinstep.2 <- c("fall.withinstep.year.glm", "fall.withinstep.date.glm", 
                                "fall.withinstep.yearplusdate.glm",
                                "fall.withinstep.null.glm")
aictab(cand.set=cand.set.fall.withinstep.2, modnames = modnames.fall.withinstep.2,  
       digits = 2, second.ord = T)
##Because year was the most supported model, I'll use that in the next step
#Running models on the spatial variables (lat, lon, and BCR) using the most supported
#temporal and individual model, 

fall.withinstep.lat.glm <-glm(mean_step_ ~ as.factor(year)+centroid_lat, family=gaussian, 
                              data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.lat.glm)


fall.withinstep.lon.glm <-glm(mean_step_ ~ as.factor(year)+centroid_lon, family=gaussian, 
                              data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.lon.glm)


fall.withinstep.latpluslon.glm <-glm(mean_step_ ~ as.factor(year)+centroid_lat+centroid_lon,
                                     family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.latpluslon.glm)

#Not including year because previous analysis found it's correlated with BCR
fall.withinstep.BCR.glm <-glm(mean_step_ ~ as.factor(BCR2), family=gaussian, 
                              data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.BCR.glm)


#Model selection on the above models
cand.set.fall.withinstep.3 <- list(fall.withinstep.lat.glm, fall.withinstep.lon.glm, 
                                   fall.withinstep.latpluslon.glm,
                                   fall.withinstep.BCR.glm, fall.withinstep.year.glm)
modnames.fall.withinstep.3 <- c("fall.withinstep.lat.glm", "fall.withinstep.lon.glm", 
                                "fall.withinstep.latpluslon.glm",
                                "fall.withinstep.BCR.glm", "fall.withinstep.year.glm")
aictab(cand.set=cand.set.fall.withinstep.3, modnames = modnames.fall.withinstep.3,  
       digits = 2, second.ord = T)
##Because the lat plus lon model was the most supported, I'll use that one in the next step
#Even though lon didn't have a significant effect on its own, because lat had a significant effect 
#only when included with lon but not on its own

#Testing the most supported model with both mean impervious radii

fall.withinstep.imp1859.glm <- glm(mean_step_~as.factor(year)+centroid_lat+centroid_lon+
                                     mean_imp_cover_1859, 
                                   family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859.glm)

fall.withinstep.imp3900.glm <- glm(mean_step_ ~ as.factor(year)+centroid_lat+centroid_lon+
                                     mean_imp_cover_3900, 
                                   family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900.glm)

#Also including models with only the impervious surface variable
fall.withinstep.imp1859only.glm <- glm(mean_step_~mean_imp_cover_1859, 
                                       family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859only.glm)

fall.withinstep.imp3900only.glm <- glm(mean_step_ ~ mean_imp_cover_3900, 
                                       family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900only.glm)

#Model selection on the above models
cand.set.fall.withinstep.4 <- list(fall.withinstep.imp1859.glm, fall.withinstep.imp3900.glm, 
                                   fall.withinstep.imp1859only.glm, fall.withinstep.imp3900only.glm,
                                   fall.withinstep.latpluslon.glm)
modnames.fall.withinstep.4 <- c("fall.withinstep.imp1859.glm", "fall.withinstep.imp3900.glm", 
                                "fall.withinstep.imp1859only.glm", "fall.withinstep.imp3900only.glm",
                                "fall.withinstep.latpluslon.glm")
aictab(cand.set=cand.set.fall.withinstep.4, modnames = modnames.fall.withinstep.4,  
       digits = 2, second.ord = T)
##lat plus lon still most supported

#Model selection with impervious surfaces and interactions

#age
fall.withinstep.imp1859xage.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                        mean_imp_cover_1859*as.factor(age),
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859xage.glm)

#sex
fall.withinstep.imp1859xsex.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                        mean_imp_cover_1859*as.factor(sex),
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859xsex.glm)

#year
fall.withinstep.imp1859xyear.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                         mean_imp_cover_1859*as.factor(year),
                                       family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859xyear.glm)

#date
fall.withinstep.imp1859xdate.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                         mean_imp_cover_1859*date.ord,
                                       family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.impx1859date.glm)

#lat
fall.withinstep.imp1859xlat.glm<- glm(mean_step_ ~ as.factor(year) + centroid_lon+centroid_lat+
                                        mean_imp_cover_1859*centroid_lat,
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859xlat.glm)

#lon
fall.withinstep.imp1859xlon.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                        mean_imp_cover_1859*centroid_lon,
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859xlon.glm)

#BCR
#Not including year, lat, or lon due to correlations
fall.withinstep.imp1859xBCR.glm<- glm(mean_step_ ~ mean_imp_cover_1859*as.factor(BCR2),
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859xBCR.glm)
#and for the larger radius as well

#age
fall.withinstep.imp3900xage.glm<- glm(mean_step_ ~as.factor(year)+centroid_lon+centroid_lat+
                                        mean_imp_cover_3900*as.factor(age),
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xage.glm)

#sex
fall.withinstep.imp3900xsex.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                        mean_imp_cover_3900*as.factor(sex),
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xsex.glm)

#year
fall.withinstep.imp3900xyear.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                         mean_imp_cover_3900*as.factor(year),
                                       family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xyear.glm)

#date
fall.withinstep.imp3900xdate.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                         mean_imp_cover_3900*date.ord,
                                       family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xdate.glm)

#lat
fall.withinstep.imp3900xlat.glm<- glm(mean_step_ ~ as.factor(year) + centroid_lon+centroid_lat+
                                        mean_imp_cover_3900*centroid_lat,
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xlat.glm)

#lon
fall.withinstep.imp3900xlon.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                        mean_imp_cover_3900*centroid_lon,
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xlon.glm)

#BCR
#not including year, lat, or lon
fall.withinstep.imp3900xBCR.glm<- glm(mean_step_ ~ mean_imp_cover_3900*as.factor(BCR2),
                                      family=gaussian, data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xBCR.glm)


#Model selection on the above models
cand.set.fall.withinstep.5 <- list(fall.withinstep.imp1859xage.glm, fall.withinstep.imp1859xsex.glm, 
                                   fall.withinstep.imp1859xyear.glm, fall.withinstep.imp1859xdate.glm,
                                   fall.withinstep.imp1859xlat.glm, fall.withinstep.imp1859xlon.glm,
                                   fall.withinstep.imp1859xBCR.glm, fall.withinstep.imp3900xage.glm, 
                                   fall.withinstep.imp3900xsex.glm, fall.withinstep.imp3900xyear.glm, 
                                   fall.withinstep.imp3900xdate.glm, fall.withinstep.imp3900xlat.glm, 
                                   fall.withinstep.imp3900xlon.glm, fall.withinstep.imp3900xBCR.glm,
                                   fall.withinstep.imp1859.glm, fall.withinstep.imp3900.glm,
                                   fall.withinstep.imp1859only.glm, fall.withinstep.imp3900only.glm,
                                   fall.withinstep.latpluslon.glm)
modnames.fall.withinstep.5 <- c("fall.withinstep.imp1859xage.glm", "fall.withinstep.imp1859xsex.glm", 
                                "fall.withinstep.imp1859xyear.glm", "fall.withinstep.imp1859xdate.glm",
                                "fall.withinstep.imp1859xlat.glm", "fall.withinstep.imp1859xlon.glm",
                                "fall.withinstep.imp1859xBCR.glm", "fall.withinstep.imp3900xage.glm", 
                                "fall.withinstep.imp3900xsex.glm", "fall.withinstep.imp3900xyear.glm", 
                                "fall.withinstep.imp3900xdate.glm","fall.withinstep.imp3900xlat.glm", 
                                "fall.withinstep.imp3900xlon.glm","fall.withinstep.imp3900xBCR.glm",
                                "fall.withinstep.imp1859.glm", "fall.withinstep.imp3900.glm",
                                "fall.withinstep.imp1859only.glm", "fall.withinstep.imp3900only.glm",
                                "fall.withinstep.latpluslon.glm")
aictab(cand.set=cand.set.fall.withinstep.5, modnames = modnames.fall.withinstep.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.fall.withinstep.6 <- list(fall.withinstep.age.glm, fall.withinstep.sex.glm, 
                                   fall.withinstep.ageplussex.glm, fall.withinstep.agexsex.glm,
                                   fall.withinstep.null.glm, fall.withinstep.year.glm, 
                                   fall.withinstep.date.glm, fall.withinstep.yearplusdate.glm,
                                   fall.withinstep.lat.glm, fall.withinstep.lon.glm, 
                                   fall.withinstep.latpluslon.glm, fall.withinstep.BCR.glm,
                                   fall.withinstep.imp1859xage.glm, fall.withinstep.imp1859xsex.glm, 
                                   fall.withinstep.imp1859xyear.glm, fall.withinstep.imp1859xdate.glm,
                                   fall.withinstep.imp1859xlat.glm, fall.withinstep.imp1859xlon.glm,
                                   fall.withinstep.imp1859xBCR.glm,fall.withinstep.imp3900xage.glm, 
                                   fall.withinstep.imp3900xsex.glm, fall.withinstep.imp3900xyear.glm, 
                                   fall.withinstep.imp3900xdate.glm, fall.withinstep.imp3900xlat.glm, 
                                   fall.withinstep.imp3900xlon.glm, fall.withinstep.imp3900xBCR.glm,
                                   fall.withinstep.imp1859.glm, fall.withinstep.imp3900.glm,
                                   fall.withinstep.imp1859only.glm, fall.withinstep.imp3900only.glm)
modnames.fall.withinstep.6 <- c("Age", "Sex",
                                "Age + Sex", "Age*Sex",
                                "Null", "Year",
                                "Date", "Year + Date",
                                "Year + Lat", "Year + Lon",
                                "Year + Lat + Lon", "BCR",
                                "Year + Lat + Lon + Imp1.859*Age", "Year + Lat + Lon + Imp1.859*Sex", 
                                "Year + Lat + Lon + Imp1.859*Year", "Year + Lat + Lon + Imp1.859*Date",
                                "Year + Lat + Lon + Imp1.859*Lat", "Year + Lat + Lon + Imp1.859*Lon",
                                "Imp1859*BCR", "Year + Lat + Lon + Imp3.900*Age", 
                                "Year + Lat + Lon + Imp3.900*Sex", "Year + Lat + Lon + Imp3.900*Year", 
                                "Year + Lat + Lon + Imp3.900*Date","Year + Lat + Lon + Imp3.900*Lat", 
                                "Year + Lat + Lon + Imp3.900*Lon","Imp3.900*BCR",
                                "Year + Lat + Lon + Imp1.859", "Year + Lat + Lon + Imp3.900",
                                "Imp1.859", "Imp3.900")
FallWithinStepFullAIC<- aictab(cand.set=cand.set.fall.withinstep.6, modnames = modnames.fall.withinstep.6,  
                               digits = 2, second.ord = T)
FallWithinStepFullAIC
#write.csv(FallWithinStepFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/FallWithinStepFullAIC2.csv")

#Running supported models as lm to generate R squared
fall.withinstep.imp3900xyear.lm<- lm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                         mean_imp_cover_3900*as.factor(year),
                                  data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp3900xyear.lm)

fall.withinstep.imp1859xyear.lm<- lm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                         mean_imp_cover_1859*as.factor(year),
                                        data=data.fall.stopover.agesex, weights=w)
summary(fall.withinstep.imp1859xyear.lm)
############Model selection for spring within stop step lengths ##############
library(AICcmodavg)
#Running models for the effects of each combination of individual variables (age and sex)
#on within stop step length
spring.withinstep.age.glm <-glm(mean_step_ ~ as.factor(age), family=gaussian, 
                                data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.age.glm)

spring.withinstep.sex.glm<-glm(mean_step_ ~ as.factor(sex), family=gaussian, 
                               data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.sex.glm)

spring.withinstep.ageplussex.glm <-glm(mean_step_ ~ as.factor(age)+as.factor(sex), family=gaussian, 
                                       data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.ageplussex.glm)

spring.withinstep.agexsex.glm <- glm(mean_step_ ~ as.factor(age)*as.factor(sex), family=gaussian, 
                                     data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.agexsex.glm)

spring.withinstep.null.glm <- glm(mean_step_ ~ 1, family=gaussian, 
                                  data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.null.glm)

#AICc model selection on the above models
cand.set.spring.withinstep.1 <- list(spring.withinstep.age.glm, spring.withinstep.sex.glm, 
                                     spring.withinstep.ageplussex.glm, 
                                     spring.withinstep.agexsex.glm, spring.withinstep.null.glm)
modnames.spring.withinstep.1 <- c("spring.withinstep.age.glm", "spring.withinstep.sex.glm", 
                                  "spring.withinstep.ageplussex.glm", "spring.withinstep.agexsex.glm", 
                                  "spring.withinstep.null.glm")
aictab(cand.set=cand.set.spring.withinstep.1, modnames = modnames.spring.withinstep.1,  
       digits = 2, second.ord = T)
##Because the agexsex was the most supported(deltaAICc=0), I'll be using that for the next round
#Running models on the effects of temporal variables (year and date) on duration
#using the most supported individual effects model
#in this case, agexsex (but not including age and year in the same model)
#since neither age nor sex is significant on its own, I'm removing both variables if one is correlated
spring.withinstep.year.glm <-glm(mean_step_ ~ as.factor(year), family=gaussian, 
                                 data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.year.glm)


spring.withinstep.date.glm <-glm(mean_step_ ~ (as.factor(sex)*as.factor(age))+date.ord, family=gaussian, 
                                 data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.date.glm)


spring.withinstep.yearplusdate.glm <-glm(mean_step_ ~ as.factor(year)+date.ord, family=gaussian, 
                                         data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.yearplusdate.glm)

#Model selection on the above models
cand.set.spring.withinstep.2 <- list(spring.withinstep.year.glm, spring.withinstep.date.glm, 
                                     spring.withinstep.yearplusdate.glm, spring.withinstep.agexsex.glm)
modnames.spring.withinstep.2 <- c("spring.withinstep.year.glm", "spring.withinstep.date.glm", 
                                  "spring.withinstep.yearplusdate.glm", "spring.withinstep.agexsex.glm")
aictab(cand.set=cand.set.spring.withinstep.2, modnames = modnames.spring.withinstep.2,  
       digits = 2, second.ord = T)

#year plus date was the most supported model
#but the date effect in that model was non-significant, so I'm just using year
#correlations between BCR and sex and lat and date are not an issue in this case

spring.withinstep.lat.glm <-glm(mean_step_ ~ as.factor(year)+ centroid_lat, family=gaussian, 
                                data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.lat.glm)


spring.withinstep.lon.glm <-glm(mean_step_ ~ as.factor(year)+centroid_lon, family=gaussian, 
                                data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.lon.glm)


spring.withinstep.latpluslon.glm <-glm(mean_step_ ~ as.factor(year)+centroid_lat+centroid_lon,
                                       family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.latpluslon.glm)


spring.withinstep.BCR.glm <-glm(mean_step_ ~ as.factor(year)+as.factor(BCR2), family=gaussian, 
                                data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.BCR.glm)


#Model selection on the above models
cand.set.spring.withinstep.3<- list(spring.withinstep.lat.glm, spring.withinstep.lon.glm, 
                                    spring.withinstep.latpluslon.glm,
                                    spring.withinstep.BCR.glm, spring.withinstep.year.glm)
modnames.spring.withinstep.3 <- c("spring.withinstep.lat.glm", "spring.withinstep.lon.glm", 
                                  "spring.withinstep.latpluslon.glm",
                                  "spring.withinstep.BCR.glm", "spring.withinstep.year.glm")
aictab(cand.set=cand.set.spring.withinstep.3, modnames = modnames.spring.withinstep.3,  
       digits = 2, second.ord = T)

##year was still the most supported

spring.withinstep.imp1859.glm <- glm(mean_step_~ as.factor(year)+mean_imp_cover_1859, 
                                     family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859.glm)


spring.withinstep.imp3900.glm <- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_3900, 
                                     family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900.glm)


spring.withinstep.imp1859only.glm <- glm(mean_step_~ mean_imp_cover_1859, 
                                         family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859only.glm)


spring.withinstep.imp3900only.glm <- glm(mean_step_ ~ mean_imp_cover_3900, 
                                         family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900only.glm)

#Model selection on the above models
cand.set.spring.withinstep.4 <- list(spring.withinstep.imp1859.glm, spring.withinstep.imp3900.glm, 
                                     spring.withinstep.imp1859only.glm, spring.withinstep.imp3900only.glm,
                                     spring.withinstep.year.glm)
modnames.spring.withinstep.4 <- c("spring.withinstep.imp1859.glm", "spring.withinstep.imp3900.glm",
                                  "spring.withinstep.imp1859only", "spring.withinstep.imp3900only",
                                  "spring.withinstep.year.glm")
aictab(cand.set=cand.set.spring.withinstep.4, modnames = modnames.spring.withinstep.4,  
       digits = 2, second.ord = T)
## Year still most supported
#And the impervious surface effects are not significant
#Model selection with impervious surfaces and interactions
#not including year in the same model as age

##age (not including year)
spring.withinstep.imp1859xage.glm<- glm(mean_step_ ~ mean_imp_cover_1859*as.factor(age),
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859xage.glm)

##sex
spring.withinstep.imp1859xsex.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_1859*as.factor(sex),
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859xsex.glm)

##year
spring.withinstep.imp1859xyear.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_1859*as.factor(year),
                                         family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859xyear.glm)

##date
spring.withinstep.imp1859xdate.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_1859*date.ord,
                                         family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.impx1859date.glm)

##lat
spring.withinstep.imp1859xlat.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_1859*centroid_lat,
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859xlat.glm)

##lon
spring.withinstep.imp1859xlon.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_1859*centroid_lon,
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859xlon.glm)

##BCR
spring.withinstep.imp1859xBCR.glm<- glm(mean_step_~  as.factor(year)+mean_imp_cover_1859*as.factor(BCR2),
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859xBCR.glm)

#and for the larger radius as well

##age (not including year)
spring.withinstep.imp3900xage.glm<- glm(mean_step_ ~ mean_imp_cover_3900*as.factor(age),
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900xage.glm)

##sex
spring.withinstep.imp3900xsex.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_3900*as.factor(sex),
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900xsex.glm)

##year
spring.withinstep.imp3900xyear.glm<- glm(mean_step_ ~ as.factor(year) + mean_imp_cover_3900*as.factor(year),
                                         family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900xyear.glm)

##date
spring.withinstep.imp3900xdate.glm<- glm(mean_step_ ~ as.factor(year)+ mean_imp_cover_3900*date.ord,
                                         family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900xdate.glm)

##lat
spring.withinstep.imp3900xlat.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_3900*centroid_lat,
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900xlat.glm)

##lon
spring.withinstep.imp3900xlon.glm<- glm(mean_step_ ~ as.factor(year)+ mean_imp_cover_3900*centroid_lon,
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900xlon.glm)

##BCR
spring.withinstep.imp3900xBCR.glm<- glm(mean_step_ ~ as.factor(year)+mean_imp_cover_3900*as.factor(BCR2),
                                        family=gaussian, data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900xBCR.glm)


#Model selection on the above models
cand.set.spring.withinstep.5 <- list(spring.withinstep.imp1859xage.glm, spring.withinstep.imp1859xsex.glm, 
                                     spring.withinstep.imp1859xyear.glm, spring.withinstep.imp1859xdate.glm,
                                     spring.withinstep.imp1859xlat.glm, spring.withinstep.imp1859xlon.glm,
                                     spring.withinstep.imp1859xBCR.glm, spring.withinstep.imp3900xage.glm, 
                                     spring.withinstep.imp3900xsex.glm, spring.withinstep.imp3900xyear.glm, 
                                     spring.withinstep.imp3900xdate.glm, spring.withinstep.imp3900xlat.glm, 
                                     spring.withinstep.imp3900xlon.glm, spring.withinstep.imp3900xBCR.glm,
                                     spring.withinstep.imp1859.glm, spring.withinstep.imp3900.glm,
                                     spring.withinstep.imp1859only.glm, spring.withinstep.imp3900only.glm,
                                     spring.withinstep.year.glm)
modnames.spring.withinstep.5 <- c("spring.withinstep.imp1859xage.glm", "spring.withinstep.imp1859xsex.glm", 
                                  "spring.withinstep.imp1859xyear.glm", "spring.withinstep.imp1859xdate.glm",
                                  "spring.withinstep.imp1859xlat.glm", "spring.withinstep.imp1859xlon.glm",
                                  "spring.withinstep.imp1859xBCR.glm", "spring.withinstep.imp3900xage.glm", 
                                  "spring.withinstep.imp3900xsex.glm", "spring.withinstep.imp3900xyear.glm", 
                                  "spring.withinstep.imp3900xdate.glm","spring.withinstep.imp3900xlat.glm", 
                                  "spring.withinstep.imp3900xlon.glm","spring.withinstep.imp3900xBCR.glm",
                                  "spring.withinstep.imp1859.glm", "spring.withinstep.imp3900.glm",
                                  "spring.withinstep.imp1859only.glm", "spring.withinstep.imp3900only.glm",
                                  "spring.withinstep.year.glm")
aictab(cand.set=cand.set.spring.withinstep.5, modnames = modnames.spring.withinstep.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.spring.withinstep.6 <- list(spring.withinstep.age.glm, spring.withinstep.sex.glm, 
                                     spring.withinstep.ageplussex.glm, spring.withinstep.agexsex.glm,
                                     spring.withinstep.null.glm, spring.withinstep.year.glm, 
                                     spring.withinstep.date.glm, spring.withinstep.yearplusdate.glm,
                                     spring.withinstep.lat.glm, spring.withinstep.lon.glm, 
                                     spring.withinstep.latpluslon.glm, spring.withinstep.BCR.glm,
                                     spring.withinstep.imp1859xage.glm, spring.withinstep.imp1859xsex.glm, 
                                     spring.withinstep.imp1859xyear.glm, spring.withinstep.imp1859xdate.glm,
                                     spring.withinstep.imp1859xlat.glm, spring.withinstep.imp1859xlon.glm,
                                     spring.withinstep.imp1859xBCR.glm, spring.withinstep.imp3900xage.glm, 
                                     spring.withinstep.imp3900xsex.glm, spring.withinstep.imp3900xyear.glm, 
                                     spring.withinstep.imp3900xdate.glm, spring.withinstep.imp3900xlat.glm, 
                                     spring.withinstep.imp3900xlon.glm, spring.withinstep.imp3900xBCR.glm,
                                     spring.withinstep.imp1859.glm, spring.withinstep.imp3900.glm,
                                     spring.withinstep.imp1859only.glm, spring.withinstep.imp3900only.glm)
modnames.spring.withinstep.6 <- c("Age", "Sex",
                                  "Age + Sex", "Age*Sex",
                                  "Null", "Year",
                                  "Age*Sex + Date", "Year + Date",
                                  "Year + Lat", "Year + Lon",
                                  "Year + Lat + Lon", "Year + BCR",
                                  "Imp1.859*Age", "Year + Imp1.859*Sex", 
                                  "Year + Imp1.859*Year", "Year + 1.859*Date",
                                  "Year + 1.859*Lat", "Year + Imp1.859*Lon",
                                  "Year + Imp1.859*BCR", "Imp3.900*Age", 
                                  "Year + Imp3.900*Sex", "Year + Imp3.900*Year", 
                                  "Year + Imp3.900*Date","Year + Imp3.900*Lat", 
                                  "Year + Imp3.900*Lon","Year + Imp3.900*BCR",
                                  "Year + Imp1.859", "Year + Imp3.900",
                                  "Imp1.859", "Imp3.900")
SpringWithinStepFullAIC<- aictab(cand.set=cand.set.spring.withinstep.6, modnames = modnames.spring.withinstep.6,  
                                 digits = 2, second.ord = T)
SpringWithinStepFullAIC
#write.csv(SpringWithinStepFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/SpringWithinStepFull2AIC.csv")

#Running supported models as lm to generate R squared
spring.withinstep.yearplusdate.lm <-lm(mean_step_ ~ as.factor(year)+date.ord,  
                                         data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.yearplusdate.lm)


spring.withinstep.year.lm <-lm(mean_step_ ~ as.factor(year),  
                                 data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.year.lm)


spring.withinstep.imp3900.lm <- lm(mean_step_ ~ as.factor(year)+mean_imp_cover_3900, 
                                      data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp3900.lm)


spring.withinstep.lat.lm <-lm(mean_step_ ~ as.factor(year)+ centroid_lat,  
                                data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.lat.lm)


spring.withinstep.lon.lm <-lm(mean_step_ ~ as.factor(year)+centroid_lon,  
                                data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.lon.lm)


spring.withinstep.imp1859.lm <- lm(mean_step_~ as.factor(year)+mean_imp_cover_1859, 
                                     data=data.spring.stopover.agesex.2, weights=w)
summary(spring.withinstep.imp1859.lm)

#######################################################################################
########################################################################################
######Preparing the data for the next variable by adding in the calculated fields into############################## 
#the full datasheet including both stops and stopovers
#Aggregating the BCRs
Woodcock.fall$BCR2 <- "NA"
View(Woodcock.fall)

for (i in 1:nrow(Woodcock.fall)){
  
  Woodcock.fall$BCR2[i]<- ifelse(test=Woodcock.fall$BCR[i]=="12", yes="13", no=
                                   ifelse(test=Woodcock.fall$BCR[i]=="22"|Woodcock.fall$BCR[i]=="23", yes="24", no=
                                            ifelse(test=Woodcock.fall$BCR[i]=="25" |
                                                     Woodcock.fall$BCR[i]=="26" |
                                                     Woodcock.fall$BCR[i]=="37", yes= "27",  
                                                   no = Woodcock.fall$BCR[i])))
  
}
View(Woodcock.fall)

#And for the spring
Woodcock.spring$BCR2 <- "NA"
View(Woodcock.spring)

for (i in 1:nrow(Woodcock.spring)){
  
  Woodcock.spring$BCR2[i]<- ifelse(test=Woodcock.spring$BCR[i]=="12", yes="13", no=
                                     ifelse(test=Woodcock.spring$BCR[i]=="22"|Woodcock.spring$BCR[i]=="23", yes="24", no=
                                              ifelse(test=Woodcock.spring$BCR[i]=="25" |
                                                       Woodcock.spring$BCR[i]=="26" |
                                                       Woodcock.spring$BCR[i]=="37", yes= "27",  
                                                     no = Woodcock.spring$BCR[i])))
  
}
View(Woodcock.spring)

#Calculating the weights
##First, adding a column with the weight for each stopover
###creating an empty column for n in the fall data
data.fall<- data.frame(Woodcock.fall)
data.fall$n<- "NA"
View(data.fall)

##filling in the n column with the number of stopovers for that bird
for (i in 1:nrow(data.fall)){
  data.fall$n[i]<- length(which(data.fall$id==data.fall$id[i]))
}
View(data.fall)

## creating and filling in a weight term to = 1/the number of stopovers for that bird

data.fall$w<- 1/as.numeric(data.fall$n)
View(data.fall)

##And doing the same thing for the spring stopover data
data.spring<- data.frame(Woodcock.spring)
data.spring$n<- "NA"
View(data.spring)

for (i in 1:nrow(data.spring)){
  data.spring$n[i]<- length(which(data.spring$id==data.spring$id[i]))
}
View(data.spring)

data.spring$w<- 1/as.numeric(data.spring$n)
View(data.spring)

#Adding in the age and sex data from the full migration datasheet
##Using the id field to merge the two dataframes and viewing them to check accuracy
data.fall.agesex <- merge(data.fall, data.agesex.fullmigration.fall, by.x="id", by.y="id")
View(data.fall.agesex)
View(data.agesex.fullmigration.fall)

#And doing the same for the spring
View(data.agesex.fullmigration.spring)
data.spring.agesex <- merge(data.spring, data.agesex.fullmigration.spring,
                            by.x="id", by.y="id")
View(data.spring.agesex)

#adding in the dates
#install.packages("anytime")
library(anytime)
#install.packages("lubridate")
library(lubridate)
#yday() returns the ordinal day 

#First, converting the start_time column into the correct date format
data.fall.agesex$date<- anydate(data.fall.agesex$start_time)
View(data.fall.agesex)
#Converting to ordinal date
data.fall.agesex$ord<- yday(data.fall.agesex$date)
View(data.fall.agesex)
## and study date relative to October 8th (first day of migration year)
data.fall.agesex$date.ord<- ifelse(data.fall.agesex$ord>279, 
                                   data.fall.agesex$ord-280, 
                                   data.fall.agesex$ord+85)
View(data.fall.agesex)

#And the same for the spring
#formatting
data.spring.agesex$date<- anydate(data.spring.agesex$start_time)
View(data.spring.agesex)
#ordinal date
data.spring.agesex$ord<- yday(data.spring.agesex$date)
View(data.spring.agesex)
#date relative to October 8th
data.spring.agesex$date.ord<- ifelse(data.spring.agesex$ord>279, 
                                     data.spring.agesex$ord-280, 
                                     data.spring.agesex$ord+85)
View(data.spring.agesex)

#And removing any points with missing age or sex data
data.spring.agesex.2<- subset(data.spring.agesex, age=="Adult"|age=="Young"&
                                sex=="m"|sex=="f")
View(data.spring.agesex.2)
rm(data.spring.agesex)

data.fall.agesex.2<- subset(data.fall.agesex, age=="Adult"|age=="Young"&
                              sex=="m"|sex=="f")
View(data.fall.agesex.2)
rm(data.fall.agesex)
#########################################################################################
################################################################################
####correlations between variables in the unsubset Fall datasheet####
#Using chi-square tests of independence to test for correlations among the
#categorical variables in the Fall
##Age and sex
fall.chisq.agesex <- chisq.test(data.fall.agesex.2$age, 
                                data.fall.agesex.2$sex)
fall.chisq.agesex
##Age and BCR
fall.chisq.ageBCR2<- chisq.test(data.fall.agesex.2$age, 
                                as.factor(data.fall.agesex.2$BCR2))
fall.chisq.ageBCR2
##Sex and BCR
fall.chisq.sexBCR2<- chisq.test(data.fall.agesex.2$sex, 
                                as.factor(data.fall.agesex.2$BCR2))
fall.chisq.sexBCR2
##Age and year
fall.chisq.ageyear<- chisq.test(data.fall.agesex.2$age, 
                                as.factor(data.fall.agesex.2$year))
fall.chisq.ageyear
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
fall.fisher.ageyear<- fisher.test(as.factor(data.fall.agesex.2$age), 
                                  as.factor(data.fall.agesex.2$year))
####Since that didn't fix the error, I tried using a simulated p-value
fall.fisher.ageyear<- fisher.test(as.factor(data.fall.agesex.2$age), 
                                  as.factor(data.fall.agesex.2$year),
                                  simulate.p.value=TRUE)
fall.fisher.ageyear
table(as.factor(data.fall.agesex.2$age), 
      as.factor(data.fall.agesex.2$year))

##Sex and year
fall.chisq.sexyear<- chisq.test(data.fall.agesex.2$sex, 
                                as.factor(data.fall.agesex.2$year))
fall.chisq.sexyear
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
fall.fisher.sexyear<- fisher.test(as.factor(data.fall.agesex.2$sex), 
                                  as.factor(data.fall.agesex.2$year))
####Since that didn't fix the error, I tried using a simulated p-value
fall.fisher.sexyear<- fisher.test(as.factor(data.fall.agesex.2$sex), 
                                  as.factor(data.fall.agesex.2$year),
                                  simulate.p.value=TRUE)
fall.fisher.sexyear
table(as.factor(data.fall.agesex.2$sex), 
      as.factor(data.fall.agesex.2$year))
##BCR and year
fall.chisq.BCRyear<- chisq.test(as.factor(data.fall.agesex.2$BCR2), 
                                as.factor(data.fall.agesex.2$year))
fall.chisq.BCRyear
table(as.factor(data.fall.agesex.2$BCR2), 
      as.factor(data.fall.agesex.2$year))
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
fall.fisher.BCRyear<- fisher.test(as.factor(data.fall.agesex.2$BCR2), 
                                  as.factor(data.fall.agesex.2$year))

####Since that didn't fix the error, I tried using a simulated p-value
fall.fisher.BCRyear<- fisher.test(as.factor(data.fall.agesex.2$BCR2), 
                                  as.factor(data.fall.agesex.2$year),
                                  simulate.p.value=TRUE)
fall.fisher.BCRyear

##And now testing the continuous date, impervious, lat, and lon variables with the binary age and sex variables
#install.packages("ltm")
library(ltm)
###age and lat
fall.biserial.agelat<-biserial.cor(data.fall.agesex.2$centroid_lat, 
                                   data.fall.agesex.2$age)
fall.biserial.agelat
###age and lon
fall.biserial.agelon<- biserial.cor(data.fall.agesex.2$centroid_lon, 
                                    data.fall.agesex.2$age)
fall.biserial.agelon 
###sex and lat
fall.biserial.sexlat<-biserial.cor(data.fall.agesex.2$centroid_lat, 
                                   data.fall.agesex.2$sex)
fall.biserial.sexlat
###sex and lon
fall.biserial.sexlon<-biserial.cor(data.fall.agesex.2$centroid_lon, 
                                   data.fall.agesex.2$sex)
fall.biserial.sexlon
###Age and date
fall.biserial.agedate <- biserial.cor(data.fall.agesex.2$date.ord, 
                                      data.fall.agesex.2$age)
fall.biserial.agedate
###Age and imp 1859
fall.biserial.age1859 <-biserial.cor(data.fall.agesex.2$mean_imp_cover_1859, 
                                     data.fall.agesex.2$age)
fall.biserial.age1859
###Age and impervious 3900
fall.biserial.age3900 <-biserial.cor(data.fall.agesex.2$mean_imp_cover_3900, 
                                     data.fall.agesex.2$age)
fall.biserial.age3900
###Sex and date
fall.biserial.sexdate <-biserial.cor(data.fall.agesex.2$date.ord, 
                                     data.fall.agesex.2$sex)
fall.biserial.sexdate
###Sex and imp 1859m
fall.biserial.sex1859 <-biserial.cor(data.fall.agesex.2$mean_imp_cover_1859, 
                                     data.fall.agesex.2$sex)
fall.biserial.sex1859
###Sex and imp 3900
fall.biserial.sex3900 <-biserial.cor(data.fall.agesex.2$mean_imp_cover_3900, 
                                     data.fall.agesex.2$sex)
fall.biserial.sex3900

#And using a Pearson's correlation coefficient to test the correlations between each combination of continuous variables
###Latitude and date
fall.pearson.latdate <- cor(data.fall.agesex.2$centroid_lat, 
                            data.fall.agesex.2$date.ord)
fall.pearson.latdate
###Latitude and imp 1859
fall.pearson.lat1859 <- cor(data.fall.agesex.2$centroid_lat, 
                            data.fall.agesex.2$mean_imp_cover_1859)
fall.pearson.lat1859
###latitude and imp 3900
fall.pearson.lat3900 <- cor(data.fall.agesex.2$centroid_lat, 
                            data.fall.agesex.2$mean_imp_cover_3900)
fall.pearson.lat3900
###longitude and date
fall.pearson.londate <- cor(data.fall.agesex.2$centroid_lon, 
                            data.fall.agesex.2$date.ord)
fall.pearson.londate
###longitude and imp 1859
fall.pearson.lon1859 <- cor(data.fall.agesex.2$centroid_lon, 
                            data.fall.agesex.2$mean_imp_cover_1859)
fall.pearson.lon1859
###longitude and imp 3900
fall.pearson.lon3900 <- cor(data.fall.agesex.2$centroid_lon, 
                            data.fall.agesex.2$mean_imp_cover_3900)
fall.pearson.lon3900
###Date and imp 1859
fall.pearson.date1859 <- cor(data.fall.agesex.2$mean_imp_cover_1859, 
                             data.fall.agesex.2$date.ord)
fall.pearson.date1859
###Date and imp 3900
fall.pearson.date3900 <- cor(data.fall.agesex.2$mean_imp_cover_3900, 
                             data.fall.agesex.2$date.ord)
fall.pearson.date3900
fall.pearson.latlon <- cor(data.fall.agesex.2$centroid_lat, 
                           data.fall.agesex.2$centroid_lon)
fall.pearson.latlon
#Using linear regression to test the correlation between the multi-category variables 
#and the continuous ones
fall.yearlat.lm <- lm(centroid_lat~as.factor(year),  
                      data=data.fall.agesex.2, weights=w)
summary(fall.yearlat.lm) 


fall.yearlon.lm <- lm(centroid_lon~as.factor(year),  
                      data=data.fall.agesex.2, weights=w)
summary(fall.yearlon.lm) 


fall.yeardate.lm <- lm(date.ord~as.factor(year), 
                       data=data.fall.agesex.2, weights=w)
summary(fall.yeardate.lm) 


fall.year1859.lm <- lm(mean_imp_cover_1859~as.factor(year),  
                       data=data.fall.agesex.2, weights=w)
summary(fall.year1859.lm) 


fall.year3900.lm <- lm(mean_imp_cover_3900~as.factor(year),  
                       data=data.fall.agesex.2, weights=w)
summary(fall.year3900.lm) 


fall.BCRdate.lm <- lm(date.ord~as.factor(BCR2),  
                      data=data.fall.agesex.2, weights=w)
summary(fall.BCRdate.lm) 

fall.BCR1859.lm <- lm(mean_imp_cover_1859~as.factor(BCR2),  
                      data=data.fall.agesex.2, weights=w)
summary(fall.BCR1859.lm) 

fall.BCR3900.lm <- lm(mean_imp_cover_3900~as.factor(BCR2),  
                      data=data.fall.agesex.2, weights=w)
summary(fall.BCR3900.lm) 
#######################################################################################
###########correlations between variables in the unsubset spring datasheet#########
#Using chi-square tests of independence to test for correlations among the
#categorical variables in the Fall
##Age and sex
spring.chisq.agesex <- chisq.test(data.spring.agesex.2$age, 
                                  data.spring.agesex.2$sex)
spring.chisq.agesex
##Age and BCR
spring.chisq.ageBCR2<- chisq.test(data.spring.agesex.2$age, 
                                  as.factor(data.spring.agesex.2$BCR2))
spring.chisq.ageBCR2
#need to do a Fisher's exact test
table(as.factor(data.spring.agesex.2$BCR2), 
      as.factor(data.spring.agesex.2$age))
spring.fisher.BCRage<- fisher.test(as.factor(data.spring.agesex.2$BCR2), 
                                   as.factor(data.spring.agesex.2$age))
####Since there was an error, I tried using a simulated p-value
spring.fisher.BCRage<- fisher.test(as.factor(data.spring.agesex.2$BCR2), 
                                   as.factor(data.spring.agesex.2$age),
                                   simulate.p.value=TRUE)
spring.fisher.BCRage
##Sex and BCR
spring.chisq.sexBCR2<- chisq.test(data.spring.agesex.2$sex, 
                                  as.factor(data.spring.agesex.2$BCR2))
spring.chisq.sexBCR2

#need to do a Fisher's exact test
table(as.factor(data.spring.agesex.2$BCR2), 
      as.factor(data.spring.agesex.2$sex))
spring.fisher.BCRsex<- fisher.test(as.factor(data.spring.agesex.2$BCR2), 
                                   as.factor(data.spring.agesex.2$sex))
####Since there was an error, I tried using a simulated p-value
spring.fisher.BCRsex<- fisher.test(as.factor(data.spring.agesex.2$BCR2), 
                                   as.factor(data.spring.agesex.2$sex),
                                   simulate.p.value=TRUE)
spring.fisher.BCRsex

##Age and year
spring.chisq.ageyear<- chisq.test(data.spring.agesex.2$age, 
                                  as.factor(data.spring.agesex.2$year))
spring.chisq.ageyear
format(spring.chisq.ageyear, scientific=F)
##Sex and year
spring.chisq.sexyear<- chisq.test(data.spring.agesex.2$sex, 
                                  as.factor(data.spring.agesex.2$year))
spring.chisq.sexyear

##BCR and year
spring.chisq.BCRyear<- chisq.test(as.factor(data.spring.agesex.2$BCR2), 
                                  as.factor(data.spring.agesex.2$year))
spring.chisq.BCRyear

###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
spring.fisher.BCRyear<- fisher.test(as.factor(data.spring.agesex.2$BCR2), 
                                    as.factor(data.spring.agesex.2$year))

####Since that didn't fix the error, I tried using a simulated p-value
spring.fisher.BCRyear<- fisher.test(as.factor(data.spring.agesex.2$BCR2), 
                                    as.factor(data.spring.agesex.2$year),
                                    simulate.p.value=TRUE)
spring.fisher.BCRyear

##And now testing the continuous date, impervious, lat, and lon variables with the binary age and sex variables
#install.packages("ltm")
library(ltm)
###age and lat
spring.biserial.agelat<-biserial.cor(data.spring.agesex.2$centroid_lat, 
                                     as.factor(data.spring.agesex.2$age))
spring.biserial.agelat
###age and lon
spring.biserial.agelon<- biserial.cor(data.spring.agesex.2$centroid_lon, 
                                      as.factor(data.spring.agesex.2$age))
spring.biserial.agelon 
###sex and lat
spring.biserial.sexlat<-biserial.cor(data.spring.agesex.2$centroid_lat, 
                                     as.factor(data.spring.agesex.2$sex))
spring.biserial.sexlat
###sex and lon
spring.biserial.sexlon<-biserial.cor(data.spring.agesex.2$centroid_lon, 
                                     as.factor(data.spring.agesex.2$sex))
spring.biserial.sexlon
###Age and date
spring.biserial.agedate <- biserial.cor(data.spring.agesex.2$date.ord, 
                                        as.factor(data.spring.agesex.2$age))
spring.biserial.agedate
###Age and imp 1859
spring.biserial.age1859 <-biserial.cor(data.spring.agesex.2$mean_imp_cover_1859, 
                                       as.factor(data.spring.agesex.2$age))
spring.biserial.age1859
###Age and impervious 3900
spring.biserial.age3900 <-biserial.cor(data.spring.agesex.2$mean_imp_cover_3900, 
                                       as.factor(data.spring.agesex.2$age))
spring.biserial.age3900
###Sex and date
spring.biserial.sexdate <-biserial.cor(data.spring.agesex.2$date.ord, 
                                       as.factor(data.spring.agesex.2$sex))
spring.biserial.sexdate
###Sex and imp 1859m
spring.biserial.sex1859 <-biserial.cor(data.spring.agesex.2$mean_imp_cover_1859, 
                                       as.factor(data.spring.agesex.2$sex))
spring.biserial.sex1859
###Sex and imp 3900
spring.biserial.sex3900 <-biserial.cor(data.spring.agesex.2$mean_imp_cover_3900, 
                                       as.factor(data.spring.agesex.2$sex))
spring.biserial.sex3900

#And using a Pearson's correlation coefficient to test the correlations between each combination of continuous variables
###Latitude and date
spring.pearson.latdate <- cor(data.spring.agesex.2$centroid_lat, 
                              data.spring.agesex.2$date.ord)
spring.pearson.latdate
###Latitude and imp 1859
spring.pearson.lat1859 <- cor(data.spring.agesex.2$centroid_lat, 
                              data.spring.agesex.2$mean_imp_cover_1859)
spring.pearson.lat1859
###latitude and imp 3900
spring.pearson.lat3900 <- cor(data.spring.agesex.2$centroid_lat, 
                              data.spring.agesex.2$mean_imp_cover_3900)
spring.pearson.lat3900
###longitude and date
spring.pearson.londate <- cor(data.spring.agesex.2$centroid_lon, 
                              data.spring.agesex.2$date.ord)
spring.pearson.londate
###longitude and imp 1859
spring.pearson.lon1859 <- cor(data.spring.agesex.2$centroid_lon, 
                              data.spring.agesex.2$mean_imp_cover_1859)
spring.pearson.lon1859
###longitude and imp 3900
spring.pearson.lon3900 <- cor(data.spring.agesex.2$centroid_lon, 
                              data.spring.agesex.2$mean_imp_cover_3900)
spring.pearson.lon3900
###Date and imp 1859
spring.pearson.date1859 <- cor(data.spring.agesex.2$mean_imp_cover_1859, 
                               data.spring.agesex.2$date.ord)
spring.pearson.date1859
###Date and imp 3900
spring.pearson.date3900 <- cor(data.spring.agesex.2$mean_imp_cover_3900, 
                               data.spring.agesex.2$date.ord)
spring.pearson.date3900
#lat and lon
spring.pearson.latlon <- cor(data.spring.agesex.2$centroid_lat, 
                             data.spring.agesex.2$centroid_lon)
spring.pearson.latlon
#Using linear regression to test the correlation between the multi-category variables 
#and the continuous ones
spring.yearlat.lm <- lm(centroid_lat~as.factor(year), 
                        data=data.spring.agesex.2, weights=w)
summary(spring.yearlat.lm) 

spring.yearlon.lm <- lm(centroid_lon~as.factor(year),  
                        data=data.spring.agesex.2, weights=w)
summary(spring.yearlon.lm) 

spring.yeardate.lm <- lm(date.ord~as.factor(year),  
                         data=data.spring.agesex.2, weights=w)
summary(spring.yeardate.lm) 


spring.year1859.lm <- lm(mean_imp_cover_1859~as.factor(year),  
                         data=data.spring.agesex.2, weights=w)
summary(spring.year1859.lm) 


spring.year3900.lm <- lm(mean_imp_cover_3900~as.factor(year),  
                         data=data.spring.agesex.2, weights=w)
summary(spring.year3900.lm) 


spring.BCRdate.lm <- lm(date.ord~as.factor(BCR2),  
                        data=data.spring.agesex.2, weights=w)
summary(spring.BCRdate.lm) 


spring.BCR1859.lm <- lm(mean_imp_cover_1859~as.factor(BCR2),  
                        data=data.spring.agesex.2, weights=w)
summary(spring.BCR1859.lm) 


spring.BCR3900.lm <- lm(mean_imp_cover_3900~as.factor(BCR2),  
                        data=data.spring.agesex.2, weights=w)
summary(spring.BCR3900.lm) 
###################################################################################

################Calculating migratory step distance################################
#First, calculating a field for whether the next observation is the same bird

library(dplyr)
#making sure the points are organized by id and then date
data.fall.agesex.dist<-data.fall.agesex.2 %>%arrange(id, date)
View(data.fall.agesex.dist)
#creating a blank field that will have a 1 or a zero depending on whether the point after it is 
#from the same bird
data.fall.agesex.dist$leg <- "NA"
View(data.fall.agesex.dist)
#Filling in the new column with 1 if the next point is the same bird and 0 if it isn't
for (i in 1:nrow(data.fall.agesex.dist)){
  
  data.fall.agesex.dist$leg[i]<- ifelse(test=data.fall.agesex.dist$uniqueID[i]=="WV-2019-0169fall",
                                        yes="0", no= ifelse(test=data.fall.agesex.dist$id[i]== 
                                                              data.fall.agesex.dist$id[i+1],
                                                            yes="1", no="0"))
  
  
}
View(data.fall.agesex.dist)
#creating a column for the distance until the next stop
data.fall.agesex.dist$step_dist <- "NA"
View(data.fall.agesex.dist)

#install.packages("geosphere")
library(geosphere)
#filling in the distance using the centroid_lon and centroid_lat of the current and next points
for (i in 1:nrow(data.fall.agesex.dist)){
  data.fall.agesex.dist$step_dist[i] <- 
    distVincentyEllipsoid(c(data.fall.agesex.dist$centroid_lon[i], data.fall.agesex.dist$centroid_lat[i]), 
                          c(data.fall.agesex.dist$centroid_lon[i+1], data.fall.agesex.dist$centroid_lat[i+1]))
}
View(data.fall.agesex.dist)
#adding in a column to translate the distances into km and fill it in with "NA" if it's the last stop
#recorded for that bird
data.fall.agesex.dist$step_distkm <- "NA"
#filling in the column row with "NA" for last stops, and the calculated distance/1000 for others
for (i in 1:nrow(data.fall.agesex.dist)){
  data.fall.agesex.dist$step_distkm[i] <- ifelse(test=data.fall.agesex.dist$leg[i]=="1",
                                                 yes=as.numeric(data.fall.agesex.dist$step_dist[i])/1000,
                                                 no="NA")
  
}
View(data.fall.agesex.dist)
#and translating it out of scientific notation
data.fall.agesex.dist$step_distkm<-format(data.fall.agesex.dist$step_distkm, scientific=F)
View(data.fall.agesex.dist)
#Removing all of the rows with NAs
data.fall.agesex.dist2<- subset(data.fall.agesex.dist, as.numeric(step_distkm) !="NA")
View(data.fall.agesex.dist2)
#removing the previous dataframe so that I don't accidentally add it to the models
rm(data.fall.agesex.2)

#and for the spring

#making sure the points are organized by id and then date
data.spring.agesex.dist<-data.spring.agesex.2 %>%arrange(id, date)
View(data.spring.agesex.dist)
#creating a blank field that will have a 1 or a zero depending on whether the point after it is 
#from the same bird
data.spring.agesex.dist$leg <- "NA"
View(data.spring.agesex.dist)
#Filling in the new column with 1 if the next point is the same bird and 0 if it isn't
for (i in 1:nrow(data.spring.agesex.dist)){
  
  data.spring.agesex.dist$leg[i]<- ifelse(test=data.spring.agesex.dist$id[i]== 
                                            data.spring.agesex.dist$id[i+1],
                                          yes="1", no="0")
  
  
}
View(data.spring.agesex.dist)
#creating a column for the distance until the next stop
data.spring.agesex.dist$step_dist <- "NA"
View(data.spring.agesex.dist)

#filling in the distance using the centroid_lon and centroid_lat of the current and next points
for (i in 1:nrow(data.spring.agesex.dist)){
  data.spring.agesex.dist$step_dist[i] <- 
    distVincentyEllipsoid(c(data.spring.agesex.dist$centroid_lon[i], data.spring.agesex.dist$centroid_lat[i]), 
                          c(data.spring.agesex.dist$centroid_lon[i+1], data.spring.agesex.dist$centroid_lat[i+1]))
}
View(data.spring.agesex.dist)
#adding in a column to translate the distances into km and fill it in with NA if it's the last stop
#recorded for that bird
data.spring.agesex.dist$step_distkm <- "NA"

#filling in the column row with "NA" for last stops, and the calculated distance/1000 for others
for (i in 1:nrow(data.spring.agesex.dist)){
  data.spring.agesex.dist$step_distkm[i] <- ifelse(test=data.spring.agesex.dist$leg[i]=="1",
                                                   yes=as.numeric(data.spring.agesex.dist$step_dist[i])/1000,
                                                   no="NA")
  
}
View(data.spring.agesex.dist)
#and translating it out of scientific notation
data.spring.agesex.dist$step_distkm<-format(data.spring.agesex.dist$step_distkm, scientific=F)
View(data.spring.agesex.dist)
#Removing all of the rows with NAs
data.spring.agesex.dist2<- subset(data.spring.agesex.dist, as.numeric(step_distkm) !="NA")
View(data.spring.agesex.dist2)
View(data.spring.agesex.dist)
#removing the previous dataframe so that I don't accidentally add it to the models
rm(data.spring.agesex.2)
rm(data.spring.agesex.dist)



######correlations in fall step distance dataset#####################
#categorical variables in the Fall
##Age and sex
fall.chisq.agesex <- chisq.test(data.fall.agesex.dist2$age, 
                                data.fall.agesex.dist2$sex)
fall.chisq.agesex
##Age and BCR
fall.chisq.ageBCR2<- chisq.test(data.fall.agesex.dist2$age, 
                                as.factor(data.fall.agesex.dist2$BCR2))
fall.chisq.ageBCR2
##Sex and BCR
fall.chisq.sexBCR2<- chisq.test(data.fall.agesex.dist2$sex, 
                                as.factor(data.fall.agesex.dist2$BCR2))
fall.chisq.sexBCR2
##Age and year
fall.chisq.ageyear<- chisq.test(data.fall.agesex.dist2$age, 
                                as.factor(data.fall.agesex.dist2$year))
fall.chisq.ageyear
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
fall.fisher.ageyear<- fisher.test(as.factor(data.fall.agesex.dist2$age), 
                                  as.factor(data.fall.agesex.dist2$year))
####Since that didn't fix the error, I tried using a simulated p-value
fall.fisher.ageyear<- fisher.test(as.factor(data.fall.agesex.dist2$age), 
                                  as.factor(data.fall.agesex.dist2$year),
                                  simulate.p.value=TRUE)
fall.fisher.ageyear
table(as.factor(data.fall.agesex.dist2$age), 
      as.factor(data.fall.agesex.dist2$year))

##Sex and year
fall.chisq.sexyear<- chisq.test(data.fall.agesex.dist2$sex, 
                                as.factor(data.fall.agesex.dist2$year))
fall.chisq.sexyear
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
fall.fisher.sexyear<- fisher.test(as.factor(data.fall.agesex.dist2$sex), 
                                  as.factor(data.fall.agesex.dist2$year))
####Since that didn't fix the error, I tried using a simulated p-value
fall.fisher.sexyear<- fisher.test(as.factor(data.fall.agesex.dist2$sex), 
                                  as.factor(data.fall.agesex.dist2$year),
                                  simulate.p.value=TRUE)
fall.fisher.sexyear
table(as.factor(data.fall.agesex.dist2$sex), 
      as.factor(data.fall.agesex.dist2$year))
##BCR and year
fall.chisq.BCRyear<- chisq.test(as.factor(data.fall.agesex.dist2$BCR2), 
                                as.factor(data.fall.agesex.dist2$year))
fall.chisq.BCRyear
table(as.factor(data.fall.agesex.dist2$BCR2), 
      as.factor(data.fall.agesex.dist2$year))
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
fall.fisher.BCRyear<- fisher.test(as.factor(data.fall.agesex.dist2$BCR2), 
                                  as.factor(data.fall.agesex.dist2$year))

####Since that didn't fix the error, I tried using a simulated p-value
fall.fisher.BCRyear<- fisher.test(as.factor(data.fall.agesex.dist2$BCR2), 
                                  as.factor(data.fall.agesex.dist2$year),
                                  simulate.p.value=TRUE)
fall.fisher.BCRyear

##And now testing the continuous date, impervious, lat, and lon variables with the binary age and sex variables
#install.packages("ltm")
library(ltm)
###age and lat
fall.biserial.agelat<-biserial.cor(data.fall.agesex.dist2$centroid_lat, 
                                   data.fall.agesex.dist2$age)
fall.biserial.agelat
###age and lon
fall.biserial.agelon<- biserial.cor(data.fall.agesex.dist2$centroid_lon, 
                                    data.fall.agesex.dist2$age)
fall.biserial.agelon 
###sex and lat
fall.biserial.sexlat<-biserial.cor(data.fall.agesex.dist2$centroid_lat, 
                                   data.fall.agesex.dist2$sex)
fall.biserial.sexlat
###sex and lon
fall.biserial.sexlon<-biserial.cor(data.fall.agesex.dist2$centroid_lon, 
                                   data.fall.agesex.dist2$sex)
fall.biserial.sexlon
###Age and date
fall.biserial.agedate <- biserial.cor(data.fall.agesex.dist2$date.ord, 
                                      data.fall.agesex.dist2$age)
fall.biserial.agedate
###Age and imp 1859
fall.biserial.age1859 <-biserial.cor(data.fall.agesex.dist2$mean_imp_cover_1859, 
                                     data.fall.agesex.dist2$age)
fall.biserial.age1859
###Age and impervious 3900
fall.biserial.age3900 <-biserial.cor(data.fall.agesex.dist2$mean_imp_cover_3900, 
                                     data.fall.agesex.dist2$age)
fall.biserial.age3900
###Sex and date
fall.biserial.sexdate <-biserial.cor(data.fall.agesex.dist2$date.ord, 
                                     data.fall.agesex.dist2$sex)
fall.biserial.sexdate
###Sex and imp 1859m
fall.biserial.sex1859 <-biserial.cor(data.fall.agesex.dist2$mean_imp_cover_1859, 
                                     data.fall.agesex.dist2$sex)
fall.biserial.sex1859
###Sex and imp 3900
fall.biserial.sex3900 <-biserial.cor(data.fall.agesex.dist2$mean_imp_cover_3900, 
                                     data.fall.agesex.dist2$sex)
fall.biserial.sex3900

#And using a Pearson's correlation coefficient to test the correlations between each combination of continuous variables
###Latitude and date
fall.pearson.latdate <- cor(data.fall.agesex.dist2$centroid_lat, 
                            data.fall.agesex.dist2$date.ord)
fall.pearson.latdate
###Latitude and imp 1859
fall.pearson.lat1859 <- cor(data.fall.agesex.dist2$centroid_lat, 
                            data.fall.agesex.dist2$mean_imp_cover_1859)
fall.pearson.lat1859
###latitude and imp 3900
fall.pearson.lat3900 <- cor(data.fall.agesex.dist2$centroid_lat, 
                            data.fall.agesex.dist2$mean_imp_cover_3900)
fall.pearson.lat3900
###longitude and date
fall.pearson.londate <- cor(data.fall.agesex.dist2$centroid_lon, 
                            data.fall.agesex.dist2$date.ord)
fall.pearson.londate
###longitude and imp 1859
fall.pearson.lon1859 <- cor(data.fall.agesex.dist2$centroid_lon, 
                            data.fall.agesex.dist2$mean_imp_cover_1859)
fall.pearson.lon1859
###longitude and imp 3900
fall.pearson.lon3900 <- cor(data.fall.agesex.dist2$centroid_lon, 
                            data.fall.agesex.dist2$mean_imp_cover_3900)
fall.pearson.lon3900
###Date and imp 1859
fall.pearson.date1859 <- cor(data.fall.agesex.dist2$mean_imp_cover_1859, 
                             data.fall.agesex.dist2$date.ord)
fall.pearson.date1859
###Date and imp 3900
fall.pearson.date3900 <- cor(data.fall.agesex.dist2$mean_imp_cover_3900, 
                             data.fall.agesex.dist2$date.ord)
fall.pearson.date3900
fall.pearson.latlon <- cor(data.fall.agesex.dist2$centroid_lat, 
                           data.fall.agesex.dist2$centroid_lon)
fall.pearson.latlon
#Using linear regression to test the correlation between the multi-category variables 
#and the continuous ones
fall.yearlat.lm <- lm(centroid_lat~as.factor(year),  
                      data=data.fall.agesex.dist2, weights=w)
summary(fall.yearlat.lm) 


fall.yearlon.lm <- lm(centroid_lon~as.factor(year),  
                      data=data.fall.agesex.dist2, weights=w)
summary(fall.yearlon.lm) 


fall.yeardate.lm <- lm(date.ord~as.factor(year),  
                       data=data.fall.agesex.dist2, weights=w)
summary(fall.yeardate.lm) 


fall.year1859.lm <- lm(mean_imp_cover_1859~as.factor(year), 
                       data=data.fall.agesex.dist2, weights=w)
summary(fall.year1859.lm) 


fall.year3900.lm <- lm(mean_imp_cover_3900~as.factor(year),  
                       data=data.fall.agesex.dist2, weights=w)
summary(fall.year3900.lm) 


fall.BCRdate.lm <- lm(date.ord~as.factor(BCR2),  
                      data=data.fall.agesex.dist2, weights=w)
summary(fall.BCRdate.lm) 


fall.BCR1859.lm <- lm(mean_imp_cover_1859~as.factor(BCR2),  
                      data=data.fall.agesex.dist2, weights=w)
summary(fall.BCR1859.lm) 


fall.BCR3900.lm <- lm(mean_imp_cover_3900~as.factor(BCR2), 
                      data=data.fall.agesex.dist2, weights=w)
summary(fall.BCR3900.lm) 




####Calculating the correlations in the spring step distance dataset####
#categorical variables in the Fall
##Age and sex
spring.chisq.agesex <- chisq.test(data.spring.agesex.dist2$age, 
                                  data.spring.agesex.dist2$sex)
spring.chisq.agesex
##Age and BCR
spring.chisq.ageBCR2<- chisq.test(data.spring.agesex.dist2$age, 
                                  as.factor(data.spring.agesex.dist2$BCR2))
spring.chisq.ageBCR2
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
spring.fisher.ageBCR<- fisher.test(as.factor(data.spring.agesex.dist2$age), 
                                   as.factor(data.spring.agesex.dist2$BCR2))
####Since that didn't fix the error, I tried using a simulated p-value
spring.fisher.ageBCR<- fisher.test(as.factor(data.spring.agesex.dist2$age), 
                                   as.factor(data.spring.agesex.dist2$BCR2),
                                   simulate.p.value=TRUE)
spring.fisher.ageBCR

##Sex and BCR
spring.chisq.sexBCR2<- chisq.test(data.spring.agesex.dist2$sex, 
                                  as.factor(data.spring.agesex.dist2$BCR2))
spring.chisq.sexBCR2
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
spring.fisher.sexBCR<- fisher.test(as.factor(data.spring.agesex.dist2$sex), 
                                   as.factor(data.spring.agesex.dist2$BCR2))
####Since that didn't fix the error, I tried using a simulated p-value
spring.fisher.sexBCR<- fisher.test(as.factor(data.spring.agesex.dist2$sex), 
                                   as.factor(data.spring.agesex.dist2$BCR2),
                                   simulate.p.value=TRUE)
spring.fisher.sexBCR

##Age and year
spring.chisq.ageyear<- chisq.test(data.spring.agesex.dist2$age, 
                                  as.factor(data.spring.agesex.dist2$year))
spring.chisq.ageyear

##Sex and year
spring.chisq.sexyear<- chisq.test(data.spring.agesex.dist2$sex, 
                                  as.factor(data.spring.agesex.dist2$year))
spring.chisq.sexyear

##BCR and year
spring.chisq.BCRyear<- chisq.test(as.factor(data.spring.agesex.dist2$BCR2), 
                                  as.factor(data.spring.agesex.dist2$year))
spring.chisq.BCRyear

table(as.factor(data.fall.agesex.dist2$BCR2), 
      as.factor(data.fall.agesex.dist2$year))
###Trying a Fisher's exact test because the chi square returned a warning 
###that it may not be accurate, likely because of so many degrees of freedom 
###and not a large enough sample size
spring.fisher.BCRyear<- fisher.test(as.factor(data.spring.agesex.dist2$BCR2), 
                                    as.factor(data.spring.agesex.dist2$year))

####Since that didn't fix the error, I tried using a simulated p-value
spring.fisher.BCRyear<- fisher.test(as.factor(data.spring.agesex.dist2$BCR2), 
                                    as.factor(data.spring.agesex.dist2$year),
                                    simulate.p.value=TRUE)
spring.fisher.BCRyear

##And now testing the continuous date, impervious, lat, and lon variables with the binary age and sex variables
#install.packages("ltm")
library(ltm)
###age and lat
spring.biserial.agelat<-biserial.cor(data.spring.agesex.dist2$centroid_lat, 
                                     data.spring.agesex.dist2$age)
spring.biserial.agelat
###age and lon
spring.biserial.agelon<- biserial.cor(data.spring.agesex.dist2$centroid_lon, 
                                      data.spring.agesex.dist2$age)
spring.biserial.agelon 
###sex and lat
spring.biserial.sexlat<-biserial.cor(data.spring.agesex.dist2$centroid_lat, 
                                     data.spring.agesex.dist2$sex)
spring.biserial.sexlat

###sex and lon
spring.biserial.sexlon<-biserial.cor(data.spring.agesex.dist2$centroid_lon, 
                                     data.spring.agesex.dist2$sex)
spring.biserial.sexlon

###Age and date
spring.biserial.agedate <- biserial.cor(data.spring.agesex.dist2$date.ord, 
                                        data.spring.agesex.dist2$age)
spring.biserial.agedate

###Age and imp 1859
spring.biserial.age1859 <-biserial.cor(data.spring.agesex.dist2$mean_imp_cover_1859, 
                                       data.spring.agesex.dist2$age)
spring.biserial.age1859

###Age and impervious 3900
spring.biserial.age3900 <-biserial.cor(data.spring.agesex.dist2$mean_imp_cover_3900, 
                                       data.spring.agesex.dist2$age)
spring.biserial.age3900

###Sex and date
spring.biserial.sexdate <-biserial.cor(data.spring.agesex.dist2$date.ord, 
                                       data.spring.agesex.dist2$sex)
spring.biserial.sexdate

###Sex and imp 1859m
spring.biserial.sex1859 <-biserial.cor(data.spring.agesex.dist2$mean_imp_cover_1859, 
                                       data.spring.agesex.dist2$sex)
spring.biserial.sex1859

###Sex and imp 3900
spring.biserial.sex3900 <-biserial.cor(data.spring.agesex.dist2$mean_imp_cover_3900, 
                                       data.spring.agesex.dist2$sex)
spring.biserial.sex3900

#And using a Pearson's correlation coefficient to test the correlations between each combination of continuous variables
###Latitude and date
spring.pearson.latdate <- cor(data.spring.agesex.dist2$centroid_lat, 
                              data.spring.agesex.dist2$date.ord)
spring.pearson.latdate

###Latitude and imp 1859
spring.pearson.lat1859 <- cor(data.spring.agesex.dist2$centroid_lat, 
                              data.spring.agesex.dist2$mean_imp_cover_1859)
spring.pearson.lat1859

###latitude and imp 3900
spring.pearson.lat3900 <- cor(data.spring.agesex.dist2$centroid_lat, 
                              data.spring.agesex.dist2$mean_imp_cover_3900)
spring.pearson.lat3900

###longitude and date
spring.pearson.londate <- cor(data.spring.agesex.dist2$centroid_lon, 
                              data.spring.agesex.dist2$date.ord)
spring.pearson.londate

###longitude and imp 1859
spring.pearson.lon1859 <- cor(data.spring.agesex.dist2$centroid_lon, 
                              data.spring.agesex.dist2$mean_imp_cover_1859)
spring.pearson.lon1859

###longitude and imp 3900
spring.pearson.lon3900 <- cor(data.spring.agesex.dist2$centroid_lon, 
                              data.spring.agesex.dist2$mean_imp_cover_3900)
spring.pearson.lon3900

###Date and imp 1859
spring.pearson.date1859 <- cor(data.spring.agesex.dist2$mean_imp_cover_1859, 
                               data.spring.agesex.dist2$date.ord)
spring.pearson.date1859

###Date and imp 3900
spring.pearson.date3900 <- cor(data.spring.agesex.dist2$mean_imp_cover_3900, 
                               data.spring.agesex.dist2$date.ord)
spring.pearson.date3900

##lat and lon
spring.pearson.latlon <- cor(data.spring.agesex.dist2$centroid_lat, 
                             data.spring.agesex.dist2$centroid_lon)
spring.pearson.latlon
#Using linear regression to test the correlation between the multi-category variables 
#and the continuous ones
spring.yearlat.lm <- lm(centroid_lat~as.factor(year),  
                        data=data.spring.agesex.dist2, weights=w)
summary(spring.yearlat.lm) 


spring.yearlon.lm <- lm(centroid_lon~as.factor(year),  
                        data=data.spring.agesex.dist2, weights=w)
summary(spring.yearlon.lm) 


spring.yeardate.lm <- lm(date.ord~as.factor(year),  
                         data=data.spring.agesex.dist2, weights=w)
summary(spring.yeardate.lm) 


spring.year1859.lm <- lm(mean_imp_cover_1859~as.factor(year), 
                         data=data.spring.agesex.dist2, weights=w)
summary(spring.year1859.lm) 


spring.year3900.lm <- lm(mean_imp_cover_3900~as.factor(year),  
                         data=data.spring.agesex.dist2, weights=w)
summary(spring.year3900.lm) 


spring.BCRdate.lm <- lm(date.ord~as.factor(BCR2),  
                        data=data.spring.agesex.dist2, weights=w)
summary(spring.BCRdate.lm) 


spring.BCR1859.lm <- lm(mean_imp_cover_1859~as.factor(BCR2),  
                        data=data.spring.agesex.dist2, weights=w)
summary(spring.BCR1859.lm) 


spring.BCR3900.lm <- lm(mean_imp_cover_3900~as.factor(BCR2), 
                        data=data.spring.agesex.dist2, weights=w)
summary(spring.BCR3900.lm) 


###Model selection for Fall migratory step distance ######
#install.packages("AICcmodavg")
library(AICcmodavg)
View(data.fall.agesex.dist2)
#Running models for the effects of each combination of individual variables (age and sex)
#on stop type

fall.betweenstop.age.glm <-glm(as.numeric(step_distkm) ~ as.factor(age), family=gaussian, 
                               data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.age.glm)


fall.betweenstop.sex.glm<-glm(as.numeric(step_distkm) ~ as.factor(sex), family=gaussian, 
                              data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.sex.glm)


fall.betweenstop.ageplussex.glm <-glm(as.numeric(step_distkm)~ as.factor(age)+as.factor(sex), family=gaussian, 
                                      data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.ageplussex.glm)


fall.betweenstop.agexsex.glm <- glm(as.numeric(step_distkm) ~ as.factor(age)*as.factor(sex), family=gaussian,
                                    data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.agexsex.glm)


fall.betweenstop.null.glm <- glm(as.numeric(step_distkm) ~ 1, family=gaussian, 
                                 data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.null.glm)

#AICc model selection on the above models
cand.set.fall.betweenstop.1 <- list(fall.betweenstop.age.glm, fall.betweenstop.sex.glm, fall.betweenstop.ageplussex.glm, 
                                    fall.betweenstop.agexsex.glm, fall.betweenstop.null.glm)
modnames.fall.betweenstop.1 <- c("fall.betweenstop.age.glm", "fall.betweenstop.sex.glm", 
                                 "fall.betweenstop.ageplussex.glm", "fall.betweenstop.agexsex.glm", 
                                 "fall.betweenstop.null.glm")
aictab(cand.set=cand.set.fall.betweenstop.1, modnames = modnames.fall.betweenstop.1,  
       digits = 2, second.ord = T)
##Because the null model was the most supported(deltaAICc=0), I won't be including 
##age or sex in the rest of my stopover duration analysis 

#Running models on the effects of temporal variables (year and date) on duration
#using the most supported individual effects model
#in this case, the null model

fall.betweenstop.year.glm <-glm(as.numeric(step_distkm) ~ as.factor(year), family=gaussian, 
                                data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.year.glm)


fall.betweenstop.date.glm <-glm(as.numeric(step_distkm) ~ date.ord, family=gaussian, 
                                data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.date.glm)


fall.betweenstop.yearplusdate.glm <-glm(as.numeric(step_distkm) ~ as.factor(year)+date.ord, 
                                        family=gaussian, 
                                        data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.yearplusdate.glm)
#Model selection on the above models

cand.set.fall.betweenstop.2 <- list(fall.betweenstop.year.glm, fall.betweenstop.date.glm, 
                                    fall.betweenstop.yearplusdate.glm, fall.betweenstop.null.glm)
modnames.fall.betweenstop.2 <- c("fall.betweenstop.year.glm", "fall.betweenstop.date.glm",
                                 "fall,betweenstop.yearplusdate", "fall.betweenstop.null.glm")
aictab(cand.set=cand.set.fall.betweenstop.2, modnames = modnames.fall.betweenstop.2,  
       digits = 2, second.ord = T)
#Because none of the other models included significant effects, I'm still using the null

#Running models on the spatial variables (lat, lon, and BCR) using the most supported
#temporal and individual model
#But not including BCR with lat and lon in the same model

fall.betweenstop.lat.glm <-glm(as.numeric(step_distkm) ~ centroid_lat, family=gaussian, 
                               data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.lat.glm)


fall.betweenstop.lon.glm <-glm(as.numeric(step_distkm) ~ centroid_lon, family=gaussian, 
                               data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.lon.glm)


fall.betweenstop.latpluslon.glm <-glm(as.numeric(step_distkm) ~ centroid_lat+centroid_lon, family=gaussian, 
                                      data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.latpluslon.glm)


fall.betweenstop.BCR.glm <-glm(as.numeric(step_distkm) ~ as.factor(BCR2), family=gaussian, 
                               data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.BCR.glm)


#Model selection on the above models
cand.set.fall.betweenstop.3 <- list(fall.betweenstop.lat.glm, fall.betweenstop.lon.glm, 
                                    fall.betweenstop.latpluslon.glm,
                                    fall.betweenstop.BCR.glm, fall.betweenstop.null.glm)
modnames.fall.betweenstop.3 <- c("fall.betweenstop.lat.glm", "fall.betweenstop.lon.glm", 
                                 "fall.betweenstop.latpluslon.glm",
                                 "fall.betweenstop.BCR.glm", "fall.betweenstop.null.glm")
aictab(cand.set=cand.set.fall.betweenstop.3, modnames = modnames.fall.betweenstop.3,  
       digits = 2, second.ord = T)


##Latitude is most supported
#Testing the most supported model with both mean impervious radii

fall.betweenstop.imp1859.glm <- glm(as.numeric(step_distkm) ~ mean_imp_cover_1859+centroid_lat, 
                                    family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859.glm)


fall.betweenstop.imp3900.glm <- glm(as.numeric(step_distkm) ~ mean_imp_cover_3900+centroid_lat, 
                                    family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900.glm)



fall.betweenstop.imp1859only.glm <- glm(as.numeric(step_distkm) ~ mean_imp_cover_1859, 
                                        family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859only.glm)


fall.betweenstop.imp3900only.glm <- glm(as.numeric(step_distkm) ~ mean_imp_cover_3900, 
                                        family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900only.glm)


#Model selection on the above models
cand.set.fall.betweenstop.4 <- list(fall.betweenstop.imp1859.glm, fall.betweenstop.imp3900.glm,
                                    fall.betweenstop.imp1859only.glm, fall.betweenstop.imp3900only.glm,
                                    fall.betweenstop.lat.glm)
modnames.fall.betweenstop.4 <- c("fall.betweenstop.imp1859.glm", "fall.betweenstop.imp3900.glm",
                                 "fall.betweenstop.imp1859only.glm", "fall.betweenstop.imp3900only.glm", 
                                 "fall.betweenstop.lat.glm")
aictab(cand.set=cand.set.fall.betweenstop.4, modnames = modnames.fall.betweenstop.4,  
       digits = 2, second.ord = T)


#Model selection with impervious surfaces and interactions

##age
fall.betweenstop.imp1859xage.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                         mean_imp_cover_1859*as.factor(age),
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859xage.glm)

##sex
fall.betweenstop.imp1859xsex.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                         mean_imp_cover_1859*as.factor(sex),
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859xsex.glm)

##year
fall.betweenstop.imp1859xyear.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                          mean_imp_cover_1859*as.factor(year),
                                        family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859xyear.glm)

##date
fall.betweenstop.imp1859xdate.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                          mean_imp_cover_1859*date.ord,
                                        family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.impx1859date.glm)

##lat
fall.betweenstop.imp1859xlat.glm<- glm(as.numeric(step_distkm) ~ centroid_lat + 
                                         mean_imp_cover_1859*centroid_lat,
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859xlat.glm)


##lon
fall.betweenstop.imp1859xlon.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                         mean_imp_cover_1859*centroid_lon,
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859xlon.glm)

#not including lat
fall.betweenstop.imp1859xBCR.glm<- glm(as.numeric(step_distkm) ~ mean_imp_cover_1859*as.factor(BCR2),
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859xBCR.glm)
#and for the larger radius as well

##age
fall.betweenstop.imp3900xage.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                         mean_imp_cover_3900*as.factor(age),
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900xage.glm)

##sex
fall.betweenstop.imp3900xsex.glm<- glm(as.numeric(step_distkm) ~ centroid_lat + 
                                         mean_imp_cover_3900*as.factor(sex),
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900xsex.glm)

##year
fall.betweenstop.imp3900xyear.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                          mean_imp_cover_3900*as.factor(year),
                                        family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900xyear.glm)

##date
fall.betweenstop.imp3900xdate.glm<- glm(as.numeric(step_distkm) ~ centroid_lat + 
                                          mean_imp_cover_3900*date.ord,
                                        family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900xdate.glm)

##lat
fall.betweenstop.imp3900xlat.glm<- glm(as.numeric(step_distkm) ~ centroid_lat + 
                                         mean_imp_cover_3900*centroid_lat,
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900xlat.glm)

##lon
fall.betweenstop.imp3900xlon.glm<- glm(as.numeric(step_distkm) ~ centroid_lat +
                                         mean_imp_cover_3900*centroid_lon,
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900xlon.glm)

#not including lat
fall.betweenstop.imp3900xBCR.glm<- glm(as.numeric(step_distkm) ~ mean_imp_cover_3900*as.factor(BCR2),
                                       family=gaussian, data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900xBCR.glm)


#Model selection on the above models
cand.set.fall.betweenstop.5 <- list(fall.betweenstop.imp1859xage.glm, fall.betweenstop.imp1859xsex.glm, 
                                    fall.betweenstop.imp1859xyear.glm, fall.betweenstop.imp1859xdate.glm,
                                    fall.betweenstop.imp1859xlat.glm, fall.betweenstop.imp1859xlon.glm,
                                    fall.betweenstop.imp1859xBCR.glm, fall.betweenstop.imp3900xage.glm, 
                                    fall.betweenstop.imp3900xsex.glm, fall.betweenstop.imp3900xyear.glm, 
                                    fall.betweenstop.imp3900xdate.glm, fall.betweenstop.imp3900xlat.glm, 
                                    fall.betweenstop.imp3900xlon.glm, fall.betweenstop.imp3900xBCR.glm,
                                    fall.betweenstop.imp1859.glm, fall.betweenstop.imp3900.glm, 
                                    fall.betweenstop.imp1859only.glm, fall.betweenstop.imp3900only.glm,
                                    fall.betweenstop.lat.glm)
modnames.fall.betweenstop.5 <- c("fall.betweenstop.imp1859xage.glm", "fall.betweenstop.imp1859xsex.glm", 
                                 "fall.betweenstop.imp1859xyear.glm", "fall.betweenstop.imp1859xdate.glm",
                                 "fall.betweenstop.imp1859xlat.glm", "fall.betweenstop.imp1859xlon.glm",
                                 "fall.betweenstop.imp1859xBCR.glm", "fall.betweenstop.imp3900xage.glm", 
                                 "fall.betweenstop.imp3900xsex.glm", "fall.betweenstop.imp3900xyear.glm", 
                                 "fall.betweenstop.imp3900xdate.glm","fall.betweenstop.imp3900xlat.glm", 
                                 "fall.betweenstop.imp3900xlon.glm","fall.betweenstop.imp3900xBCR.glm",
                                 "fall.betweenstop.imp1859.glm", "fall.betweenstop.imp3900.glm",
                                 "fall.betweenstop.imp1859only.glm", "fall.betweenstop.imp3900only.glm",
                                 "fall.betweenstop.lat.glm")
aictab(cand.set=cand.set.fall.betweenstop.5, modnames = modnames.fall.betweenstop.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.fall.betweenstop.6 <- list(fall.betweenstop.age.glm, fall.betweenstop.sex.glm, 
                                    fall.betweenstop.ageplussex.glm, fall.betweenstop.agexsex.glm,
                                    fall.betweenstop.null.glm, fall.betweenstop.year.glm, 
                                    fall.betweenstop.date.glm, fall.betweenstop.yearplusdate.glm, 
                                    fall.betweenstop.lat.glm, fall.betweenstop.lon.glm,
                                    fall.betweenstop.latpluslon.glm, fall.betweenstop.BCR.glm,
                                    fall.betweenstop.imp1859xage.glm, fall.betweenstop.imp1859xsex.glm, 
                                    fall.betweenstop.imp1859xyear.glm, fall.betweenstop.imp1859xdate.glm,
                                    fall.betweenstop.imp1859xlat.glm, fall.betweenstop.imp1859xlon.glm,
                                    fall.betweenstop.imp1859xBCR.glm,fall.betweenstop.imp3900xage.glm, 
                                    fall.betweenstop.imp3900xsex.glm, fall.betweenstop.imp3900xyear.glm, 
                                    fall.betweenstop.imp3900xdate.glm, fall.betweenstop.imp3900xlat.glm, 
                                    fall.betweenstop.imp3900xlon.glm, fall.betweenstop.imp3900xBCR.glm,
                                    fall.betweenstop.imp1859.glm, fall.betweenstop.imp3900.glm,
                                    fall.betweenstop.imp1859only.glm, fall.betweenstop.imp3900only.glm)
modnames.fall.betweenstop.6 <- c("Age", "Sex",
                                 "Age + Sex", "Age*Sex",
                                 "Null", "Year",
                                 "Date", "Year + Date",
                                 "Lat", "Lon",
                                 "Lat + Lon", "BCR",
                                 "Lat + Imp1.859*age", "Lat + Imp1859*Sex", 
                                 "Lat + Imp1.859*Year", "Lat + Imp1.859*Date",
                                 "Lat + Imp1.859*Lat", "Lat + Imp1.859*Lon",
                                 "Imp1.859*BCR", "Lat + Imp3.900*Age", 
                                 "Lat + Imp3.900*Sex", "Lat + Imp3.900*Year", 
                                 "Lat + Imp3.900*Date","Lat + Imp3.900*Lat", 
                                 "Lat + Imp3.900*Lon","Imp3.900*BCR",
                                 "Lat + Imp1.859", "Lat + Imp3.900",
                                 "Imp1.859", "Imp3.900")
FallBetweenStopFullAIC<- aictab(cand.set=cand.set.fall.betweenstop.6, modnames = modnames.fall.betweenstop.6,  
                                digits = 2, second.ord = T)
FallBetweenStopFullAIC
#write.csv(FallBetweenStopFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/FallBetweenStop2FullAIC.csv")

#Running supported models as lm to generate R squared
fall.betweenstop.imp1859.lm <- lm(as.numeric(step_distkm) ~ mean_imp_cover_1859+centroid_lat, 
                                   data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859.lm)


fall.betweenstop.lat.lm <-lm(as.numeric(step_distkm) ~ centroid_lat,  
                               data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.lat.lm)


fall.betweenstop.imp3900.lm <- lm(as.numeric(step_distkm) ~ mean_imp_cover_3900+centroid_lat, 
                                     data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp3900.lm)


fall.betweenstop.imp1859xlat.lm<- lm(as.numeric(step_distkm) ~ centroid_lat + 
                                         mean_imp_cover_1859*centroid_lat,
                                        data=data.fall.agesex.dist2, weights=w)
summary(fall.betweenstop.imp1859xlat.lm)

###Model selection for Spring migratory step distance######################################################
#install.packages("AICcmodavg")
library(AICcmodavg)

#Running models for the effects of each combination of individual variables (age and sex)
#on stop type

spring.betweenstop.age.glm <-glm(as.numeric(step_distkm) ~ as.factor(age), family=gaussian, 
                                 data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.age.glm)


spring.betweenstop.sex.glm<-glm(as.numeric(step_distkm) ~ as.factor(sex), family=gaussian, 
                                data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.sex.glm)


spring.betweenstop.ageplussex.glm <-glm(as.numeric(step_distkm)~ as.factor(age)+as.factor(sex), 
                                        family=gaussian, 
                                        data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.ageplussex.glm)


spring.betweenstop.agexsex.glm <- glm(as.numeric(step_distkm) ~ as.factor(age)*as.factor(sex), 
                                      family=gaussian,
                                      data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.agexsex.glm)


spring.betweenstop.null.glm <- glm(as.numeric(step_distkm) ~ 1, family=gaussian, 
                                   data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.null.glm)

#AICc model selection on the above models
cand.set.spring.betweenstop.1 <- list(spring.betweenstop.age.glm, spring.betweenstop.sex.glm, 
                                      spring.betweenstop.ageplussex.glm, 
                                      spring.betweenstop.agexsex.glm, spring.betweenstop.null.glm)
modnames.spring.betweenstop.1 <- c("spring.betweenstop.age.glm", "spring.betweenstop.sex.glm", 
                                   "spring.betweenstop.ageplussex.glm", "spring.betweenstop.agexsex.glm", 
                                   "spring.betweenstop.null.glm")
aictab(cand.set=cand.set.spring.betweenstop.1, modnames = modnames.spring.betweenstop.1,  
       digits = 2, second.ord = T)
##The sex model was most supported but non-significant
##I'm using the agexsex model even though the interactive effect wasn't significant because 
#sex had a significant impact in that one and not individually

#Running models on the effects of temporal variables (year and date) on duration
#using the most supported individual effects model
#in this case, the agexsex model
#Age and sex are both correlated with year those variables won't be in the year or year+date models

spring.betweenstop.year.glm <-glm(as.numeric(step_distkm) ~ as.factor(year), family=gaussian, 
                                  data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.year.glm)


spring.betweenstop.date.glm <-glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                    date.ord, family=gaussian, 
                                  data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.date.glm)


spring.betweenstop.yearplusdate.glm <-glm(as.numeric(step_distkm) ~ as.factor(year)+date.ord, 
                                          family=gaussian, 
                                          data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.yearplusdate.glm)

#Model selection on the above models

cand.set.spring.betweenstop.2 <- list(spring.betweenstop.year.glm, spring.betweenstop.date.glm, 
                                      spring.betweenstop.yearplusdate.glm, spring.betweenstop.agexsex.glm)
modnames.spring.betweenstop.2 <- c("spring.betweenstop.year.glm", "spring.betweenstop.date.glm",
                                   "spring,betweenstop.yearplusdate.glm", "spring.betweenstop.agexsex.glm")
aictab(cand.set=cand.set.spring.betweenstop.2, modnames = modnames.spring.betweenstop.2,  
       digits = 2, second.ord = T)
##Year+date was the most supported model but the year effect was non-significant 
##So I'm moving forward with just date

#Running models on the spatial variables (lat, lon, and BCR) using the most supported
#temporal and individual model
#But not including BCR with lat, lon, age, or sex in the same model

spring.betweenstop.lat.glm <-glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                   date.ord+centroid_lat, family=gaussian, 
                                 data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.lat.glm)


spring.betweenstop.lon.glm <-glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                   date.ord+centroid_lon, family=gaussian, 
                                 data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.lon.glm)


spring.betweenstop.latpluslon.glm <-glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                          date.ord+centroid_lat+centroid_lon, family=gaussian, 
                                        data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.latpluslon.glm)


spring.betweenstop.BCR.glm <-glm(as.numeric(step_distkm) ~ date.ord+as.factor(BCR2), family=gaussian, 
                                 data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.BCR.glm)


#Model selection on the above models
cand.set.spring.betweenstop.3 <- list(spring.betweenstop.lat.glm, spring.betweenstop.lon.glm, 
                                      spring.betweenstop.latpluslon.glm,
                                      spring.betweenstop.BCR.glm, spring.betweenstop.date.glm)
modnames.spring.betweenstop.3 <- c("spring.betweenstop.lat.glm", "spring.betweenstop.lon.glm", 
                                   "spring.betweenstop.latpluslon.glm",
                                   "spring.betweenstop.BCR.glm", "spring.betweenstop.date.glm")
aictab(cand.set=cand.set.spring.betweenstop.3, modnames = modnames.spring.betweenstop.3,  
       digits = 2, second.ord = T)


##Lat + lon was the most supported, but the lon effect was not significant
#So I'm using the lat model
#Testing the most supported model with both mean impervious radii

spring.betweenstop.imp1859.glm <- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                        date.ord+centroid_lat+mean_imp_cover_1859, 
                                      family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859.glm)


spring.betweenstop.imp3900.glm <- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                        date.ord+centroid_lat+mean_imp_cover_3900, 
                                      family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900.glm)



spring.betweenstop.imp1859only.glm <- glm(as.numeric(step_distkm) ~ mean_imp_cover_1859, 
                                          family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859.glm)


spring.betweenstop.imp3900only.glm <- glm(as.numeric(step_distkm) ~ mean_imp_cover_3900, 
                                          family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900.glm)


#Model selection on the above models
cand.set.spring.betweenstop.4 <- list(spring.betweenstop.imp1859.glm, spring.betweenstop.imp3900.glm,
                                      spring.betweenstop.imp1859only.glm, spring.betweenstop.imp3900only.glm,
                                      spring.betweenstop.lat.glm)
modnames.spring.betweenstop.4 <- c("spring.betweenstop.imp1859.glm", "spring.betweenstop.imp3900.glm",
                                   "spring.betweenstop.imp1859only.glm", "spring.betweenstop.imp3900only.glm", 
                                   "spring.betweenstop.lat.glm")
aictab(cand.set=cand.set.spring.betweenstop.4, modnames = modnames.spring.betweenstop.4,  
       digits = 2, second.ord = T)
#lat still most supported

#Model selection with impervious surfaces and interactions
#Not including age or sex with BCR or year

##age
spring.betweenstop.imp1859xage.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord+centroid_lat +
                                           mean_imp_cover_1859*as.factor(age),
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859xage.glm)

##sex
spring.betweenstop.imp1859xsex.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord+centroid_lat +
                                           mean_imp_cover_1859*as.factor(sex),
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859xsex.glm)

##year
spring.betweenstop.imp1859xyear.glm<- glm(as.numeric(step_distkm) ~ centroid_lat + date.ord +
                                            mean_imp_cover_1859*as.factor(year),
                                          family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859xyear.glm)

##date
spring.betweenstop.imp1859xdate.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                            date.ord + centroid_lat +
                                            mean_imp_cover_1859*date.ord,
                                          family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.impx1859date.glm)

##lat
spring.betweenstop.imp1859xlat.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord + centroid_lat + 
                                           mean_imp_cover_1859*centroid_lat,
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859xlat.glm)


##lon
spring.betweenstop.imp1859xlon.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord + centroid_lat +
                                           mean_imp_cover_1859*centroid_lon,
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859xlon.glm)


spring.betweenstop.imp1859xBCR.glm<- glm(as.numeric(step_distkm) ~ date.ord +
                                           mean_imp_cover_1859*as.factor(BCR2),
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp1859xBCR.glm)
#and for the larger radius as well

##age
spring.betweenstop.imp3900xage.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord + centroid_lat +
                                           mean_imp_cover_3900*as.factor(age),
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xage.glm)

##sex
spring.betweenstop.imp3900xsex.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord + centroid_lat + 
                                           mean_imp_cover_3900*as.factor(sex),
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xsex.glm)

##year
spring.betweenstop.imp3900xyear.glm<- glm(as.numeric(step_distkm) ~ centroid_lat + date.ord +
                                            mean_imp_cover_3900*as.factor(year),
                                          family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xyear.glm)

##date
spring.betweenstop.imp3900xdate.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                            date.ord + centroid_lat + 
                                            mean_imp_cover_3900*date.ord,
                                          family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xdate.glm)

##lat
spring.betweenstop.imp3900xlat.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord+centroid_lat + 
                                           mean_imp_cover_3900*centroid_lat,
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xlat.glm)

##lon
spring.betweenstop.imp3900xlon.glm<- glm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord + centroid_lat +
                                           mean_imp_cover_3900*centroid_lon,
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xlon.glm)


spring.betweenstop.imp3900xBCR.glm<- glm(as.numeric(step_distkm) ~ date.ord + 
                                           mean_imp_cover_3900*as.factor(BCR2),
                                         family=gaussian, data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xBCR.glm)


#Model selection on the above models
cand.set.spring.betweenstop.5 <- list(spring.betweenstop.imp1859xage.glm, spring.betweenstop.imp1859xsex.glm, 
                                      spring.betweenstop.imp1859xyear.glm, spring.betweenstop.imp1859xdate.glm,
                                      spring.betweenstop.imp1859xlat.glm, spring.betweenstop.imp1859xlon.glm,
                                      spring.betweenstop.imp1859xBCR.glm, spring.betweenstop.imp3900xage.glm, 
                                      spring.betweenstop.imp3900xsex.glm, spring.betweenstop.imp3900xyear.glm, 
                                      spring.betweenstop.imp3900xdate.glm, spring.betweenstop.imp3900xlat.glm, 
                                      spring.betweenstop.imp3900xlon.glm, spring.betweenstop.imp3900xBCR.glm,
                                      spring.betweenstop.imp1859.glm, spring.betweenstop.imp3900.glm, 
                                      spring.betweenstop.imp1859only.glm, spring.betweenstop.imp3900only.glm,
                                      spring.betweenstop.lat.glm)
modnames.spring.betweenstop.5 <- c("spring.betweenstop.imp1859xage.glm", "spring.betweenstop.imp1859xsex.glm", 
                                   "spring.betweenstop.imp1859xyear.glm", "spring.betweenstop.imp1859xdate.glm",
                                   "spring.betweenstop.imp1859xlat.glm", "spring.betweenstop.imp1859xlon.glm",
                                   "spring.betweenstop.imp1859xBCR.glm", "spring.betweenstop.imp3900xage.glm", 
                                   "spring.betweenstop.imp3900xsex.glm", "spring.betweenstop.imp3900xyear.glm", 
                                   "spring.betweenstop.imp3900xdate.glm","spring.betweenstop.imp3900xlat.glm", 
                                   "spring.betweenstop.imp3900xlon.glm", "spring.betweenstop.imp3900xBCR.glm",
                                   "spring.betweenstop.imp1859.glm", "spring.betweenstop.imp3900.glm",
                                   "spring.betweenstop.imp1859only.glm", "spring.betweenstop.imp3900only.glm",
                                   "spring.betweenstop.lat.glm")
aictab(cand.set=cand.set.spring.betweenstop.5, modnames = modnames.spring.betweenstop.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.spring.betweenstop.6 <- list(spring.betweenstop.age.glm, spring.betweenstop.sex.glm, 
                                      spring.betweenstop.ageplussex.glm, spring.betweenstop.agexsex.glm,
                                      spring.betweenstop.null.glm, spring.betweenstop.year.glm, 
                                      spring.betweenstop.date.glm, spring.betweenstop.yearplusdate.glm, 
                                      spring.betweenstop.lat.glm, spring.betweenstop.lon.glm,
                                      spring.betweenstop.latpluslon.glm, spring.betweenstop.BCR.glm,
                                      spring.betweenstop.imp1859xage.glm, spring.betweenstop.imp1859xsex.glm, 
                                      spring.betweenstop.imp1859xyear.glm, spring.betweenstop.imp1859xdate.glm,
                                      spring.betweenstop.imp1859xlat.glm, spring.betweenstop.imp1859xlon.glm,
                                      spring.betweenstop.imp1859xBCR.glm, spring.betweenstop.imp3900xage.glm, 
                                      spring.betweenstop.imp3900xsex.glm, spring.betweenstop.imp3900xyear.glm, 
                                      spring.betweenstop.imp3900xdate.glm, spring.betweenstop.imp3900xlat.glm, 
                                      spring.betweenstop.imp3900xlon.glm, spring.betweenstop.imp3900xBCR.glm,
                                      spring.betweenstop.imp1859.glm, spring.betweenstop.imp3900.glm,
                                      spring.betweenstop.imp1859only.glm, spring.betweenstop.imp3900only.glm)
modnames.spring.betweenstop.6 <- c("Age", "Sex",
                                   "Age + Sex", "Age*Sex",
                                   "Null", "Year",
                                   "Age*Sex + date", "Year + Date",
                                   "Age*Sex + Date + Lat", "Age*Sex + Date + Lon",
                                   "Age*Sex + Date + Lat + Lon", "Date + BCR",
                                   "Age*Sex + Date + Lat + Imp1.859*Age", "Age*Sex + Date + Lat + Imp1.859*Sex", 
                                   "Date + Lat + Imp1.859*Year", "Age*Sex + Date + Lat + Imp1.859*Date",
                                   "Age*Sex + Date + Lat + Imp1.859*Lat", "Age*Sex + Date + Lat + Imp1.859*Lon",
                                   "Date + Imp1.859*BCR", "Age*Sex + Date + Lat + Imp3.900*Age", 
                                   "Age*Sex + Date + Lat + Imp3.900*Sex", "Date + Lat + Imp3.900*Year", 
                                   "Age*Sex + Date + Lat + Imp3.900*Date","Age*Sex + Date + Lat + Imp3.900*Lat", 
                                   "Age*Sex + Date + Lat + Imp3.900*Lon","Date + Imp3.900*BCR",
                                   "Age*Sex + Date + Lat + Imp1.859", "Age*Sex + Date + Lat + Imp3.900",
                                   "Imp1.859", "Imp3.900")
SpringBetweenStopFullAIC<- aictab(cand.set=cand.set.spring.betweenstop.6, modnames = modnames.spring.betweenstop.6,  
                                  digits = 2, second.ord = T)
SpringBetweenStopFullAIC
write.csv(SpringBetweenStopFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/SpringBetweenStop2FullAICSupp.csv")

#Running supported models as lm to generate R squared
spring.betweenstop.latpluslon.lm <-lm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                          date.ord+centroid_lat+centroid_lon,  
                                        data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.latpluslon.lm)


spring.betweenstop.lat.lm <-lm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                   date.ord+centroid_lat, 
                                 data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.lat.lm)


spring.betweenstop.imp3900xlon.lm<- lm(as.numeric(step_distkm) ~ (as.factor(age)*as.factor(sex))+
                                           date.ord + centroid_lat +
                                           mean_imp_cover_3900*centroid_lon,
                                          data=data.spring.agesex.dist2, weights=w)
summary(spring.betweenstop.imp3900xlon.lm)
##################################################################

###Model selection for stop type in the Fall #################################
#install.packages("AICcmodavg")
library(AICcmodavg)
#converting the stop type field into numbers, 0 for stop and 1 for stopover
data.fall.agesex.2$stop <- "NA"
View(data.fall.agesex.2)

for (i in 1:nrow(data.fall.agesex.2)){
  
  data.fall.agesex.2$stop[i]<- ifelse(test=data.fall.agesex.2$stop_type[i]=="stop", yes="0",
                                      no="1")
  
}
View(data.fall.agesex.2)
#Running models for the effects of each combination of individual variables (age and sex)
#on stop type

fall.stoptype.age.glm <-glm(as.factor(stop) ~ as.factor(age), family=binomial, 
                            data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.age.glm)


fall.stoptype.sex.glm<-glm(as.factor(stop) ~ as.factor(sex), family=binomial, 
                           data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.sex.glm)


fall.stoptype.ageplussex.glm <-glm(as.factor(stop)~ as.factor(age)+as.factor(sex), family=binomial, 
                                   data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.ageplussex.glm)


fall.stoptype.agexsex.glm <- glm(as.factor(stop) ~ as.factor(age)*as.factor(sex), family=binomial, 
                                 data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.agexsex.glm)


fall.stoptype.null.glm <- glm(as.factor(stop) ~ 1, family=binomial, 
                              data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.null.glm)

#AICc model selection on the above models
cand.set.fall.stoptype.1 <- list(fall.stoptype.age.glm, fall.stoptype.sex.glm, fall.stoptype.ageplussex.glm, 
                                 fall.stoptype.agexsex.glm, fall.stoptype.null.glm)
modnames.fall.stoptype.1 <- c("fall.stoptype.age.glm", "fall.stoptype.sex.glm", 
                              "fall.stoptype.ageplussex.glm", "fall.stoptype.agexsex.glm", 
                              "fall.stoptype.null.glm")
aictab(cand.set=cand.set.fall.stoptype.1, modnames = modnames.fall.stoptype.1,  
       digits = 2, second.ord = T)
##Because the null model was the most supported(deltaAICc=0), I won't be including 
##age or sex in the rest of my stopover duration analysis 

#Running models on the effects of temporal variables (year and date) on duration
#using the most supported individual effects model
#in this case, the null model

fall.stoptype.year.glm <-glm(as.factor(stop) ~ as.factor(year), family=binomial, 
                             data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.year.glm)

fall.stoptype.date.glm <-glm(as.factor(stop) ~ date.ord, family=binomial, 
                             data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.date.glm)

fall.stoptype.yearplusdate.glm<-glm(as.factor(stop) ~ as.factor(year)+date.ord, family=binomial, 
                                    data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.yearplusdate.glm)


#Model selection on the above models

cand.set.fall.stoptype.2 <- list(fall.stoptype.year.glm, fall.stoptype.date.glm, 
                                 fall.stoptype.yearplusdate.glm, fall.stoptype.null.glm)
modnames.fall.stoptype.2 <- c("fall.stoptype.year.glm", "fall.stoptype.date.glm",
                              "fall.stoptype.yearplusdate.glm", "fall.stoptype.null.glm")
aictab(cand.set=cand.set.fall.stoptype.2, modnames = modnames.fall.stoptype.2,  
       digits = 2, second.ord = T)
##Null was still the most supported model

#Running models on the spatial variables (lat, lon, and BCR) using the most supported
#temporal and individual model
#But not including BCR with lat and lon in the same model, or lat and lon together 
#because of correlations

fall.stoptype.lat.glm <-glm(as.factor(stop) ~ centroid_lat, family=binomial, 
                            data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.lat.glm)


fall.stoptype.lon.glm <-glm(as.factor(stop) ~ centroid_lon, family=binomial, 
                            data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.lon.glm)


fall.stoptype.BCR.glm <-glm(as.factor(stop) ~ as.factor(BCR2), family=binomial, 
                            data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.BCR.glm)
#not doing one with both lat and lon because they're correlated

#Model selection on the above models
cand.set.fall.stoptype.3 <- list(fall.stoptype.lat.glm, fall.stoptype.lon.glm, 
                                 fall.stoptype.BCR.glm, fall.stoptype.null.glm)
modnames.fall.stoptype.3 <- c("fall.stoptype.lat.glm", "fall.stoptype.lon.glm", 
                              "fall.stoptype.BCR.glm", "fall.stoptype.null.glm")
aictab(cand.set=cand.set.fall.stoptype.3, modnames = modnames.fall.stoptype.3,  
       digits = 2, second.ord = T)

##The null model was still most supported
#Testing the most supported model with both mean impervious radii

fall.stoptype.imp1859.glm <- glm(as.factor(stop) ~ mean_imp_cover_1859, 
                                 family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp1859.glm)

fall.stoptype.imp3900.glm <- glm(as.factor(stop) ~ mean_imp_cover_3900, 
                                 family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900.glm)


#Model selection on the above models
cand.set.fall.stoptype.4 <- list(fall.stoptype.imp1859.glm, fall.stoptype.imp3900.glm,
                                 fall.stoptype.null.glm)
modnames.fall.stoptype.4 <- c("fall.stoptype.imp1859.glm", "fall.stoptype.imp3900.glm", 
                              "fall.stoptype.null.glm")
aictab(cand.set=cand.set.fall.stoptype.4, modnames = modnames.fall.stoptype.4,  
       digits = 2, second.ord = T)


#Model selection with impervious surfaces and interactions

fall.stoptype.imp1859xage.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(age),
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp1859xage.glm)


fall.stoptype.imp1859xsex.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(sex),
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp1859xsex.glm)


fall.stoptype.imp1859xyear.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(year),
                                     family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp1859xyear.glm)


fall.stoptype.imp1859xdate.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*date.ord,
                                     family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.impx1859date.glm)


fall.stoptype.imp1859xlat.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*centroid_lat,
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp1859xlat.glm)


fall.stoptype.imp1859xlon.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*centroid_lon,
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp1859xlon.glm)


fall.stoptype.imp1859xBCR.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(BCR2),
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp1859xBCR.glm)
#and for the larger radius as well

fall.stoptype.imp3900xage.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(age),
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900xage.glm)


fall.stoptype.imp3900xsex.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(sex),
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900xsex.glm)


fall.stoptype.imp3900xyear.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(year),
                                     family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900xyear.glm)


fall.stoptype.imp3900xdate.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*date.ord,
                                     family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900xdate.glm)


fall.stoptype.imp3900xlat.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*centroid_lat,
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900xlat.glm)


fall.stoptype.imp3900xlon.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*centroid_lon,
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900xlon.glm)


fall.stoptype.imp3900xBCR.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(BCR2),
                                    family=binomial, data=data.fall.agesex.2, weights=w)
summary(fall.stoptype.imp3900xBCR.glm)


#Model selection on the above models
cand.set.fall.stoptype.5 <- list(fall.stoptype.imp1859xage.glm, fall.stoptype.imp1859xsex.glm, 
                                 fall.stoptype.imp1859xyear.glm, fall.stoptype.imp1859xdate.glm,
                                 fall.stoptype.imp1859xlat.glm, fall.stoptype.imp1859xlon.glm,
                                 fall.stoptype.imp1859xBCR.glm,fall.stoptype.imp3900xage.glm, 
                                 fall.stoptype.imp3900xsex.glm, fall.stoptype.imp3900xyear.glm, 
                                 fall.stoptype.imp3900xdate.glm, fall.stoptype.imp3900xlat.glm, 
                                 fall.stoptype.imp3900xlon.glm, fall.stoptype.imp3900xBCR.glm,
                                 fall.stoptype.imp1859.glm, fall.stoptype.imp3900.glm,
                                 fall.stoptype.null.glm)
modnames.fall.stoptype.5 <- c("fall.stoptype.imp1859xage.glm", "fall.stoptype.imp1859xsex.glm", 
                              "fall.stoptype.imp1859xyear.glm", "fall.stoptype.imp1859xdate.glm",
                              "fall.stoptype.imp1859xlat.glm", "fall.stoptype.imp1859xlon.glm",
                              "fall.stoptype.imp1859xBCR.glm", "fall.stoptype.imp3900xage.glm", 
                              "fall.stoptype.imp3900xsex.glm", "fall.stoptype.imp3900xyear.glm", 
                              "fall.stoptype.imp3900xdate.glm","fall.stoptype.imp3900xlat.glm", 
                              "fall.stoptype.imp3900xlon.glm","fall.stoptype.imp3900xBCR.glm",
                              "fall.stoptype.imp1859.glm", "fall.stoptype.imp3900.glm",
                              "fall.stoptype.null.glm")
aictab(cand.set=cand.set.fall.stoptype.5, modnames = modnames.fall.stoptype.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.fall.stoptype.6 <- list(fall.stoptype.age.glm, fall.stoptype.sex.glm, 
                                 fall.stoptype.ageplussex.glm, fall.stoptype.agexsex.glm,
                                 fall.stoptype.null.glm, fall.stoptype.year.glm, 
                                 fall.stoptype.date.glm, fall.stoptype.yearplusdate.glm,
                                 fall.stoptype.lat.glm, fall.stoptype.lon.glm,
                                 fall.stoptype.BCR.glm,
                                 fall.stoptype.imp1859xage.glm, fall.stoptype.imp1859xsex.glm, 
                                 fall.stoptype.imp1859xyear.glm, fall.stoptype.imp1859xdate.glm,
                                 fall.stoptype.imp1859xlat.glm, fall.stoptype.imp1859xlon.glm,
                                 fall.stoptype.imp1859xBCR.glm,fall.stoptype.imp3900xage.glm, 
                                 fall.stoptype.imp3900xsex.glm, fall.stoptype.imp3900xyear.glm, 
                                 fall.stoptype.imp3900xdate.glm, fall.stoptype.imp3900xlat.glm, 
                                 fall.stoptype.imp3900xlon.glm, fall.stoptype.imp3900xBCR.glm,
                                 fall.stoptype.imp1859.glm, fall.stoptype.imp3900.glm)
modnames.fall.stoptype.6 <- c("Age", "Sex",
                              "Age + Sex", "Age* Sex",
                              "Null", "Year",
                              "Date", "Year + Date",
                              "Lat", "Lon",
                              "BCR",
                              "Imp1.859*Age", "Imp1.859*Sex", 
                              "Imp1.859*Year", "Imp1.859*Date",
                              "Imp1.859*Lat", "Imp1.859*Lon",
                              "Imp1.859*BCR", "Imp3.900*Age", 
                              "Imp3.900*Sex", "Imp3.900*Year", 
                              "Imp3.900*Date","Imp3.900*Lat", 
                              "Imp3.900*Lon","Imp3.900*BCR",
                              "Imp1.859", "Imp3.900")
FallStopTypeFullAIC<- aictab(cand.set=cand.set.fall.stoptype.6, modnames = modnames.fall.stoptype.6,  
                             digits = 2, second.ord = T)
FallStopTypeFullAIC
#write.csv(FallStopTypeFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/FallStopType2FullAIC.csv")

#Calculating psuedo R-squared
#install.packages("descr")
library(descr)
LogRegR2(fall.stoptype.null.glm)
LogRegR2(fall.stoptype.age.glm)
LogRegR2(fall.stoptype.sex.glm)
LogRegR2(fall.stoptype.date.glm)
####Model selection for stop type in the Spring #####
#install.packages("AICcmodavg")
library(AICcmodavg)
#converting the stop type field into numbers, 0 for stop and 1 for stopover
data.spring.agesex.2$stop <- "NA"
View(data.spring.agesex.2)

for (i in 1:nrow(data.spring.agesex.2)){
  
  data.spring.agesex.2$stop[i]<- ifelse(test=data.spring.agesex.2$stop_type[i]=="stop", yes="0",
                                        no="1")
  
}
View(data.spring.agesex.2)

#Running models for the effects of each combination of individual variables (age and sex)
#on stop type

spring.stoptype.age.glm <-glm(as.factor(stop) ~ as.factor(age), family=binomial, 
                              data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.age.glm)


spring.stoptype.sex.glm<-glm(as.factor(stop) ~ as.factor(sex), family=binomial, 
                             data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.sex.glm)


spring.stoptype.ageplussex.glm <-glm(as.factor(stop)~ as.factor(age)+as.factor(sex), family=binomial, 
                                     data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.ageplussex.glm)


spring.stoptype.agexsex.glm <- glm(as.factor(stop) ~ as.factor(age)*as.factor(sex), family=binomial, 
                                   data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.agexsex.glm)


spring.stoptype.null.glm <- glm(as.factor(stop) ~ 1, family=binomial, 
                                data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.null.glm)

#AICc model selection on the above models
cand.set.spring.stoptype.1 <- list(spring.stoptype.age.glm, spring.stoptype.sex.glm, spring.stoptype.ageplussex.glm, 
                                   spring.stoptype.agexsex.glm, spring.stoptype.null.glm)
modnames.spring.stoptype.1 <- c("spring.stoptype.age.glm", "spring.stoptype.sex.glm", 
                                "spring.stoptype.ageplussex.glm", "spring.stoptype.agexsex.glm", 
                                "spring.stoptype.null.glm")
aictab(cand.set=cand.set.spring.stoptype.1, modnames = modnames.spring.stoptype.1,  
       digits = 2, second.ord = T)
##Because the null model was the most supported(deltaAICc=0), I won't be including 
##age or sex in the rest of my stopover duration analysis 

#Running models on the effects of temporal variables (only date included in this analysis) on stop type
#using the most supported individual effects model
#in this case, the null model

spring.stoptype.year.glm <-glm(as.factor(stop) ~ as.factor(year), family=binomial, 
                               data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.year.glm)


spring.stoptype.date.glm <-glm(as.factor(stop) ~ date.ord, family=binomial, 
                               data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.date.glm)


spring.stoptype.yearplusdate.glm <-glm(as.factor(stop) ~ as.factor(year)+date.ord, family=binomial, 
                                       data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.yearplusdate.glm)

#Model selection on the above models

cand.set.spring.stoptype.2 <- list(spring.stoptype.year.glm, spring.stoptype.date.glm, 
                                   spring.stoptype.yearplusdate.glm, spring.stoptype.null.glm)
modnames.spring.stoptype.2 <- c("spring.stoptyp.year.glm", "spring.stoptype.date.glm", 
                                "spring.stoptype.yearplusdate.glm", "spring.stoptype.null.glm")
aictab(cand.set=cand.set.spring.stoptype.2, modnames = modnames.spring.stoptype.2,  
       digits = 2, second.ord = T)
##Null was still the most supported model

#Running models on the spatial variables (lat, lon, and BCR) using the most supported
#temporal and individual model
#But not including BCR with lat and lon in the same model

spring.stoptype.lat.glm <-glm(as.factor(stop) ~ centroid_lat, family=binomial, 
                              data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.lat.glm)


spring.stoptype.lon.glm <-glm(as.factor(stop) ~ centroid_lon, family=binomial, 
                              data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.lon.glm)


spring.stoptype.latpluslon.glm <-glm(as.factor(stop) ~ centroid_lat+centroid_lon, family=binomial, 
                                     data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.lon.glm)


spring.stoptype.BCR.glm <-glm(as.factor(stop) ~ as.factor(BCR2), family=binomial, 
                              data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.BCR.glm)


#Model selection on the above models
cand.set.spring.stoptype.3 <- list(spring.stoptype.lat.glm, spring.stoptype.lon.glm, 
                                   spring.stoptype.latpluslon.glm,
                                   spring.stoptype.BCR.glm, spring.stoptype.null.glm)
modnames.spring.stoptype.3 <- c("spring.stoptype.lat.glm", "spring.stoptype.lon.glm", 
                                "spring.stoptype .latpluslon.glm",
                                "spring.stoptype.BCR.glm", "spring.stoptype.null.glm")
aictab(cand.set=cand.set.spring.stoptype.3, modnames = modnames.spring.stoptype.3,  
       digits = 2, second.ord = T)

##The null model was still most supported
#Testing the most supported model with both mean impervious radii

spring.stoptype.imp1859.glm <- glm(as.factor(stop) ~ mean_imp_cover_1859, 
                                   family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp1859.glm)


spring.stoptype.imp3900.glm <- glm(as.factor(stop) ~ mean_imp_cover_3900, 
                                   family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900.glm)


#Model selection on the above models
cand.set.spring.stoptype.4 <- list(spring.stoptype.imp1859.glm, spring.stoptype.imp3900.glm,
                                   spring.stoptype.null.glm)
modnames.spring.stoptype.4 <- c("spring.stoptype.imp1859.glm", "spring.stoptype.imp3900.glm", 
                                "spring.stoptype.null.glm")
aictab(cand.set=cand.set.spring.stoptype.4, modnames = modnames.spring.stoptype.4,  
       digits = 2, second.ord = T)
##All models are supported

#Model selection with impervious surfaces and interactions

##age
spring.stoptype.imp1859xage.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(age),
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp1859xage.glm)

##sex
spring.stoptype.imp1859xsex.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(sex),
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp1859xsex.glm)

##year
spring.stoptype.imp1859xyear.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(year),
                                       family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.impx1859xyear.glm)

#date
spring.stoptype.imp1859xdate.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*date.ord,
                                       family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.impx1859date.glm)

##lat
spring.stoptype.imp1859xlat.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*centroid_lat,
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp1859xlat.glm)

##lon
spring.stoptype.imp1859xlon.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*centroid_lon,
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp1859xlon.glm)

##BCR
spring.stoptype.imp1859xBCR.glm<- glm(as.factor(stop) ~ mean_imp_cover_1859*as.factor(BCR2),
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp1859xBCR.glm)
#and for the larger radius as well

##age
spring.stoptype.imp3900xage.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(age),
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900xage.glm)


spring.stoptype.imp3900xsex.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(sex),
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900xsex.glm)


spring.stoptype.imp3900xyear.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(year),
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900xyear.glm)


spring.stoptype.imp3900xdate.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*date.ord,
                                       family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900xdate.glm)


spring.stoptype.imp3900xlat.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*centroid_lat,
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900xlat.glm)


spring.stoptype.imp3900xlon.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*centroid_lon,
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900xlon.glm)

##BCR
spring.stoptype.imp3900xBCR.glm<- glm(as.factor(stop) ~ mean_imp_cover_3900*as.factor(BCR2),
                                      family=binomial, data=data.spring.agesex.2, weights=w)
summary(spring.stoptype.imp3900xBCR.glm)


#Model selection on the above models
cand.set.spring.stoptype.5 <- list(spring.stoptype.imp1859xage.glm, spring.stoptype.imp1859xsex.glm, 
                                   spring.stoptype.imp1859xyear.glm, spring.stoptype.imp1859xdate.glm,
                                   spring.stoptype.imp1859xlat.glm, spring.stoptype.imp1859xlon.glm,
                                   spring.stoptype.imp1859xBCR.glm, spring.stoptype.imp3900xage.glm, 
                                   spring.stoptype.imp3900xsex.glm, spring.stoptype.imp3900xyear.glm, 
                                   spring.stoptype.imp3900xdate.glm, spring.stoptype.imp3900xlat.glm, 
                                   spring.stoptype.imp3900xlon.glm, spring.stoptype.imp3900xBCR.glm,
                                   spring.stoptype.imp1859.glm, spring.stoptype.imp3900.glm,
                                   spring.stoptype.null.glm)
modnames.spring.stoptype.5 <- c("spring.stoptype.imp1859xage.glm", "spring.stoptype.imp1859xsex.glm", 
                                "spring.stoptype.imp1859xyear.glm", "spring.stoptype.imp1859xdate.glm",
                                "spring.stoptype.imp1859xlat.glm", "spring.stoptype.imp1859xlon.glm",
                                "spring.stoptype.imp1859xBCR.glm", "spring.stoptype.imp3900xage.glm", 
                                "spring.stoptype.imp3900xsex.glm", "spring.stoptype.imp3900xyear.glm",
                                "spring.stoptype.imp3900xdate.glm","spring.stoptype.imp3900xlat.glm", 
                                "spring.stoptype.imp3900xlon.glm","spring.stoptype.imp3900xBCR.glm",
                                "spring.stoptype.imp1859.glm", "spring.stoptype.imp3900.glm",
                                "spring.stoptype.null.glm")
aictab(cand.set=cand.set.spring.stoptype.5, modnames = modnames.spring.stoptype.5,  
       digits = 2, second.ord = T)
#And creating an AIC table of all the models run
cand.set.spring.stoptype.6 <- list(spring.stoptype.age.glm, spring.stoptype.sex.glm, 
                                   spring.stoptype.ageplussex.glm, spring.stoptype.agexsex.glm,
                                   spring.stoptype.null.glm, spring.stoptype.year.glm, 
                                   spring.stoptype.date.glm, spring.stoptype.yearplusdate.glm, 
                                   spring.stoptype.lat.glm, spring.stoptype.lon.glm,
                                   spring.stoptype.latpluslon.glm, spring.stoptype.BCR.glm,
                                   spring.stoptype.imp1859xage.glm, spring.stoptype.imp1859xsex.glm, 
                                   spring.stoptype.imp1859xyear.glm, spring.stoptype.imp1859xdate.glm,
                                   spring.stoptype.imp1859xlat.glm, spring.stoptype.imp1859xlon.glm,
                                   spring.stoptype.imp1859xBCR.glm, spring.stoptype.imp3900xage.glm, 
                                   spring.stoptype.imp3900xsex.glm,  spring.stoptype.imp3900xyear.glm, 
                                   spring.stoptype.imp3900xdate.glm, spring.stoptype.imp3900xlat.glm, 
                                   spring.stoptype.imp3900xlon.glm, spring.stoptype.imp3900xBCR.glm,
                                   spring.stoptype.imp1859.glm, spring.stoptype.imp3900.glm)
modnames.spring.stoptype.6 <- c("Age", "Sex",
                                "Age + Sex", "Age*Sex",
                                "Null", "Year",
                                "Date", "Year + Date",
                                "Lat", "Lon",
                                "Lat + Lon", "BCR",
                                "Imp1.859*Age", "Imp1.859*Sex", 
                                "Imp1.859*Year", "Imp1.859*Date",
                                "Imp1.859*Lat", "Imp1.859*Lon",
                                "Imp1.859*BCR", "Imp3.900*Age", 
                                "Imp3.900*Sex", "Imp3.900*Year",
                                "Imp3.900*Date","Imp3.900*Lat", 
                                "Imp3.900*Lon","Imp3.900*BCR",
                                "Imp1.859", "Imp3.900")
SpringStopTypeFullAIC<- aictab(cand.set=cand.set.spring.stoptype.6, modnames = modnames.spring.stoptype.6,  
                               digits = 2, second.ord = T)
SpringStopTypeFullAIC
#write.csv(SpringStopTypeFullAIC, file="C:/Users/zoepa/OneDrive/Desktop/Honors thesis 2/SpringStopType2FullAIC.csv")

#Calculating psuedo R-squared
LogRegR2(spring.stoptype.age.glm)
LogRegR2(spring.stoptype.lat.glm)
LogRegR2(spring.stoptype.imp1859.glm)
LogRegR2(spring.stoptype.imp3900.glm)
LogRegR2(spring.stoptype.lon.glm)
##################Calculating stats for the results############################
nrow(data.fall.agesex.2)
length(unique(data.fall.agesex.2$id))

nrow(data.spring.agesex.2)
length(unique(data.spring.agesex.2$id))

table(data.fall.agesex.2$age, data.fall.agesex.2$sex)
table(data.spring.agesex.2$age, data.spring.agesex.2$sex)

nrow(data.fall.stopover.agesex)
length(unique(data.fall.stopover.agesex$id))

nrow(data.spring.stopover.agesex.2)
length(unique(data.spring.stopover.agesex.2$id))

nrow(data.fall.agesex.dist2)
length(unique(data.fall.agesex.dist2$id))

nrow(data.spring.agesex.dist2)
length(unique(data.spring.agesex.dist2$id))

mean(data.fall.agesex.2$mean_imp_cover_1859)
min(data.fall.agesex.2$mean_imp_cover_1859)
max(data.fall.agesex.2$mean_imp_cover_1859)

mean(data.spring.agesex.2$mean_imp_cover_1859)
min(data.spring.agesex.2$mean_imp_cover_1859)
max(data.spring.agesex.2$mean_imp_cover_1859)

mean(data.fall.agesex.2$mean_imp_cover_3900)
min(data.fall.agesex.2$mean_imp_cover_3900)
max(data.fall.agesex.2$mean_imp_cover_3900)

mean(data.spring.agesex.2$mean_imp_cover_3900)
min(data.spring.agesex.2$mean_imp_cover_3900)
max(data.spring.agesex.2$mean_imp_cover_3900)

#############################plotting experimentation####################################
install.packages("ggplot2")
library(ggplot2)

#Plotting each of the different duration datasets
fall.duration.1859.plot<- ggplot(data=Woodcock.fall.stopover, aes(x=mean_imp_cover_1859, y=duration_h))
fall.duration.1859.plot+geom_point(color="blue")+xlab("mean impervious cover")+ylab("stopover duration (h)")+theme_classic()+geom_smooth(method=lm)

fall.duration.3900.plot<- ggplot(data=Woodcock.fall.stopover, aes(x=mean_imp_cover_3900, y=duration_h))
fall.duration.3900.plot+geom_point(color="blue")+xlab("mean impervious cover")+ylab("stopover duration (h)")+theme_classic()+geom_smooth(method=lm)

spring.duration.1859.plot<- ggplot(data=Woodcock.spring.stopover, aes(x=mean_imp_cover_1859, y=duration_h))
spring.duration.1859.plot+geom_point(color="blue")+xlab("mean impervious cover")+ylab("stopover duration (h)")+theme_classic()+geom_smooth(method=lm)

spring.duration.3900.plot<- ggplot(data=Woodcock.spring.stopover, aes(x=mean_imp_cover_3900, y=duration_h))
spring.duration.3900.plot+geom_point(color="blue")+xlab("mean impervious cover")+ylab("stopover duration (h)")+theme_classic()+geom_smooth(method=lm)


mean_imp_cover_1859<- seq(0:100, 1)
#colnames(mean_imp_cover_1859)<- "mean_imp_cover_1859" 

Data.predict<- predict(duration.fall.1859.glm,(newdata=mean_imp_cover_1859))   #, se.fit=T))

View(Data.predict)

Data.predict$lower<- Data.predict$fit - Data.predict$se.fit*1.96
Data.predict$upper<- Data.predict$fit + Data.predict$se.fit*1.96
Data.predict$Duration<- duration.h$duration.h

head(Data.predict)

Duration1.plot<- ggplot(Data.predict, aes(x=duration, y=fit)) 

Duration1.plot +
  geom_point(data=Woodcock.fall.stopover, aes(x=mean_imp_cover_1859, y=duration_h), size=3)+
  scale_colour_manual+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill= "gray", alpha=0.7)+
  geom_line() +
  ylab ("Mean impervious surface cover") +
  xlab ("Duration of stopover (h)")+ 
  #scale_y_continuous(limits=c(400,550))+
  #scale_x_continuous(limits=c(210,280))+
  theme(text= element_text(size=16)) 

#duration~exp(impervious)

Woodcock.migration <- read.csv("C:/Users/zoepa/OneDrive/Desktop/Honors thesis/migration_data_all_seasons_for_ZP_101323.csv")
View(Woodcock.migration)


#################Making figures###########################
fall.withinstep.imp3900xyear.glm<- glm(mean_step_ ~ as.factor(year)+centroid_lon+centroid_lat+
                                         mean_imp_cover_3900*as.factor(year),
                                       family=gaussian, data=data.fall.stopover.agesex, weights=w)


#creating a series of sequential impervious surface values to work from
#first, figuring out the minimum and maximum values for that column so that I can generate 
#all numbers in between them 
#(rounding the last digit down for minimum and up for maximum to make sure the true value is included)
min(data.fall.stopover.agesex$mean_imp_cover_3900)
max(data.fall.stopover.agesex$mean_imp_cover_3900)
#impervious surface cover ranges from 0.06414 to 61.8908 so I'm including predicted values from 0-62


#Doing the same for year
min(data.fall.stopover.agesex$year)
max(data.fall.stopover.agesex$year)
#years range from 2018-2022



#figuring out the mean latitude and longitude values so that I can hold those constant in the model
mean(data.fall.stopover.agesex$centroid_lat)
mean(data.fall.stopover.agesex$centroid_lon)


#generating all possible combinations of lat and impervious cover
withinsteppredict<-expand.grid(mean_imp_cover_3900=c(0.00: 62.00), 
                                year=c(2018:2022), centroid_lat=38.22865, centroid_lon=-79.53686)

Fall.withinstep.predict.3900 <- data.frame(predict(fall.withinstep.imp3900xyear.glm,
                                                    newdata=withinsteppredict, se.fit=T))
View(Fall.withinstep.predict.3900)

#approximating upper and lower confidence intervals based on SE * 1.96
Fall.withinstep.predict.3900$lower<- Fall.withinstep.predict.3900$fit - Fall.withinstep.predict.3900$se.fit*1.96
Fall.withinstep.predict.3900$upper<- Fall.withinstep.predict.3900$fit + Fall.withinstep.predict.3900$se.fit*1.96
#and also adding columns showing the values for latitude and mean imp cover
Fall.withinstep.predict.3900$mean_imp_cover_3900<- withinsteppredict$mean_imp_cover_3900
Fall.withinstep.predict.3900$centroid_lat<- withinsteppredict$centroid_lat
Fall.withinstep.predict.3900$year<- withinsteppredict$year
Fall.withinstep.predict.3900$centroid_lon<- withinsteppredict$centroid_lon
View(Fall.withinstep.predict.3900)

#creating a plot with the predicted data
library(ggplot2)


quantile(data.fall.stopover.agesex$mean_imp_cover_3900, 0.95)
quantile(data.fall.stopover.agesex$mean_imp_cover_3900, 0.50)

withinstep.predict.plot<- ggplot(Fall.withinstep.predict.3900, aes(x=mean_imp_cover_3900, y=fit)) 
WithinstepPlot2<-withinstep.predict.plot+geom_ribbon(aes(ymin=lower, ymax=upper, group=as.factor(year)), fill="grey89")+
  geom_line(aes(color= as.factor(year), linetype=as.factor(year)),linewidth=0.7)+ 
scale_color_manual(name='Year', labels=c("2018", "2019", "2020", "2021", "2022"), 
                     values=c('red', 'blue', 'green4', 'purple', 'darkorange'))+
scale_linetype_manual(name='Year', labels=c("2018", "2019", "2020", "2021", "2022"), 
                    values=c('solid', 'longdash', 'dotted', 'dotdash', 'dashed'))+
  ylab ("Predicted average within stop short movement distance")+ 
  xlab ("Mean impervious surface cover (%)")+ 
  theme_classic()
WithinstepPlot2

WithinstepPlot3<-WithinstepPlot2+
theme(text= element_text(family="serif", size=11))

WithinstepPlot3

#Adding vertical lines at the points where 50% and 95% of points have smaller impervious covers
WithinstepPlot4<-WithinstepPlot3 +
  geom_vline(xintercept= c(1.345569, 17.98448), linetype='dashed', color='black')
  
WithinstepPlot4

plot(data.fall.agesex.dist2$centroid_lat,
     data.fall.agesex.dist2$mean_imp_cover_3900)
mean(data.fall.stopover.agesex$mean_imp_cover_1859)



betweenstop.predict.restricted<-expand.grid(mean_imp_cover_3900=c(.005378101: 5), 
                                            centroid_lat=c(30:45))

Fall.betweenstop.predict.3900.restricted <- data.frame(predict(fall.betweenstop.imp3900xlat.glm,
                                                               newdata=betweenstop.predict.restricted, se.fit=T))
View(Fall.betweenstop.predict.3900.restricted)

#approximating upper and lower confidence intervals based on SE * 1.96
Fall.betweenstop.predict.3900.restricted$lower<- Fall.betweenstop.predict.3900.restricted$fit - Fall.betweenstop.predict.3900.restricted$se.fit*1.96
Fall.betweenstop.predict.3900.restricted$upper<- Fall.betweenstop.predict.3900.restricted$fit + Fall.betweenstop.predict.3900.restricted$se.fit*1.96
#and also adding columns showing the values for latitude and mean imp cover
Fall.betweenstop.predict.3900.restricted$mean_imp_cover_3900<- betweenstop.predict.restricted$mean_imp_cover_3900
Fall.betweenstop.predict.3900.restricted$centroid_lat<- betweenstop.predict.restricted$centroid_lat
View(Fall.betweenstop.predict.3900.restricted)

#creating a plot with the predicted data
library(ggplot2)

#subsetting the predict dataframe so that it only includes points on 4 latitude values
betweenstop.predict.restsubset<-subset(Fall.betweenstop.predict.3900.restricted, centroid_lat=="30"|centroid_lat=="35"
                                       |centroid_lat=="40"|centroid_lat=="45")
View(betweenstop.predict.restsubset)

betweenstop.predict.restplot<- ggplot(betweenstop.predict.restsubset, aes(x=mean_imp_cover_3900, y=fit)) 
predictedplot<- betweenstop.predict.restplot +geom_ribbon(aes(ymin=lower, ymax=upper, group=as.factor(centroid_lat)), fill= "grey93")+
  geom_line(aes(color= as.factor(centroid_lat),linetype = as.factor(centroid_lat)))+ 
  scale_color_manual(name='Latitude(degrees)', labels=c("30", "35", "40", "45"), 
                     values=c('darkorchid4', 'blue', 'red', 'green4'))+
  scale_linetype_manual(name='Latitude(degrees)', labels=c("30", "35", "40", "45"), 
                        values=c('solid', 'longdash', 'dotted', 'dotdash'))+
  ylab ("Predicted migratory step distance (km)")+ 
  xlab ("Mean impervious surface cover (%)")+ 
  theme(text= element_text(size=16, family="TT Times Tew Roman"))+
  theme_classic()
predictedplot

#split the dataset into northern and southern latitude and analyze for post-hoc analysis 

fall.1859.hist<-ggplot(data.fall.agesex.2, aes(mean_imp_cover_1859))
fall.1859.hist + geom_histogram(binwidth=0.5, color="blue")+
  xlab("Mean percent impervious surface cover")+
  ylab("Count")+
 theme_classic()+
  theme(text=element_text(size=16, family="serif"))

fall.3900.hist<-ggplot(data.fall.agesex.2, aes(mean_imp_cover_3900))
fall.3900.hist + geom_histogram(binwidth=0.5, color="red")+
  xlab("Mean percent impervious surface cover")+
  ylab("Count")+
  theme_classic()+
  theme(text=element_text(size=16, family="serif"))

spring.1859.hist<-ggplot(data.spring.agesex.2, aes(mean_imp_cover_1859))
spring.1859.hist + geom_histogram(binwidth=0.5, color="darkseagreen3")+
  xlab("Mean percent impervious surface cover")+
  ylab("Count")+
  theme_classic()+
  theme(text=element_text(size=16, family="serif"))

spring.3900.hist<-ggplot(data.spring.agesex.2, aes(mean_imp_cover_3900))
spring.3900.hist + geom_histogram(binwidth=0.5, color="darkorange")+
  xlab("Mean percent impervious surface cover")+
  ylab("Count")+
  theme_classic()+
  theme(text=element_text(size=16, family="serif"))

#Boxplot of impervious surface cover by age-sex class
#Combining age and sex into one variable
data.fall.agesex.2$AgeSex<- as.factor(paste(data.fall.agesex.2$age,data.fall.agesex.2$sex))
summary(data.fall.agesex.2$AgeSex)
data.spring.agesex.2$AgeSex<- as.factor(paste(data.spring.agesex.2$age,data.spring.agesex.2$sex))
summary(data.spring.agesex.2$AgeSex)

View(data.fall.agesex.2)
View(data.spring.agesex.2)

#adding the spring and fall data into one dataframe
FullData<-rbind(data.fall.agesex.2, data.spring.agesex.2)
nrow(FullData)
View(FullData)

library(ggplot2)
Woodcock.boxplot<- ggplot(FullData, aes(AgeSex, mean_imp_cover_1859, fill=as.factor(season)))

Woodcock.boxplot + 
  geom_boxplot(aes(fill=as.factor(season)))+
scale_fill_manual(name='Season', values=c("royalblue1", "cornsilk"))+
  xlab("Age and Sex")+
  ylab("Mean percent impervious surface cover (1.859 km radius)")+
  theme_classic()+
  theme(text=element_text(size=12, family="serif"))

#Making figure four with just the plots of everything
FallDuration1859 <- ggplot(data.fall.stopover.agesex, aes(mean_imp_cover_1859,
                                                          duration_h))

FallDuration1859Plot<-FallDuration1859+geom_point(size=0.3)+
  xlab("Mean %imp cover (1.859 km radius)")+
  ylab("Stopover duration (hrs)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
FallDuration1859Plot


FallDuration3900 <- ggplot(data.fall.stopover.agesex, aes(mean_imp_cover_3900,
                                                          duration_h))
FallDuration3900Plot<-FallDuration3900+geom_point(size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Stopover duration (hrs)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
FallDuration3900Plot

#spring duration
SpringDuration1859 <- ggplot(data.spring.stopover.agesex.2, aes(mean_imp_cover_1859,
                                                          duration_h))
SpringDuration1859Plot<-SpringDuration1859+geom_point(size=0.3)+
  xlab("Mean %imp cover (1.859 km radius)")+
  ylab("Stopover duration (hrs)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
SpringDuration1859Plot

SpringDuration3900 <- ggplot(data.spring.stopover.agesex.2, aes(mean_imp_cover_3900,
                                                                duration_h))
SpringDuration3900Plot<-SpringDuration3900+geom_point(size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Stopover duration (hrs)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
SpringDuration3900Plot

#Within stop movement length
FallWithinstep1859 <- ggplot(data.fall.stopover.agesex, aes(mean_imp_cover_1859,
                                                          mean_step_))
FallWithinstep1859Plot<-FallWithinstep1859+geom_point(size=0.4)+
  xlab("Mean %imp cover (1.859 km radius)")+
  ylab("Mean within stop movement length (km)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
FallWithinstep1859Plot

FallWithinstep3900 <- ggplot(data.fall.stopover.agesex, aes(mean_imp_cover_3900,
                                                          mean_step_))
FallWithinstep3900Plot<-FallWithinstep3900+geom_point(size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Mean within stop movement length (km)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
FallWithinstep3900Plot

SpringWithinstep1859 <- ggplot(data.spring.stopover.agesex.2, aes(mean_imp_cover_1859,
                                                            mean_step_))
SpringWithinstep1859Plot<-SpringWithinstep1859+geom_point(size=0.3)+
  xlab("Mean %imp cover (1.859 km radius)")+
  ylab("Mean within stop movement length (km)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
SpringWithinstep1859Plot

SpringWithinstep3900 <- ggplot(data.spring.stopover.agesex.2, aes(mean_imp_cover_3900,
                                                                  mean_step_))
SpringWithinstep3900Plot<-SpringWithinstep3900+geom_point(size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Mean within stop movement length (km)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
SpringWithinstep3900Plot

FallBetweenstop1859 <- ggplot(data.fall.agesex.dist2, aes(mean_imp_cover_1859, as.numeric(step_distkm)))
FallBetweenstop1859Plot<-FallBetweenstop1859+geom_point(size=0.3)+
  xlab("Mean %imp cover (1.859 km radius)")+
  ylab("Migratory step length (km)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
FallBetweenstop1859Plot

FallBetweenstop3900 <- ggplot(data.fall.agesex.dist2, aes(mean_imp_cover_3900, as.numeric(step_distkm)))
FallBetweenstop3900Plot<-FallBetweenstop3900+geom_point(size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Migratory step length (km)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
FallBetweenstop3900Plot

SpringBetweenstop1859 <- ggplot(data.spring.agesex.dist2, aes(mean_imp_cover_1859, as.numeric(step_distkm)))
SpringBetweenstop1859Plot<-SpringBetweenstop1859+geom_point(size=0.3)+
xlab("Mean %imp cover (1.859 km radius)")+
ylab("Migratory step length (km)")+
theme_classic()+
theme(text=element_text(size=6, family="serif"))
SpringBetweenstop1859Plot

SpringBetweenstop3900 <- ggplot(data.spring.agesex.dist2, aes(mean_imp_cover_3900, as.numeric(step_distkm)))
SpringBetweenstop3900Plot<-SpringBetweenstop3900+geom_point(size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Migratory step length (km)")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))
SpringBetweenstop3900Plot

FallStoptype1859<- ggplot(data.fall.agesex.2, aes(mean_imp_cover_1859, stop_type))
FallStoptype1859Plot<-FallStoptype1859 + 
  geom_boxplot(outlier.size=0.3)+
  xlab("Mean %imp cover (1.859 km radius)")+
  ylab("Stop type")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))

FallStoptype3900<- ggplot(data.fall.agesex.2, aes(mean_imp_cover_3900, stop_type))
FallStoptype3900Plot<-FallStoptype3900 + 
  geom_boxplot(outlier.size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Stop type")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))

SpringStoptype1859<- ggplot(data.spring.agesex.2, aes(mean_imp_cover_1859, stop_type))
SpringStoptype1859Plot<-SpringStoptype1859 + 
  geom_boxplot(outlier.size=0.3)+
  xlab("Mean %imp cover (1.859 km radius)")+
  ylab("Stop type")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))

SpringStoptype3900<- ggplot(data.spring.agesex.2, aes(mean_imp_cover_3900, stop_type))
SpringStoptype3900Plot<-SpringStoptype3900 + 
  geom_boxplot(outlier.size=0.3)+
  xlab("Mean %imp cover (3.900 km radius)")+
  ylab("Stop type")+
  theme_classic()+
  theme(text=element_text(size=6, family="serif"))

#install.packages("cowplot")
library(cowplot)


chronology.grid<-plot_grid(FallDuration1859Plot, FallDuration3900Plot,
                           SpringDuration1859Plot, SpringDuration3900Plot,
                           FallWithinstep1859Plot, FallWithinstep3900Plot,
                           SpringWithinstep1859Plot, SpringWithinstep3900Plot,
                        FallBetweenstop1859Plot, FallBetweenstop3900Plot,
                        SpringBetweenstop1859Plot, SpringBetweenstop3900Plot,
                        FallStoptype1859Plot, FallStoptype3900Plot,
                        SpringStoptype1859Plot, SpringStoptype3900Plot,
                           label_size=12,
                           ncol=4
)


chronology.grid

chronology.grid2<-plot_grid(FallDuration1859Plot, 
                           SpringDuration1859Plot, 
                           FallWithinstep1859Plot, 
                           SpringWithinstep1859Plot, 
                           FallBetweenstop1859Plot, 
                           SpringBetweenstop1859Plot, 
                           FallStoptype1859Plot, 
                           SpringStoptype1859Plot, 
                           labels=c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"),
                           label_size=8,
                           ncol=2,
                           align="h"
)

chronology.grid2
ggsave("chronology.grid2.jpeg", device="jpeg",
       scale = 1, width = 6, height = 8, units = c("in"),
       dpi = 1200, limitsize = TRUE)
chronology.grid3<-plot_grid(FallDuration3900Plot,
                            SpringDuration3900Plot,
                           FallWithinstep3900Plot,
                           SpringWithinstep3900Plot,
                           FallBetweenstop3900Plot,
                           SpringBetweenstop3900Plot,
                           FallStoptype3900Plot,
                           SpringStoptype3900Plot,
                           labels=c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"),
                           label_size=8,
                           ncol=2
)

#This saves as a jpeg - will also work with individual ggplot figures
ggsave("chronology.grid3.jpeg", device="jpeg",
       scale = 1, width = 6, height = 8, units = c("in"),
       dpi = 1200, limitsize = TRUE)

####Post-hoc experimentation##########
HighDuration<-subset(data.fall.stopover.agesex, duration_h > 500)
View(HighDuration)
max(HighDuration$mean_imp_cover_1859)

HighImpervious<- subset(data.fall.stopover.agesex, mean_imp_cover_1859>22)
View(HighImpervious)
max(HighImpervious$duration_h)


#experimenting with looking at the data as en exponential function
install.packages("drc")
library(drc)
install.packages("nlme")
install.packages("aomisc")
library(aomisc)
model <- drm(duration_h ~ mean_imp_cover_1859, fct = DRC.expoDecay(),
             data = data.fall.stopover.agesex)
summary(model)
test<-lm(log(duration_h)~ mean_imp_cover_1859, data=data.fall.stopover.agesex, weights=w)
summary(test)


