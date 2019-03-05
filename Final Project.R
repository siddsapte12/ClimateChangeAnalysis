
#set environment
setwd("E:/r mining/emission")
#importing lib
library(countrycode)
library(dplyr)
library(ggplot2)
library(reshape2)
#library(tidyr)
library(ggrepel)


#importing csv
ghg = read.csv("Greenhouse Gas (GHGs) Emissions without Land Use, Land-Use Change and Forestry (LULUCF), in Gigagrams (Gg) CO2 equivalent UN DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
co = read.csv("Carbon dioxide (CO2) Emissions without Land Use, Land-Use Change and Forestry (LULUCF), in Gigagrams (Gg) UN DATA REPORT (1).csv",header = T, sep = ",",stringsAsFactors = F)
hfc = read.csv("Hydrofluorocarbons (HFCs) Emissions, in Gigagrams (Gg) CO2 equivalent UN DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
methane = read.csv("Methane (CH4) Emissions without Land Use, Land-Use Change and Forestry (LULUCF), in Gigagrams (Gg) CO2 equivalent UN DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
no = read.csv("Nitrous oxide (N2O) Emissions without Land Use, Land-Use Change and Forestry (LULUCF), in Gigagrams (Gg) CO2 equivalent UN DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
pfc = read.csv("Perfluorocarbons (PFCs) Emissions, in Gigagrams (Gg) CO2 equivalent EU DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
sul = read.csv("Sulphur hexafluoride (SF6) Emissions, in Gigagrams (Gg) CO2 equivalent  EU DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
sources = read.csv("global.1751_2010.csv",header = T, sep = ",",stringsAsFactors = F)
co.emmission = read.csv("Carbon dioxide emissions (CO2), metric tons of CO2 per capita (CDIAC) UN DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
co.per.dollar = read.csv("Carbon dioxide emissions (CO2), kg CO2 per $1 GDP (PPP) (UNFCCC) UN DATA REPORT.csv",header = T, sep = ",",stringsAsFactors = F)
reason = read.csv("nation.1751_2014.csv",header = T, sep = ",",stringsAsFactors = F)
gdp = read.csv("UNdata_Export_20190214_070854165 (1).csv",sep = ",",stringsAsFactors = F)
sealevel<-read.csv("csiro_alt_gmsl_yr_2015_sealevelcsv.csv",stringsAsFactors = FALSE)
reason<-read.csv("nation.1751_2014.csv",stringsAsFactors = FALSE)
glacier<-read.csv("glaciers_csv.csv",stringsAsFactors = FALSE)
pop = population

#mapping contry and continent using countrycode()
cont.conti = countrycode(ghg$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.co = countrycode(co$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.hfc = countrycode(hfc$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.methane = countrycode(methane$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.no = countrycode(no$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.pfc = countrycode(pfc$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.sul = countrycode(sul$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.ce = countrycode(co.emmission$Country.or.Area,origin = "country.name",destination = "continent")
cont.conti.nation = countrycode(reason$Nation,origin = "country.name",destination = "continent")


##Load country-year observations into variable (mainly for Country name, continent and eurocontrol_pru)
cont.code = countrycode::codelist_panel


#with 6 continents
cont.conti=replace(cont.conti, is.na(cont.conti), "Europe")
cont.conti.co =replace(cont.conti.co, is.na(cont.conti.co), "Europe")
cont.conti.hfc =replace(cont.conti.hfc, is.na(cont.conti.hfc), "Europe")
cont.conti.methane =replace(cont.conti.methane, is.na(cont.conti.methane), "Europe")
cont.conti.no =replace(cont.conti.no, is.na(cont.conti.no), "Europe")
cont.conti.pfc =replace(cont.conti.pfc, is.na(cont.conti.pfc), "Europe")
cont.conti.sul =replace(cont.conti.sul, is.na(cont.conti.sul), "Europe")
cont.conti.ce = replace(cont.conti.ce, is.na(cont.conti.ce),"Europe")
cont.conti.nation = replace(cont.conti.nation, is.na(cont.conti.nation),"Europe")


#attaching continent column to all gases csv
ghg$conti = cont.conti
co$conti = cont.conti.co
hfc$conti = cont.conti.hfc  
methane$conti = cont.conti.methane
no$conti = cont.conti.no
pfc$conti = cont.conti.pfc
sul$conti = cont.conti.sul
co.emmission$conti = cont.conti.ce
reason$conti = cont.conti.nation
#gdp$conti = cont.conti.gdp


#replacing country in country code
cont.code$country.name.en = replace(cont.code$country.name.en,cont.code$country.name.en == "United States", "United States of America")

#calculating unique countries of north and south america to seperate in the continent
sa.country = as.data.frame(cont.code$country.name.en)
sa.continent = as.data.frame(cont.code$eurocontrol_pru)

sa = filter(sa.country,sa.continent == "Southern America")
sa.unique = unique(sa,incomparables = F)
sa.unique

na.country = as.data.frame(cont.code$country.name.en, stringsAsFactors = F)
na.continent = as.data.frame(cont.code$eurocontrol_pru, stringsAsFactors = F)

na = filter(na.country,na.continent == "Northern America")
na.unique = unique(na)
na.unique

##-----------------------------------------------------------------------------#
#for plotting emission continent wise,we are sorting the data with respect to continent and then converting Americas
#into north and south america.
#green house gas
ec=ghg
ec.agg<-aggregate(ec$Value,by=list(ec$Country.or.Area),FUN=mean)
ec.agg$continent<-countrycode(ec.agg$Group.1,origin = "country.name",destination = "continent")
ec.agg$continent<-replace(ec.agg$continent,is.na(ec.agg$continent),"Europe")

#loop to sort countries based on north and south america---------------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(ec.agg$Group.1))
  {
    if( sa.unique[i,1]== ec.agg$Group.1[j]){
      
      ec.agg$continent[j] = "South America"
    }
  }}



p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec.agg$Group.1))
  {
    if( na.unique[p,1]== ec.agg$Group.1[q]){
      
      ec.agg$continent[q] = "North America"
    }
  }}
ec.agg.ghg<-aggregate(ec.agg$x,by=list(ec.agg$continent),FUN=sum)
#Emissions continent wise
#co emission
ec.co=co
ec.coagg<-aggregate(ec.co$Value,by=list(ec.co$Country.or.Area),FUN=mean)
ec.coagg$continent<-countrycode(ec.coagg$Group.1,origin = "country.name",destination = "continent")
ec.coagg$continent<-replace(ec.coagg$continent,is.na(ec.coagg$continent),"Europe")
#co loop to sort countries based on north and south america---------------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(ec.coagg$Group.1))
  {
    if( sa.unique[i,1]== ec.coagg$Group.1[j]){
      
      ec.coagg$continent[j] = "South America"
    }
  }}



p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec.coagg$Group.1))
  {
    if( na.unique[p,1]== ec.coagg$Group.1[q]){
      
      ec.coagg$continent[q] = "North America"
    }
  }}
ec.agg.co<-aggregate(ec.coagg$x,by=list(ec.coagg$continent),FUN=sum)


#sorting hydro floro carbons
ec.hfc=hfc
ec.hfcagg<-aggregate(ec.hfc$Value,by=list(ec.hfc$Country.or.Area),FUN=mean)
ec.hfcagg$continent<-countrycode(ec.hfcagg$Group.1,origin = "country.name",destination = "continent")
ec.hfcagg$continent<-replace(ec.hfcagg$continent,is.na(ec.hfcagg$continent),"Europe")
#hfc loop to sort countries based on north and south america---------------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(ec.hfcagg$Group.1))
  {
    if( sa.unique[i,1]== ec.hfcagg$Group.1[j]){
      
      ec.hfcagg$continent[j] = "South America"
    }
  }}

p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec.hfcagg$Group.1))
  {
    if( na.unique[p,1]== ec.hfcagg$Group.1[q]){
      
      ec.hfcagg$continent[q] = "North America"
    }
  }}

ec.agg.hfc<-aggregate(ec.hfcagg$x,by=list(ec.hfcagg$continent),FUN=sum)


# sorting methane
ec.methane = methane
ec.methaneagg<-aggregate(ec.methane$Value,by=list(ec.methane$Country.or.Area),FUN=mean)

ec.methaneagg$continent<-countrycode(ec.methaneagg$Group.1,origin = "country.name",destination = "continent")
ec.methaneagg$continent<-replace(ec.methaneagg$continent,is.na(ec.methaneagg$continent),"Europe")

#methane,loop to sort countries based on north and south america---------------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(ec.methaneagg$Group.1))
  {
    if( sa.unique[i,1]== ec.methaneagg$Group.1[j]){
      
      ec.methaneagg$continent[j] = "South America"
    }
  }}

p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec.methaneagg$Group.1))
  {
    if( na.unique[p,1]== ec.methaneagg$Group.1[q]){
      
      ec.methaneagg$continent[q] = "North America"
    }
  }}


ec.agg.methane<-aggregate(ec.methaneagg$x,by=list(ec.methaneagg$continent),FUN=sum)

#sorting and creating new tables based on nitrous oxide
ec.no = no
ec.noagg<-aggregate(ec.no$Value,by=list(ec.no$Country.or.Area),FUN=mean)
ec.noagg$continent<-countrycode(ec.noagg$Group.1,origin = "country.name",destination = "continent")
ec.noagg$continent<-replace(ec.noagg$continent,is.na(ec.noagg$continent),"Europe")
#no,loop to sort countries based on north and south america---------------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(ec.noagg$Group.1))
  {
    if( sa.unique[i,1]== ec.noagg$Group.1[j]){
      
      ec.noagg$continent[j] = "South America"
    }
  }}

p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec.noagg$Group.1))
  {
    if( na.unique[p,1]== ec.noagg$Group.1[q]){
      
      ec.noagg$continent[q] = "North America"
    }
  }}



ec.agg.no<-aggregate(ec.noagg$x,by=list(ec.noagg$continent),FUN=sum)


#pfc,sorting and storing data 
ec.pfc = pfc
ec.pfcagg<-aggregate(ec.pfc$Value,by=list(ec.pfc$Country.or.Area),FUN=mean)
ec.pfcagg$continent<-countrycode(ec.pfcagg$Group.1,origin = "country.name",destination = "continent")
ec.pfcagg$continent<-replace(ec.pfcagg$continent,is.na(ec.pfcagg$continent),"Europe")
#pfc,loop to sort countries based on north and south america---------------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(ec.pfcagg$Group.1))
  {
    if( sa.unique[i,1]== ec.pfcagg$Group.1[j]){
      
      ec.pfcagg$continent[j] = "South America"
    }
  }}

p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec.pfcagg$Group.1))
  {
    if( na.unique[p,1]== ec.pfcagg$Group.1[q]){
      
      ec.pfcagg$continent[q] = "North America"
    }
  }}



ec.agg.pfc<-aggregate(ec.pfcagg$x,by=list(ec.pfcagg$continent),FUN=sum)

#sul,sorting and storing related data
ec.sul = sul
ec.sulagg<-aggregate(ec.sul$Value,by=list(ec.sul$Country.or.Area),FUN=mean)
ec.sulagg$continent<-countrycode(ec.sulagg$Group.1,origin = "country.name",destination = "continent")
ec.sulagg$continent<-replace(ec.sulagg$continent,is.na(ec.sulagg$continent),"Europe")
#sul,loop to sort countries based on north and south america---------------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(ec.sulagg$Group.1))
  {
    if( sa.unique[i,1]== ec.sulagg$Group.1[j]){
      
      ec.sulagg$continent[j] = "South America"
    }
  }}

p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec.sulagg$Group.1))
  {
    if( na.unique[p,1]== ec.sulagg$Group.1[q]){
      
      ec.sulagg$continent[q] = "North America"
    }
  }}



ec.agg.sul<-aggregate(ec.sulagg$x,by=list(ec.sulagg$continent),FUN=sum)







#plotting greenhouse gasses based on continent
ec = ghg %>% select(1,2,3,4)

#to convert americas to north and south america
p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(ec$Country.or.Area))
  {
    if( na.unique[p,1]== ec$Country.or.Area[q]){
      
      ec$conti[q] = "North America"
    }
  }}
ec.agg = aggregate(ec$Value, by = list(ec$conti, ec$Year), FUN = sum)
names(ec.agg)[1]<-paste("continent")
emsn.conti.ghg = ggplot(data = ec.agg, aes(x = ec.agg$Group.2, y = ec.agg$x,group = continent, color =continent)) +
  geom_line()+
  geom_point()

emsn_plot<-emsn.conti.ghg+scale_x_continuous(breaks  = c(1990:2012))+scale_y_continuous(labels = scales::comma)+xlab("Years")+ylab("Emissions in Gigagrams, CO2 equivalent")+ggtitle("Ghg emissions continent wise")

emsn_plot

#plotting all gases based upon continent
gases = ggplot(data=ec.agg.co, aes(x=ec.agg.co$Group.1, y = ec.agg.co$x,group =1))+
        geom_line(aes(color = "CO2"))+geom_point(aes(color = "CO2"))+
        geom_line(data = ec.agg.hfc, aes(x=ec.agg.hfc$Group.1,y = ec.agg.hfc$x,color ="HFC"))+geom_point(data = ec.agg.hfc, aes(x=ec.agg.hfc$Group.1,y = ec.agg.hfc$x,color ="HFC"))+
        geom_line(data = ec.agg.methane, aes(x=Group.1,y =x,color ="Methane"))+geom_point(data = ec.agg.methane, aes(x=Group.1,y =x,color ="Methane"))+
        geom_line(data = ec.agg.no, aes(x=Group.1,y =x,color ="Nitrous Oxide"))+geom_point(data = ec.agg.no, aes(x=Group.1,y =x,color ="Nitrous Oxide"))+
        geom_line(data = ec.agg.pfc, aes(x=Group.1,y =x,color ="Perfluorocarbons"))+geom_point(data = ec.agg.pfc, aes(x=Group.1,y =x,color ="Perfluorocarbons"))+
        geom_line(data = ec.agg.sul, aes(x=Group.1,y =x,color ="Sulphur hexafluoride"))+geom_point(data = ec.agg.sul, aes(x=Group.1,y =x,color ="Sulphur hexafluoride"))+
        scale_y_continuous(labels = scales::comma)+xlab("Continents")+ylab("Emissions in Gigagrams, CO2 equivalent")+ggtitle("Emission of all the Gases,continent wise")

gases
#-----------------------------------------------------------------------------#
#co2 emissions percentages in ghg emission

#% of co2 in ghg in asia
co.asia = (ec.agg.co[1,2]/ec.agg.ghg[1,2])*100
co.asia
#% of co2 in ghg in europe
co.eu = (ec.agg.co[2,2]/ec.agg.ghg[2,2])*100
co.eu
#% of co2 in ghg in north america
co.na = (ec.agg.co[3,2]/ec.agg.ghg[3,2])*100
co.na
#% of co2 in ghg in oceania
co.oce = (ec.agg.co[4,2]/ec.agg.ghg[4,2])*100
co.oce

#creating a table to show the value
gas.co.continent = matrix(c(co.asia,co.eu,co.na,co.oce),ncol = 1, byrow = T)
colnames(gas.co.continent) = c("perc")
rownames(gas.co.continent) = c("Asia","Europe","North America","Oceania")
gas.co.continent.table = as.table(gas.co.continent)
gas.co.continent.df =as.data.frame(gas.co.continent)

#merging ghg and co2 values based on group.1
merged.ghg.co = merge(ec.agg.ghg,ec.agg.co,by="Group.1")
names(merged.ghg.co)[2]<-paste("GHG")
names(merged.ghg.co)[3]<-paste("CO2")

merged.ghg.co.melted = melt(merged.ghg.co, id.vars = "Group.1")

#plotting the percentages of carbondioxide per continent
ggplot(merged.ghg.co.melted, aes(x=Group.1, y=value)) +
  geom_bar(stat='identity', position='dodge',aes(fill=variable))+
  xlab("Continent")+ylab("Emission")+scale_y_continuous(labels = scales::comma)+
  ggtitle("Percentages of carbon dioxide per continent per ghg gases emission")+
  geom_text(data = gas.co.continent.df, aes(label = paste(round(perc[1],2),"%"),y = 590000, x = 1.3))+
  geom_text(data = gas.co.continent.df, aes(label = paste(round(perc[2],2),"%"),y = 6900000, x = 2.3))+
  geom_text(data = gas.co.continent.df, aes(label = paste(round(perc[3],2),"%"),y = 4200000, x = 3.4))+
  geom_text(data = gas.co.continent.df, aes(label = paste(round(perc[4],2),"%"),y = 830000, x = 4.3))


#getting the value into one dataframe

#---------------------------------------------------------------------#
#reading per kg CO2, per dollar (PPP) data

co2_ppp <- co.per.dollar
str(co2_ppp)

#Omit records that have NULL PPP values
co2_ppp<- na.omit(co2_ppp)

#read country-wise GDP data
str(gdp)
gdp<- gdp[-3] #irrelevant variable

# Trial to check NA#-------------

co2_ppp_gdp<- merge(co2_ppp%>%filter(Year>=1995 & Year<=2012)%>%group_by(Country.or.Area,Year,Value),
                    gdp%>%filter(Year>=1995 & Year<=2012)%>%group_by(Country.or.Area,Year,Value),
                    by = c("Country.or.Area", "Year"), all.x=TRUE)
str(co2_ppp_gdp)

names(co2_ppp_gdp)[3]<- paste("kg.CO2.per.Dollar.PPP")
names(co2_ppp_gdp)[4]<- paste("GDP")

#NULL GDP Values
na.gdp<-unique(co2_ppp_gdp[is.na(co2_ppp_gdp$GDP),]%>% select(1))
na.gdp

#replace these values in GDP data frame
gdp$Country.or.Area<- replace(as.character(gdp$Country.or.Area), gdp$Country.or.Area=="Czechia", values= "Czech Republic")

gdp$Country.or.Area<- replace(as.character(gdp$Country.or.Area), gdp$Country.or.Area=="United Kingdom of Great Britain and Northern Ireland", values= "United Kingdom")

#Merge GDP and PPP data into one dataframe for years 1995-2012
co2_ppp_gdp<- merge(co2_ppp%>%filter(Year>=1995 & Year<=2012)%>%group_by(Country.or.Area,Year,Value),
                    gdp%>%filter(Year>=1995 & Year<=2012)%>%group_by(Country.or.Area,Year,Value),
                    by = c("Country.or.Area", "Year"), all.x=TRUE)
str(co2_ppp_gdp)
names(co2_ppp_gdp)[3]<- paste("kg.CO2.per.Dollar.PPP")
names(co2_ppp_gdp)[4]<- paste("GDP")

#co2_ppp_gdp$Year<- as.numeric(co2_ppp_gdp$Year)

#check for NA 
anyNA(co2_ppp_gdp)

#----------------------------------------------------------------------------------------#
#Average gdp of each year
gdppp_avg<- aggregate(c(co2_ppp_gdp$GDP),by=list(co2_ppp_gdp$Country.or.Area), FUN="mean" )
#Rename columns
colnames(gdppp_avg)<- c("Country.or.Area","GDP")

#Average kg Co2 per dollar GDP (PPP)
PPP<- aggregate(co2_ppp_gdp$kg.CO2.per.Dollar.PPP,by = list(co2_ppp_gdp$Country.or.Area), FUN= "mean")  
#Rename columns
colnames(PPP)<- c("Country.or.Area","PPP")

#merge average PPP with GDP
gdppp_avg<- merge(gdppp_avg, PPP,by = "Country.or.Area",all.y=TRUE)

str(gdppp_avg)
count(gdppp_avg)


gdp_ppp <- gdppp_avg
#Load country-year observations into variable (mainly for Country name, continent and eurocontrol_pru)
cntry <- countrycode::codelist_panel

#cntry_csv<- read.csv("C:/Users/iyeng/Documents/MPS Analytics/ALY 6040/Project/B_emission/country-codes_csv.csv")

#Renamed column name in country for Merge in future.
cntry<- cntry%>% rename(Country.or.Area = country.name.en)

gdp_ppp$continent<- countrycode(gdp_ppp$Country.or.Area, "country.name","continent")

gdp_ppp<- merge ( x= unique(gdp_ppp), y = unique(cntry%>% select(1,9)), by = "Country.or.Area",all.x = TRUE)
#Replace Americas value with corresponding eurocontrol_pru value
gdp_ppp$continent<- replace(gdp_ppp$continent, gdp_ppp$continent=="Americas", gdp_ppp$eurocontrol_pru[which(gdp_ppp$continent=="Americas")])
#remove eurocontrol_pru column (optional step)
gdp_ppp<- within(gdp_ppp, rm(eurocontrol_pru))

str(gdp_ppp)

gdp_ppp_plot<- ggplot(gdp_ppp, aes(x=GDP,y=PPP))+geom_point(aes(color = gdp_ppp$continent))+
  geom_label_repel(aes(label = gdp_ppp$Country.or.Area),
                   box.padding   = 0.35,
                   point.padding = 0.8,
                   segment.color = 'grey50') +
  theme_classic()

gdp_ppp_plot

#--------------------------------------------------------------------#
#Country names as Rownames
rownames(gdppp_avg)<-c(as.character(gdppp_avg$Country.or.Area))

#used get_dist from factoextra package to compute distance matrix between rows
library(factoextra)
distance <- get_dist(gdppp_avg[,2:3])
#used to visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#kmeans clustering , 3 clusters , Low-Moderate-High GDP 
km<-kmeans(gdppp_avg[,2:3],3, iter.max = 2, nstart=3)

fviz_cluster(km, data = gdppp_avg[,2:3])

#---------------------------------------------------------------------#


#co2 emission from various sources###################

reason
reason<-replace(reason,is.na(reason),0)
any(is.na(reason))=="TRUE"
head(reason)

reason$con<-countrycode(reason$Nation,"country.name","continent")
reason$con<-replace(reason$con,is.na(reason$con),"Europe")
#using nation dataset to find out the various sources of emission----------------------------------------------#
i = 0
j = 0
for (i in 1:length(sa.unique[,1])) {
  for (j in 1:length(reason$Nation))
  {
    if( toupper(sa.unique[i,1])== reason$Nation[j]){
      
      reason$con[j] = "South America"
    }
  }}

p=0
q=0

for (p in 1:length(na.unique[,1])) {
  for (q in 1:length(reason$Nation))
  {
    if(toupper( na.unique[p,1])== reason$Nation[q]){
      
      reason$con[q] = "North America"
    }
  }}

library(dplyr)
#to remove unrelated values
reason<-reason %>% 
  filter(!grepl('Americas', con))


reason$con
sort(reason$con)

#to convert each column into integer
reason$Emissions.from.solid.fuel.consumption<-as.integer(reason$Emissions.from.solid.fuel.consumption)
reason$Emissions.from.liquid.fuel.consumption<-as.integer(reason$Emissions.from.liquid.fuel.consumption)
reason$Emissions.from.gas.fuel.consumption<-as.integer(reason$Emissions.from.gas.fuel.consumption)
reason$Emissions.from.cement.production<-as.integer(reason$Emissions.from.cement.production)
reason$Emissions.from.gas.flaring<-as.integer(reason$Emissions.from.gas.flaring)
reason$Per.capita.CO2.emissions..metric.tons.of.carbon.<-as.integer(reason$Per.capita.CO2.emissions..metric.tons.of.carbon.)
class(reason)

a<-aggregate(reason, by=list(reason$con), FUN=mean)
a
a$Nation<-NULL
a$con<-NULL
a$Year<-NULL
names(a)[names(a) == "Group.1"] <- "Continent"

#plot sources of carbon emission-------------------------------------------

ggplot(a,aes(Continent,Emissions.from.liquid.fuel.consumption,group=1))+ 
  geom_line(aes(color = "liquid fuel"))+geom_point(aes(color="liquid fuel"))+
  geom_line(data=a ,aes(Continent,Emissions.from.solid.fuel.consumption,color="solid fuel"))+geom_point(aes(Continent,Emissions.from.solid.fuel.consumption,color="solid fuel"))+
  geom_line(data=a,aes(Continent,Emissions.from.gas.fuel.consumption,color="gas fuel"))+geom_point(aes(Continent,Emissions.from.gas.fuel.consumption,color="gas fuel"))+
  geom_line(data=a,aes(Continent,Emissions.from.cement.production,color="cement production"))+geom_point(aes(Continent,Emissions.from.cement.production,color="cement production"))+
  geom_line(data=a,aes(Continent,Emissions.from.gas.flaring,color="gas flare"))+geom_point(aes(Continent,Emissions.from.gas.flaring,color="gas flare"))+
  ylab("emission levels")+ggtitle("                  Sources of carbon emission")
#-----------------------------------------------------------------------------


######glacier density#########
carbon<-read.csv("nation.1751_2014.csv",stringsAsFactors = FALSE)

carbon<-cbind(carbon$Total.CO2.emissions.from.fossil.fuels.and.cement.production..thousand.metric.tons.of.C.,carbon$Year)
carbon[,1]

glacier

#changing the header 
colnames(carbon)<-c("Emission.Measure","Year")

#merge two data frames based on year
emm_glac<-merge(carbon, glacier%>%select(1,2),by= "Year",all.y=TRUE )
str(emm_glac)

emm_glac<-aggregate(emm_glac$Emission.Measure,by=list(emm_glac$Year,emm_glac$Mean.cumulative.mass.balance),FUN="sum")

colnames(emm_glac)<-c("Year","Mean.cumulative.mass.balance","Carbon.Emmission.Measure")
str(emm_glac)
emm_glac$Mean.cumulative.mass.balance<-as.integer(emm_glac$Mean.cumulative.mass.balance)

#plotting glacier density with respect to year
plot( emm_glac$Year, emm_glac$Mean.cumulative.mass.balance, type="l", col="red",ylab = "glacier density",xlab = "year" ,main="comparison of glacier density and carbon emission with respect to time" )
par(new=TRUE)

with(emm_glac, plot(emm_glac$Year, emm_glac$Carbon.Emmission.Measure, type = "l",col="green", axes=F, xlab=NA, ylab=NA, cex=1))

axis(side = 4)
mtext("carbon emission measure",side = 4, line = 2)
legend("left",legend=c("glacier level","carbon level"), "sea level",lty=1:1, cex=0.7,title = "line types", col=c("red", "green"))




#comparision of carbon emission vs sea level#######

str(sealevel)
sealevel$GMSL<-as.integer(sealevel$GMSL)
sealevel$Time<-NULL
sealevel

sea_level<-merge(carbon, sealevel,by= "Year",all.y=TRUE )
str(sea_level)

sea_level<-aggregate(sea_level$Emission.Measure,by=list(sea_level$Year,sea_level$GMSL),FUN="sum")
colnames(sea_level)<-c("Year","Mean sea level","Carbon.Emmission.Measure")
str(sea_level)
#plot sea level vs carbon
plot( sea_level$Year, sea_level$`Mean sea level`, type="l", col="red",ylab = "mean sea level",xlab = "year" ,main="comparison of mean sea level and carbon emission with respect to time" )
par(new=TRUE)


with(sea_level, plot(sea_level$Year, sea_level$Carbon.Emmission.Measure, type = "l",col="green", axes=F, xlab=NA, ylab=NA, cex=1))

axis(side = 4)
mtext("carbon emission measure",side = 4, line = 2)
legend("topleft",legend=c("sea level","carbon level"), "sea level",lty=1:1, cex=1,title = "line types", col=c("red", "green"))

    
#prediction of sea level
library("forecast")
sea<-ts(sea_level$`Mean sea level`, start=c(2000), end=c(2014))
plot(sea)
fit<-auto.arima(sea)
seafor<-forecast(fit,10)
plot(seafor,ylab = "sea level",xlab="year",main="prediction of sea level")



#prediction of glacier level
glaci<-ts(emm_glac$Mean.cumulative.mass.balance,start = c(2000),end=c(2014))
plot(glaci)
fit1<-auto.arima(glaci)
glacifor<-forecast(fit1,10)
glacifor
plot(glacifor,ylab = "glacier density",xlab = "year",main="prediction of glacier density")

