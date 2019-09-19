
######## Script for Anton et al. 2019 published in NEE    ##########

#Find the paper here: https://www.nature.com/articles/s41559-019-0851-0

# Packages needed to run this code

library(xlsx)
library(plyr)
library("metafor", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("Rmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("plotrix", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tiff", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

# download the data for the analysis in PANGAEA here: https://doi.org/10.1594/PANGAEA.895681 

# LOAD DATA AND PREPARE IT FOR ANALISIS
setwd("/Users/antonga/Dropbox/Andrea_Nate/Invasion calamity/R") # I have it here 
data <- read.csv("Anton_etal_PANGAEA.csv",header=T,sep=",",na.string='') 
data$unique.ID.observation<-as.character(data$unique.ID.observation)
data$exotic.species.name<-as.character(data$exotic.species.name)
data$original_refnum.z<-paste(data$original.reference.number.of.publication,data$exotic.species.name) # Because some studies included several species of exotics, 
# we created a new column that combined the unique study ID and the exotic species



# SENSITIVITY ANALYSIS AND PUBLICATION BIAS 

# Sensitivity Figure 1a
datapp<-data[data$native.trophic.level=="primary producer" & complete.cases(data$native.trophic.level),] 
datah<-data[data$native.trophic.level=="herbivore" & complete.cases(data$native.trophic.level),] 
datad<-data[data$native.trophic.level=="detritivore" & complete.cases(data$native.trophic.level),] 
datao<-data[data$native.trophic.level=="omnivore" & complete.cases(data$native.trophic.level),] 
datap<-data[data$native.trophic.level=="predator" & complete.cases(data$native.trophic.level),] 
datamt<-data[data$native.trophic.level=="several" & complete.cases(data$native.trophic.level),] 

# Sensitivity Figure 1b
datappe<-data[data$exotic.trophic.level=="p. producer" & complete.cases(data$exotic.trophic.level),] 
datahe<-data[data$exotic.trophic.level=="herbivore" & complete.cases(data$exotic.trophic.level),] 
datade<-data[data$exotic.trophic.level=="detritivore" & complete.cases(data$exotic.trophic.level),] 
dataoe<-data[data$exotic.trophic.level=="omnivore" & complete.cases(data$exotic.trophic.level),] 
datape<-data[data$exotic.trophic.level=="predator" & complete.cases(data$exotic.trophic.level),] 

# For the Supplementary Table 5 in the paper, I did a publication bias test for each individual group
#  Publication bias test for omnivores (figure 1a), this is an example, we did this for all the trophic groups 
# for omnivores it was detected one outlier
a<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=datao,method="ML") #p-value of intercept <0.1, the there is assymetry
summary(a)
b<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original.reference.number.of.publication,data=datao) # 1st step to remove outliers
b
c<-rstandard.rma.mv(b) 
c<-c$resid
d<-cbind(datao,c)
e<-d[d$c<(3),] 
e<-e[e$c>(-3),] 
f<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=e,method="ML") # check for asymmetry con el data set without outliers
g<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original.reference.number.of.publication,data=e)
g

# Get the dataset without the outlier
data1<-data[-which(data$unique.ID.observation=="274 2"),] #based on the sensitivity analysis 

# Sensitivity amalysis for Figure 3
datata<-data[data$response.variable=="taxa abundance" & complete.cases(data$response.variable),] 
datatr<-data[data$response.variable=="species richness" & complete.cases(data$response.variable),] 
databe<-data[data$response.variable=="bio elements" & complete.cases(data$response.variable),] 
datarp<-data[data$response.variable=="rate process" & complete.cases(data$response.variable),] 
datas<-data[data$response.variable=="survival" & complete.cases(data$response.variable),] 
datag<-data[data$response.variable=="growth" & complete.cases(data$response.variable),] 
dataf<-data[data$response.variable=="fitness" & complete.cases(data$response.variable),] 
datab<-data[data$response.variable=="behaviour" & complete.cases(data$response.variable),] 
datawq<-data[data$response.variable=="water quality" & complete.cases(data$response.variable),] 
datacs<-data[data$response.variable=="sediment stability" & complete.cases(data$response.variable),] 

# We ran the code below for each response variable
# For the Supplementary Table 5 in the paper, I did a publication bias test for each individual group 
# Five outliers were detected for biochemical elements 
a<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=databe,method="ML")
summary(a)
b<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original.reference.number.of.publication,data=databe) 
b
c<-rstandard.rma.mv(b) 
c<-c$resid
d<-cbind(databe,c)
e<-d[d$c<(3),] 
e<-e[e$c>(-3),] 
f<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=e,method="ML") 
g<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original.reference.number.of.publication,data=e)
g

# Get the dataset without the 5 outliers
data2<-data[-which(data$unique.ID.observation=="274 2" | data$unique.ID.observation=="422 7" | data$unique.ID.observation=="433 5" | data$unique.ID.observation=="603 7" | 
                     data$unique.ID.observation=="737 2" ),] # based on the sensitivity analysis

# Sensitivity analysis for eacj exotic species
# Cut the data by species name
sp18cm<-data[data$exotic.species.name=="Carcinus maenas",] 
sp18cc<-data[data$exotic.species.name=="Caulerpa cylindracea",] 
sp18ll<-data[data$exotic.species.name=="Lophocladia lallemandii",] 
sp18mg<-data[data$exotic.species.name=="Mytilus galloprovincialis",] 
sp18cfor<-data[data$exotic.species.name=="Crepidula fornicata",] 
sp18hs<-data[data$exotic.species.name=="Hemigrapsus sanguineus",] 
sp18ct<-data[data$exotic.species.name=="Caulerpa taxifolia",] 
sp18mg2<-data[data$exotic.species.name=="Magallana gigas",] 
sp18pv<-data[data$exotic.species.name=="Pterois volitans",] 
sp18ce<-data[data$exotic.species.name=="Carpobrotus edulis",] 
sp18sa<-data[data$exotic.species.name=="Spartina alterniflora",] 
sp18fe<-data[data$exotic.species.name=="Ficopomatus enigmaticus",] 
sp18cf<-data[data$exotic.species.name=="Codium fragile",] 
sp18sm<-data[data$exotic.species.name=="Sargassum muticum",] 
sp18mv<-data[data$exotic.species.name=="Marenzelleria spp.",] 
sp18up<-data[data$exotic.species.name=="Undaria pinnatifida",] 
sp18dv<-data[data$exotic.species.name=="Didemnum vexillum",] 
sp18gv<-data[data$exotic.species.name=="Gracilaria vermiculophylla",] 
sp18as<-data[data$exotic.species.name=="Arcuatula senhousia",] 

# Below examples of the code used to test publication bias for two exotic species
#  Publication bias test for Carcinus maenas
a<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=sp18cm,method="ML") #p-value of intercept <0.1, assymetry
summary(a)
b<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original_refnum.z,data=sp18cm) # 1st step to remove outliers
b
c<-rstandard.rma.mv(b) 
c<-c$resid
d<-cbind(sp18cm,c)
e<-d[d$c<(3),] 
e<-e[e$c>(-3),] 
f<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=e,method="ML") 
g<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original_refnum.z,data=e)
g

# Publication bias test for Caulerpa cylindracea
a<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=sp18cc,method="ML") 
summary(a)
b<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original_refnum.z,data=sp18cc) 
b
c<-rstandard.rma.mv(b) 
c<-c$resid
d<-cbind(sp18cc,c)
e<-d[d$c<(3),] 
e<-e[e$c>(-3),] 
f<-rma(yi..Hedges.g.effect.size.,vi..Hedges.g.variance., mods = ~sqrt(vi..Hedges.g.variance.),data=e,method="ML") 
g<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=~1|original_refnum.z,data=e)
g

# In this script I am showing a few publication bias tests (e.g., for Carcinus maenas and Caulerpa cylindracea, ) as as example





# META-ANALYSES associated to the different tables and figures in the paper

# First the global analysis to get the OVERALL EFFECT of exotic species
#overall<-rma.mv(yi,vi,random=list(~1 | original_refnum.z, ~1 | exotic.taxa.species.good),data=quant12rateinc2,method="ML") 
overall<-rma.mv(yi..Hedges.g.effect.size.,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=data,method="ML") 

# Analysis for TABLE 1
table1<-rma.mv(yi..Hedges.g.effect.size.~exotic.species.name-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z),data=data,method="ML")
table1

### Analysis for FIGURE 1 
# For the analysis on Fig. 1a we removed the observations on which the trophic level of the native community could not be determined (e.g., fluxes from the "native sediment") and categorized as "unknown"
dataminus<-data1[-which(data1$native.trophic.level=="unknown"),]
fig1a<-rma.mv(yi..Hedges.g.effect.size.~native.trophic.level-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=dataminus,method="ML") 

# For analysis on Figure 1b
fig1b<-rma.mv(yi..Hedges.g.effect.size.~exotic.trophic.level-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=data2,method="ML") 

# Analysis for FIGURE 2 
# The code for the analysis of figure is in a separate R file in the github 


### Analysis for FIGURE 3
# test for the 10 categories of response variable
fig3<-rma.mv(yi..Hedges.g.effect.size.~response.variable-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=data2,method="ML") 
fig3

# test for the 3 categories of ecological complexity (identfied with three different colors in Fig. 3)
fig3_colors<-rma.mv(yi..Hedges.g.effect.size.~level.ecological.complexity-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=data2,method="ML") 
fig3_colors


### Analysis for FIGURE 4
fig4a<-rma.mv(yi..Hedges.g.effect.size.~Origen.exotic.species.Terrestrial.Marine.Freshwater-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=data,method="ML") 
fig4a

fig4b<-rma.mv(yi..Hedges.g.effect.size.~Origen.exotic.species..Mobile.Sessile-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=data,method="ML") 
fig4b

fig4c<-rma.mv(yi..Hedges.g.effect.size.~exotic.introduction.in.island.or.continent-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=data,method="ML") 
fig4c

#For figure 4d data was analyzed independently as north and south latitude
dataln<-data[which(data$latitude> 0),]
datals<-data[which(data$latitude< 0),]
fig4dn<-rma.mv(yi..Hedges.g.effect.size.~latitude,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=dataln,method="ML")
fig4dn
fig4ds<-rma.mv(yi..Hedges.g.effect.size.~latitude,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z, ~1 | exotic.species.name),data=datals,method="ML")
fig4ds

### Analysis for FIGURE 5

data19<-data[which(data$exotic.species.name=="Carcinus maenas" | data$exotic.species.name=="Caulerpa cylindracea" | data$exotic.species.name=="Lophocladia lallemandii" | data$exotic.species.name=="Mytilus galloprovincialis" | 
                     data$exotic.species.name=="Crepidula fornicata" | data$exotic.species.name=="Hemigrapsus sanguineus" | data$exotic.species.name=="Caulerpa taxifolia" | data$exotic.species.name=="Magallana gigas" |
                     data$exotic.species.name=="Pterois volitans" | data$exotic.species.name=="Carpobrotus edulis" | data$exotic.species.name=="Spartina alterniflora" | data$exotic.species.name=="Ficopomatus enigmaticus" |
                     data$exotic.species.name=="Codium fragile" | data$exotic.species.name=="Sargassum muticum" | data$exotic.species.name=="Marenzelleria spp." | data$exotic.species.name=="Undaria pinnatifida" |
                     data$exotic.species.name=="Didemnum vexillum" | data$exotic.species.name=="Gracilaria vermiculophylla" | data$exotic.species.name=="Arcuatula senhousia"),] # based on the sensitivity analysis

fig5<-rma.mv(yi..Hedges.g.effect.size.~exotic.species.name-1,vi..Hedges.g.variance.,random=list(~1 | original_refnum.z),data=data19,method="ML")
fig5









