
### load packages
library(dplyr)
library(forcats)  ## for fct_collapse  function
library("metafor")
library("Rmisc")


### read table- need to setwd() to where ever table from Anton et al. (2019) is on your computer
setwd("/Users/geraldn/Dropbox/Invasion calamity shared/Data_final/Final_csv")
dat<-read.csv("quant_FINAL_11.csv",header=T,sep=",") 
## update names
dat$exotic.trophic.level<-dat$exotic.trophic.level.new
dat$native.taxa.trophic.level<-dat$native.taxa.trophic.level.new

#############################################################################################################################
#############################################################################################################################
###### get species with greater than 3 species    names(dat)      ##################################################################################################
#############################################################################################################################
dat4<-aggregate(yi~qual_ID*exotic.taxa.species.good ,FUN=length, data=dat)##  169
dat5<-aggregate(yi~exotic.taxa.species.good ,FUN=length, data=dat4)##  75 species
dat6<-dat5[dat5$yi>2,]   ##18 species
names(dat6)[2]<-paste("number_studies") 
# limit orignial dat
datgr3<-dat[dat$exotic.taxa.species.good %in% dat6$exotic.taxa.species.good,]
datgr3$exotic.taxa.species.good<-factor(datgr3$exotic.taxa.species.good)
## get unique species taxa group table
sptaxa <- datgr3 %>% 
  select(exotic.taxa.species.good,exotic.taxa.group.new) %>% 
  mutate(sp_taxa=paste(exotic.taxa.species.good,exotic.taxa.group.new)) %>% 
  filter(!duplicated(sp_taxa))

#   levels(datgr3$exotic.taxa.species.good)
## !!! from dat2 ------ only four variables  !!!!!!!!!!!!!!!!!!!!!
dat2gr3<-dat2[dat2$exotic.taxa.species.good %in% dat6$exotic.taxa.species.good,]
dat2gr3$exotic.taxa.species.good<-factor(dat2gr3$exotic.taxa.species.good)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
###################### PLOT speceis with more 3 studies- only all  #########################################
#############################################################################################################################
# get the order of species based on mean from meta-analysis analysis - rma.mv
x1r1<-rma.mv(yi~datgr3$exotic.taxa.species.good-1,vi,random=~factor(original_refnum.z),data=datgr3,method="ML") 
ll<-data.frame(levels(datgr3$exotic.taxa.species.good),as.numeric(x1r1[[1]]))
names(ll)[1]<-paste("exotic.taxa.species.good") 
names(ll)[2]<-paste("meany") 
lll<-ll[order(ll$meany, decreasing=T),] ## 
datgr3$exotic.taxa.species.good<-factor(datgr3$exotic.taxa.species.good,levels=as.character(lll$exotic.taxa.species.good))
# rerun model with correct level order
m<-rma.mv(yi~datgr3$exotic.taxa.species.good-1,vi,random=~factor(original_refnum.z),data=datgr3,method="ML") 
#############################################################
# start setting variables for plot
nn<-aggregate(yi~exotic.taxa.species.good ,FUN=length, data=datgr3)#
studies<-aggregate(datgr3$original_refnum.z~exotic.taxa.species.good ,FUN = function(x) length(unique(x)), data=datgr3)
mmean<-aggregate(yi~exotic.taxa.species.good ,FUN=mean, data=datgr3)#
lab<-levels(datgr3$exotic.taxa.species.good)
n<-c(1:length(lab))##row # of y levels 
xuni<-0.7  #universal for x location of top legends
xl<-c(-1.5,1.5)
yl<-c(0.7,length(lab)+1)
xpos<-1.2 # set locations for n and sig
xpos2<-1.82
pvals <- m[[5]]   # Extract the p-values
# Use the symnum function to produce the symbols
sigS <- symnum(pvals, na = FALSE, legend = FALSE,
               cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
               symbols = c("***", "**", "*", " "))
# col
invcol<-"red3"
exotneg<-"grey20"
exotpos<-"grey20"   #"grey40"
sslab<-c(15,19,21)
lll$lab_col<-exotneg
lll$sigp<-pvals
lll$lab_col[lll$meany>0]<-exotpos
lll$lab_col[lll$sigp<(0.05)]<-invcol
lll$lab_shap<-sslab[2]
lll$lab_shap[lll$sigp<(0.05)]<-sslab[1]
lll$lab_shap[lll$meany>(-0.0)]<-sslab[3]
mes<-(m$ci.ub-m$ci.lb)/2
lll$mod<-paste(format(m$b[n],digits=1),"±",format(mes,digits=2)," "
                ,sigS, sep="")

lll$stud<-paste("(",studies[,2],",",nn[,2],")",sep="")
vlab<-rev(c("Significant effect","Non-significant effect, mean g<0","Non-significant effect, mean g>0")) 
cclab<-rev(c(invcol,exotneg,exotpos)) 

 #  names(im)  
lll2<-lll %>% 
  mutate(picy=(n/((max(n)-min(n))+spred))+shiftnum) %>% 
  left_join(sptaxa[,c(1,2)]) %>% 
  left_join(im[,c(1,2,4,6)], by=c("exotic.taxa.group.new"="taxonomic.group")) %>%  # unique(lll$exo_pred_group_2) unique(im$taxonomic.group) 
  mutate(stagger=rep(c(xxx,xxx-0.075),length.out=length(n)))
#############################################################################
############################################################################
######
# with out pics
par(mfrow=c(1,1),mar=c(2.0,12,4.5,7.7),oma=c(1,0,1,1),cex=1.2, xpd=NA)
## set x and y for plots
x<-m$b[n]
xmean<-mean(x)
y<-n
ci<-m$b[n]-m$ci.lb[n]
plot(x,y,xlab=" ",ylab=" ",xlim=xl,ylim=yl,axes=F,pch=lll$lab_shap,cex=2.3,col=lll$lab_col)
#text(-0.28,length(lab)+1," ",cex=1.3) # y labesl
nnnn<-0.2
text(xpos+nnnn,n,lll$stud,cex=1, pos=4) # number of studies and obs
text(xpos2+nnnn,n,lll$mod,cex=1, pos=4) # mean ci pvalue
#
text(-3.85,n,lab, col=lll$lab_col,cex=1,font=3,xpd=NA, pos=4)  # y labels pos=2 left of corr  pos=4 rith of point
axis(2,las=2,labels=F,at=n,font=3)
#axis(2,las=2,labels=lab,font.axis=3,at=n)
axis(1, las=1, pos=0.75) # pos is coordinate of axis
ablineclip(v = xmean, y1 = .75,y2 = length(lab)+0.5,lty = 2)    # line at mean
ablineclip(v = 0, y1 = .75,y2 = length(lab)+0.5,lty = 1)   # line at 0
error.x(x,y,ci,col=lll$lab_col)
points(x,y,pch=lll$lab_shap,cex=2.3,col=lll$lab_col, bg="white")## replot points
legend(0.2-xuni,yl[2]+3.5,legend=rev(vlab),col=rev(cclab),cex=1.2,bty="n",ncol=1,
       xpd=T,y.intersp=.75,x.intersp=.5,pch=sslab)
mtext(expression("Effect size"~italic('g')), side=1, font=1,outer=TRUE,line=-1.2
      ,cex=1.3, at=.57)
#### legends    ############################################################ 
# pics added in other software
legend(xl[1]-1.95-xuni,yl[2]+3.5,legend=im$taxonomic.group[1:4],col=exotneg,cex=1.2,bty="n",
       ncol=1,xpd=T,y.intersp=.8) # or .7?
legend(xl[1]-0.3-xuni,yl[2]+3.5,legend=im$taxonomic.group[5:7],col=exotneg,cex=1.2,bty="n",
       ncol=1,xpd=T,y.intersp=.8)
#save as PDF  height  device size 8.3 x 13       18_worst_aug_18
### !!!!  save as PDF  height  device size 7 x 10     !!!!!!!!!!!
# full height of labtop screen x above downloads
#   with pics
setwd("/Users/geraldn/Dropbox/Invasion calamity shared/Data_final/plots") 
dev.copy(pdf,'18_worst_mar19_nopics.pdf',width=8.6,height=13)  # ,width=8.6,height=13
dev.off()
