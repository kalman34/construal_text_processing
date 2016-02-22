
# SR SF ingroup

# kal test git version control

#### set up ####
setwd("~/Google_Drive/Kal Higgins docs/R analyses SRC")

src <- read.csv(file="Construal_SR_1_160210.csv")

library(psych)
library(ggplot2)
theme_set(theme_bw(base_size = 14)) 
library(rockchalk)

####### EXCLUSIONS ######

# 162 completed the study

# failed attn1 (survey): at 158
src <- src[ which(src$attn1 == "reading") , ]

# expressed suspicion about partner's existence in answer to "would you work with partner again": at 154
src <- src[ which(is.na(src$suspchk) == TRUE ) , ] 

# seen description of michael before: 151
src <- src[ which(src$prevmich == 2 ) , ]

# copied description of michael: 147
src <- src[ which(src$copied == 2 ) , ]

# wrote 'Michael' in their message: at 140
src <- src[ which(is.na(src$usedmic) == TRUE ) , ]

# final count: 140 participants
#write.csv(src, file = "Construal_SR_1_151025.excl.csv")


#src <- read.csv(file="Construal_SR_1_151025.excl.csv")

#exclude plagiarized = at 139
#src <- src[ which(src$plag == 0) , ]


#### COMPUTING ####
attach(src)
## rmq
src$loc <- (rmq_1 + rmq_3 + rmq_4 + rmq_5 + rmq_8 + rmq_16 + rmq_21 + rmq_25 + rmq_28 + rmq_29 + (7-rmq_13) + (7-rmq_24))/12
src$ass <- ((7-rmq_2) + rmq_6 + rmq_7 + rmq_9 + (7-rmq_10) + rmq_11 + rmq_15 + rmq_19 + rmq_20 + rmq_22 + (7-rmq_27) + rmq_30)/12
src$locomassess = src$loc-src$ass

#rfq
src$prom = (6-rfq_1) + rfq_3 + rfq_7 + (6-rfq_9) + rfq_10 + (6-rfq_11)
src$prev = (6-rfq_2) + (6-rfq_4) + rfq_5 + (6-rfq_6) + (6-rfq_8)
src$prommprev = src$prom-src$prev

# closeness measure
src$closeness <- (((8-close_1) + (8-close_2) + close_3 + close_4 + (8-close_5) + (8-close_6) + close_7 + close_8)/8)

# ntoBelong
src$belong = ((6-belong_1) + (6-belong_3) + (6-belong_7) + belong_2 + belong_4 + belong_5 + belong_6 + belong_8 + belong_9 + belong_10)/10

# Relational Trust
src$reltrust = (reltrust_1 + reltrust_2 + reltrust_3 + reltrust_4)/4

# Epistemic Trust
src$epistrust = (epistrust_1 + epistrust_2 + epistrust_3 + epistrust_4)/4

# message trust
src$messtrust = (messtrust_1 + messtrust_2 + messtrust_3 + messtrust_4)/4

#rapport
#src$rappos <- (comfort + friendly + harmony + satisf + pos)/5

# need for closure
src$nfclose <- (nfc_1 + nfc_2 +  nfc_3 +  nfc_4 +	nfc_5	+ nfc_6	+ nfc_7 +	nfc_8 +	nfc_9	+ nfc_10+	nfc_11 +	nfc_12 +	nfc_13 +	nfc_14 +	nfc_15)/15


# valence change
src$vchange <- src$vrecall-src$vmessage

detach(src)

src$ingroup <- src$DO.BR.FL_57
src$dislike <- src$DO.BR.FL_63

src$vmessage<-as.numeric(src$vmessage)
src$vrecall<-as.numeric(src$vrecall)


srlike <- subset(src, dislike=="like")
srdislike <- subset(src, dislike=="dislike")
sringroup <- subset(src, ingroup=="ingroup")
sroutgroup <- subset(src, ingroup=="outgroup")

#PYTHON CONSTRUAL ANALYSES FEB 10, 2016#

summary(aov(const_summ ~ dislike*ingroup, data=src))
summary(lm(const_summ ~ dislike*ingroup, data=src))
summary(lm(const_summ ~ dislike+ingroup, data=src))

summary(lm(const_sumr ~ dislike*ingroup, data=src))
summary(lm(const_sumr ~ dislike+ingroup, data=src))



#Subset analyses Dec 22#

srlike_ingroup <- subset(srlike, ingroup=="ingroup")
srdislike_ingroup <- subset(srdislike, ingroup=="ingroup")

srlike_outgroup <- subset(srlike, ingroup=="outgroup")
srdislike_outgroup <- subset(srdislike, ingroup=="outgroup")

r_tempdist <- (rpresent - (rpast + rfuture))
m_tempdist<- (mpresent - (mpast + mfuture))

r_socialclose<- (rsocial + rfriend)
m_socialclose<- (msocial + mfriend)


cor.test(srlike_ingroup$vmessage, srlike_ingroup$vrecall, data=srlike_ingroup)
cor.test(srdislike_ingroup$vmessage, srdislike_ingroup$vrecall, data=srdislike_ingroup) 

cor.test(srlike_outgroup$vmessage, srlike_outgroup$vrecall, data=srlike_outgroup) 
cor.test(srdislike_outgroup$vmessage, srdislike_outgroup$vrecall, data=srdislike_outgroup)

ggplot(src, aes(vmessage, vrecall)) + geom_point(shape=1) + geom_smooth(method=lm)

#Closeness and Cells
cor.test(srlike_ingroup$vchange, srlike_ingroup$closeness, data=srlike_ingroup)
cor.test(srdislike_ingroup$vchange, srdislike_ingroup$closeness, data=srdislike_ingroup) 

cor.test(srlike_outgroup$vchange, srlike_outgroup$closeness, data=srlike_outgroup) 
cor.test(srdislike_outgroup$vchange, srdislike_outgroup$closeness, data=srdislike_outgroup)

cor.test(srlike_ingroup$vmessage, srlike_ingroup$closeness, data=srlike_ingroup)
cor.test(srdislike_ingroup$vmessage, srdislike_ingroup$closeness, data=srdislike_ingroup) 

cor.test(srlike_outgroup$vmessage, srlike_outgroup$closeness, data=srlike_outgroup) 
cor.test(srdislike_outgroup$vmessage, srdislike_outgroup$closeness, data=srdislike_outgroup)

cor.test(srlike_ingroup$vrecall, srlike_ingroup$closeness, data=srlike_ingroup)
cor.test(srdislike_ingroup$vrecall, srdislike_ingroup$closeness, data=srdislike_ingroup) 

cor.test(srlike_outgroup$vrecall, srlike_outgroup$closeness, data=srlike_outgroup) 
cor.test(srdislike_outgroup$vrecall, srdislike_outgroup$closeness, data=srdislike_outgroup)


#LIWC

cor.test(srlike_ingroup$m_socialclose, srlike_ingroup$closeness, data=srlike_ingroup)
cor.test(srdislike_ingroup$m_socialclose, srdislike_ingroup$closeness, data=srdislike_ingroup)

cor.test(srlike_outgroup$m_socialclose, srlike_outgroup$closeness, data=srlike_outgroup) 
cor.test(srdislike_outgroup$m_socialclose, srdislike_outgroup$closeness, data=srdislike_outgroup)


cor.test(srlike_ingroup$mfriend, srlike_ingroup$closeness, data=srlike_ingroup)
cor.test(srdislike_ingroup$mfriend, srdislike_ingroup$closeness, data=srdislike_ingroup) 

cor.test(srlike_outgroup$mfriend, srlike_outgroup$closeness, data=srlike_outgroup) 
cor.test(srdislike_outgroup$mfriend, srdislike_outgroup$closeness, data=srdislike_outgroup)

cor.test(srlike_ingroup$msocial, srlike_ingroup$closeness, data=srlike_ingroup)
cor.test(srdislike_ingroup$msocial, srdislike_ingroup$closeness, data=srdislike_ingroup) 

cor.test(srlike_outgroup$msocial, srlike_outgroup$closeness, data=srlike_outgroup) 
cor.test(srdislike_outgroup$msocial, srdislike_outgroup$closeness, data=srdislike_outgroup)


###### ANALYSIS ######

#cor recall and message by group#

cor.test(sringroup$vmessage, sringroup$vrecall) # 0.3053025 

ggplot(src, aes(vmessage, vrecall)) + geom_point(shape=1) + geom_smooth(method=lm)

cor.test(sroutgroup$vmessage, sroutgroup$vrecall) #.497117 

ggplot(src, aes(sroutgroup$vmessage, sroutgroup$vrecall)) + geom_point(shape=1) + geom_smooth(method=lm)

### ATTITUDES

#MESSAGE

summary(aov(vmessage ~ dislike, data=src))

summary(aov(vmessage ~ dislike*ingroup, data=src))

ggplot(src, aes(dislike, vmessage)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  facet_wrap(~ingroup) + #makes two different graphs for the different groups
  ggtitle ("Message Valence")

summary(aov(vmessage ~ dislike*ingroup, data=src))
model<-(aov(vmessage ~ dislike*ingroup, data=src)) 
TukeyHSD(model)


# RECALL

summary(aov(vrecall ~ dislike, data=src))

summary(aov(vrecall ~ dislike*ingroup, data=src))

ggplot(src, aes(dislike, vrecall)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  facet_wrap(~ingroup) + #makes two different graphs for the different groups
  ggtitle ("Recall Valence")

model<-(aov(vrecall ~ dislike*ingroup, data=src))
TukeyHSD(model)

#CHANGE

summary(aov(vchange ~ dislike, data=src))

summary(aov(vchange ~ dislike*ingroup, data=src))

ggplot(src, aes(dislike, vchange)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  facet_wrap(~ingroup) + #makes two different graphs for the different groups
  ggtitle ("Valence Change")

TukeyHSD(aov(vchange ~ dislike*ingroup, data=src))

summary(lm(vchange ~ closeness, data=src))


### INTERPERSONAL

# EPISTEMIC TRUST

summary(aov(epistrust ~ dislike, data=src)) #not sig

summary(aov(epistrust ~ ingroup, data=src)) #not sig

summary(aov(epistrust ~ dislike*ingroup, data=src)) #not sig

ggplot(src, aes(dislike, epistrust)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  facet_wrap(~ingroup) + #makes two different graphs for the different groups
  ggtitle ("Epistemic trust")

# RELATIONAL TRUST 

summary(aov(reltrust ~ dislike, data=src))

summary(aov(reltrust ~ ingroup, data=src))

summary(aov(reltrust ~ dislike*ingroup, data=src))

ggplot(src, aes(dislike, reltrust)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  ggtitle ("Relational trust")

# CLOSENESS

summary(aov(closeness ~ dislike, data=src)) #sig 

summary(aov(closeness ~ ingroup, data=src)) #trend

summary(aov(closeness ~ dislike*ingroup, data=src))

ggplot(src, aes(dislike, closeness)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  ggtitle ("Closeness")

ggplot(src, aes(ingroup, closeness)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  ggtitle ("Closeness")


ggplot(src, aes(dislike, closeness)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  facet_wrap(~ingroup)
ggtitle ("Closeness")

ggplot(src, aes(ingroup, closeness)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  facet_wrap(~dislike)
ggtitle ("Closeness")

#NEED TO BELONG#

summary(aov(belong ~ dislike, data=src)) 

summary(aov(belong ~ ingroup, data=src)) 

summary(aov(belong ~ dislike*ingroup, data=src))

ggplot(src, aes(dislike, belong)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  ggtitle ("Need to belong")

#RAPPORT# STILL NEED TO CALIBRATE/CALCULATE

summary(aov(Rapport ~ dislike, data=src)) 

summary(aov(closeness ~ ingroup, data=src)) 

summary(aov(closeness ~ dislike*ingroup, data=src))

ggplot(src, aes(dislike, closeness)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
  theme(legend.position='none') +
  ggtitle ("Rapport")


####___##### HAVEN'T REALLY RUN THESE YET AS FAR AS I REMEMBER

# audience tuning
summary(aov(vmessage ~ dislike, data=src)) # sig

# audience tuning
summary(aov(vrecall ~ dislike, data=src)) # sig

summary(lm(vrecall ~ dislike + vmessage, data=src)) # sig

summary(lm(vrecall ~ dislike + vmessage + closeness, data=src)) # sig

summary(lm(vrecall ~ dislike + vmessage + closeness, data=srfailure)) # sig

summary(lm(vrecall ~ dislike + vmessage + closeness, data=srsuccess)) # sig

summary(lm(vrecall ~ dislike + vmessage + messtrust + ingroup, data=src)) # sig

summary(lm(vrecall ~ dislike + vmessage + messtrust_1 + vmessage * messtrust_1, data=src)) # sig
summary(lm(vrecall ~ dislike + vmessage + messtrust_1, data=src)) # sig
summary(lm(vrecall ~ dislike + vmessage * messtrust_1, data=src)) # sig

summary(lm(vrecall ~ vmessage + messtrust_1, data=srlike)) # sig
summary(lm(vrecall ~ vmessage + messtrust_1, data=srdislike)) # sig

summary(lm(vrecall ~ vmessage * messtrust_1, data=srlike)) # sig
summary(lm(vrecall ~ vmessage * messtrust_1, data=srdislike)) # sig

summary(lm(vrecall ~ messtrust_1, data=srlike)) # sig
summary(lm(vrecall ~  messtrust_1, data=srdislike)) # sig

summary(lm(vrecall ~ dislike + vmessage + messtrust, data=srfailure)) # sig

summary(lm(vrecall ~ dislike + vmessage + messtrust, data=srsuccess)) # sig

summary(lm(vrecall ~ dislike  + messtrust, data=src)) # sig

summary(lm(vrecall ~ dislike +  messtrust, data=srfailure)) # sig

summary(lm(vrecall ~ dislike +  messtrust, data=srsuccess)) # sig

summary(lm(vrecall ~ dislike *  messtrust, data=src) 
        
        ####_______________##INTERACTIONS ####
        
        # audience tuning interaction?#
        summary(aov(vrecall ~ dislike*ingroup, data=src)) # ns
        
        # audience tuning interaction?#
        summary(aov(vchange ~ dislike*ingroup, data=src)) # ns
        
        theme_set(theme_black(base_size = 24)) 
        
        ggplot(src, aes(dislike, vmessage)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) +
          ggtitle ("Message Valence")
        
        ggplot(src, aes(dislike, vrecall)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) +
          ggtitle ("Recall Valence")
        
        ggplot(src, aes(dislike, vchange)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) +
          ggtitle ("Valence Change") #INTERACTIONS--
        
        
        ##### INTERPERSONAL ####
        
        #install.packages(ggplot2)
        library(ggplot2)
        theme_set(theme_bw(base_size = 14
        ))
        
        mainef <- function(x){
          sr <- aov(x ~ ingroup, data=src)
          summary(sr)
        }
        
        mainef(src$closeness) # marginal effect, outgroup > ingroup
        
        ggplot(src, aes(ingroup, closeness)) +
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          stat_summary(fun.data=mean_se, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          theme(legend.position='none') 
        
        mainef(src$rappos) # 
        ggplot(src, aes(ingroup, rappos)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        ####_______####
        
        
        mainef(src$rapneg) # 
        ggplot(src, aes(ingroup, rapneg)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        
        mainef(src$futwrk) # NS EFFECT ON DESIRE TO WORK WITH PARTNER AGAIN
        ggplot(src, aes(ingroup, futwrk)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(color=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(color=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        
        mainef(src$reltrust) # NS
        ggplot(src, aes(ingroup, reltrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        mainef(src$epistrust) # NS
        ggplot(src, aes(ingroup, epistrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        mainef(src$messtrust) # NS 
        ggplot(src, aes(ingroup, messtrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        
        # effect of liking
        mainef <- function(x){
          sr <- aov(x ~ dislike, data=src)
          summary(sr)
        }
        
        summary(aov(mWC ~ dislike, data=src))
        
        ggplot(src, aes(dislike, mWC)) +
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          stat_summary(fun.data=mean_se, geom='errorbar') +
          geom_jitter(aes(color=dislike), size=2, position = position_jitter(width = .2)) + 
          theme(legend.position='none') 
        
        mainef(src$closeness) # sig dislike > like
        
        ggplot(src, aes(dislike, closeness)) +
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          stat_summary(fun.data=mean_se, geom='errorbar') +
          geom_jitter(aes(color=dislike), size=2, position = position_jitter(width = .2)) + 
          theme(legend.position='none') 
        
        mainef(src$futwrk) # sig dislike > like
        ggplot(src, aes(ingroup, futwrk)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(color=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(color=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        
        mainef(src$reltrust) # sig dislike > like
        ggplot(src, aes(ingroup, reltrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        mainef(src$epistrust) # NS
        ggplot(src, aes(ingroup, epistrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        mainef(src$messtrust) # NS 
        ggplot(src, aes(ingroup, messtrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        
        
        ### interactions
        
        mainef <- function(x){
          sr <- aov(x ~ ingroup*dislike, data=src)
          summary(sr)
        }
        
        ##### mainef(src$closeness) # marginal effect, outgroup > ingroup ##### 
        
        ggplot(src, aes(ingroup, closeness)) +
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          stat_summary(fun.data=mean_se, geom='errorbar') +
          geom_jitter(aes(color=ingroup), size=2, position = position_jitter(width = .2)) + 
          theme(legend.position='none') +
          facet_wrap(src$dislike)
        
        mainef(src$rappos) # 
        ggplot(src, aes(ingroup, rappos)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        mainef(src$rapneg) # 
        ggplot(src, aes(ingroup, rapneg)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        
        mainef(src$futwrk) # NS EFFECT ON DESIRE TO WORK WITH PARTNER AGAIN
        ggplot(src, aes(ingroup, futwrk)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(color=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(color=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        
        mainef(src$reltrust) # NS
        ggplot(src, aes(ingroup, reltrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        mainef(src$epistrust) # NS
        ggplot(src, aes(ingroup, epistrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') 
        
        mainef(src$messtrust) # NS 
        ggplot(src, aes(ingroup, messtrust)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=ingroup), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=ingroup), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none')  #MAIN EF STUFF--DID WE RUN THESE?
        
        #Liking of Michael (likem)#
        
        summary(aov(likem_2 ~ dislike*ingroup, data=src))
        
        ggplot(src, aes(dislike, likem_2)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup)
        ggtitle ("Liking of Michael")
        
        ggplot(src, aes(dislike, likem_2)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          ggtitle ("Liking of Michael")
        
        ggplot(src, aes(dislike, likem_2)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup)
        ggtitle ("Liking of Michael")  
        
        summary(aov(plikem_2 ~ dislike*ingroup, data=src))
        ggplot(src, aes(dislike, plikem_2)) +
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup)
        ggtitle ("Audience Liking of Michael")
        
        ggplot(src, aes(dislike, plikem_2)) +
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          ggtitle ("Audience Liking of Michael")
        
        #SIMILARITY#
        
        summary(aov(sim_1 ~ dislike*ingroup, data=src))
        
        ggplot(src, aes (dislike, sim_1)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup)
        ggtitle ("similarity to partner")
        
        ggplot(src, aes (dislike, sim_1)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          ggtitle ("similarity to partner")
        #####LIWC Analyses#####
        
        colnames(src)
        
        #CUSTOM LIWC VARIABLES
        
        attach(src)
        
        src$r_tempdist <- (rpresent - (rpast + rfuture))
        src$m_tempdist<- (mpresent - (mpast + mfuture))
        
        src$r_socialclose<- (rsocial + rfriend)
        src$m_socialclose<- (msocial + mfriend)
        
        detach(src)
        
        #ANOVA for #
        summary(aov(mrelativ ~ dislike, data=src))
        
        summary(aov(mrelativ ~ ingroup, data=src))
        
        summary(aov(mrelativ ~ dislike*ingroup, data=src))
        
        
        summary(aov(mpast ~ dislike, data=src))
        
        summary(aov(mpast ~ ingroup, data=src))
        
        summary(aov(mpast ~ dislike*ingroup, data=src))
        
        
        summary(aov(mfuture ~ dislike, data=src))
        
        summary(aov(mfuture ~ ingroup, data=src))
        
        summary(aov(mfuture ~ dislike*ingroup, data=src))
        
        
        summary(aov(msocial ~ dislike, data=src))
        summary(lm(msocial ~ dislike, data=src))
        
        summary(aov(msocial ~ ingroup, data=src))
        
        summary(aov(msocial ~ dislike*ingroup, data=src))
        
        ggplot(src, aes(dislike, msocial)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) + #makes two different graphs for the different groups
          ggtitle ("Social words for message")
        
        summary(aov(mfriend ~ dislike, data=src))
        
        summary(aov(mfriend ~ ingroup, data=src))
        
        summary(aov(mfriend ~ dislike*ingroup, data=src))
        
        
        summary(aov(mfriend ~ dislike, data=src))
        
        summary(aov(mfriend ~ ingroup, data=src))
        
        summary(aov(mfriend ~ dislike*ingroup, data=src))
        
        ggplot(src, aes(dislike, mfriend)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) + #makes two different graphs for the different groups
          ggtitle ("Friend words for message")
        
        summary(aov(m_socialclose ~ dislike, data=src))
        
        summary(aov(m_socialclose ~ ingroup, data=src))
        
        summary(aov(m_socialclose ~ dislike*ingroup, data=src))
        
        ggplot(src, aes(dislike, m_socialclose)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) + #makes two different graphs for the different groups
          ggtitle ("Social + friend words for message")
        
        
        summary(aov(m_tempdist ~ dislike, data=src))
        
        summary(aov(m_tempdist ~ ingroup, data=src))
        
        summary(aov(m_tempdist ~ dislike*ingroup, data=src))
        
        summary(lm(m_socialclose ~ closeness, data = src))
        
        summary(lm(closeness ~ m_socialclose, data = src))
        
        ggplot(src, aes(m_socialclose, closeness)) +
          geom_jitter(size=2, position = position_jitter(width = .2), color="black") + 
          geom_smooth(method=lm,color="black",size=1) +
          labs(x="m_socialclose", y= "closeness") +
          theme(legend.position='none', 
                axis.title.x = element_text(face="bold", vjust=-0.25),
                axis.title.y = element_text(face="bold", vjust=0.8),
                axis.text.x = element_text(face="bold", vjust=-.8), axis.text.y = element_text(face="bold")) +
          ggtitle('Closeness on social closeness words')
        
        ggplot(src, aes(closeness, m_socialclose)) +
          geom_jitter(size=2, position = position_jitter(width = .2), color="black") + 
          geom_smooth(method=lm,color="black",size=1) +
          labs(x="closeness", y= "m_socialclose") +
          theme(legend.position='none', 
                axis.title.x = element_text(face="bold", vjust=-0.25),
                axis.title.y = element_text(face="bold", vjust=0.8),
                axis.text.x = element_text(face="bold", vjust=-.8), axis.text.y = element_text(face="bold")) +
          ggtitle('Social closeness words on closeness')
        
        ggplot(src, aes(dislike, vmessage)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) + #makes two different graphs for the different groups
          ggtitle ("Message Valence")
        
        # RECALL FOR LIWC 
        
        summary(aov(rrelativ ~ dislike, data=src))
        
        summary(aov(rrelativ ~ ingroup, data=src))
        
        summary(aov(rrelativ ~ dislike*ingroup, data=src))
        
        
        summary(aov(rpast ~ dislike, data=src))
        
        summary(aov(rpast ~ ingroup, data=src))
        
        summary(aov(rpast ~ dislike*ingroup, data=src))
        
        
        summary(aov(rfuture ~ dislike, data=src))
        
        summary(aov(rfuture ~ ingroup, data=src))
        
        summary(aov(rfuture ~ dislike*ingroup, data=src))
        
        
        summary(aov(rsocial ~ dislike, data=src))
        
        summary(aov(rsocial ~ ingroup, data=src))
        
        summary(aov(rsocial ~ dislike*ingroup, data=src))
        
        
        summary(aov(r_socialclose ~ dislike, data=src))
        
        summary(aov(r_socialclose ~ ingroup, data=src))
        
        summary(aov(r_socialclose ~ dislike*ingroup, data=src))
        
        
        summary(aov(r_tempdist ~ dislike, data=src))
        
        summary(aov(r_tempdist ~ ingroup, data=src))
        
        summary(aov(r_tempdist ~ dislike*ingroup, data=src))
        
        summary(lm(r_socialclose ~ closeness, data = src))
        
        ggplot(src, aes(dislike, rsocial)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) + #makes two different graphs for the different groups
          ggtitle ("Social words for recall")
        
        #word count for LIWC
        summary(aov(mWC ~ dislike*ingroup, data=src))
        summary(aov(rWC ~ dislike*ingroup, data=src))
        
        summary(lm(mWC ~ dislike, data = src))
        
        summary(lm(rWC ~ dislike, data = src))
        
        
        #timer
        summary(aov(timer_2.2 ~ dislike*ingroup, data=src))
        summary(aov(timer_2.5 ~ dislike*ingroup, data=src))
        
        summary(lm(timer_2.2 ~ dislike, data = src))
        
        summary(lm(timer_2.2 ~ closeness, data = src))
        
        #CHANGE
        
        summary(aov(vchange ~ dislike, data=src))
        
        summary(aov(vchange ~ dislike*ingroup, data=src))
        
        ggplot(src, aes(dislike, vchange)) + 
          stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
          geom_jitter(aes(shape=dislike), size=2, position = position_jitter(width = .2)) + 
          stat_summary(aes(group=dislike), fun.y = mean, geom="point", size=3) +
          theme(legend.position='none') +
          facet_wrap(~ingroup) + #makes two different graphs for the different groups
          ggtitle ("Valence Change")
        
        TukeyHSD(aov(vchange ~ dislike*ingroup, data=src))
        
        lm(formula = vchange ~ ingroup + dislike, data = src)
        
        summary(lm(formula = m_socialclose ~ closeness + dislike, data = src))
        
        ggplot(src, aes(ingroup + dislike, vchange)) +
          geom_jitter(size=2, position = position_jitter(width = .2), color="black") + 
          geom_smooth(method=lm,color="black",size=1) +
          labs(x="closeness", y= "m_socialclose") +
          theme(legend.position='none', 
                axis.title.x = element_text(face="bold", vjust=-0.25),
                axis.title.y = element_text(face="bold", vjust=0.8),
                axis.text.x = element_text(face="bold", vjust=-.8), axis.text.y = element_text(face="bold")) +
          ggtitle('Interaction between dislike and valence change')
        
        #### STOPPED HERE AS OF WED DEC 2
        # mediating effect of epis & reltrust on closeness, rappos, rapneg
        
        summary(lm(closeness ~ ingroup + epistrust, data=src)) # ingroup drops to ns, epis sig
        summary(lm(closeness ~ ingroup + reltrust, data=src)) # ingroup drops to ns, rel sig
        summary(lm(epistrust ~ ingroup + closeness, data=src)) # both sig
        summary(lm(reltrust ~ ingroup + closeness, data=src)) # both sig
        
        summary(lm(rappos ~ ingroup + epistrust, data=src)) # ingroup drops to ns, epis sig
        summary(lm(rappos ~ ingroup + reltrust, data=src)) # ingroup drops to ns, rel sig
        summary(lm(epistrust ~ ingroup + rappos, data=src)) # both sig
        summary(lm(reltrust ~ ingroup + rappos, data=src)) # both sig
        
        summary(lm(rapneg ~ ingroup + epistrust, data=src)) # ingroup drops to ns, epis sig
        summary(lm(rapneg ~ ingroup + reltrust, data=src)) # ingroup drops to ns, rel sig
        summary(lm(epistrust ~ ingroup + rapneg, data=src)) # both sig
        summary(lm(reltrust ~ ingroup + rapneg, data=src)) # both sig
        
        mainef(src$mood) # NO EFFECT ON MOOD
        
        
        mainef(src$belong) # NO EFFECT ON BELONGING
        mainef(src$nfclose) # NO EFFECT ON NFCLOSE
        mainef(src$ass)
        mainef(src$loc)
        mainef(src$prom)
        mainef(src$prev)
        
        ###### MEDIATION ANALYSIS ######
        
        install.packages("boot")
        library(boot)   			#Accesses the R package called "boot" that contains
        #specialized bootstrapping functions.
        
        
        #Function to calculate a, b, ab, c, c'
        abfun <- function(srm, d)  {
          
          E<- srm[d,]			# allows boot to select sample
          fit.0 <- lm(closeness  ~ ingroup , data=E)		#regression to estimate c
          fit.1 <- lm(trust  ~ ingroup , data=E)		#regression to estimate a
          fit.2 <- lm(closeness  ~ ingroup + trust , data=E)	#regression to estimate b, c'
          ccoef<-coef(fit.0)			#pull coef c from regression output
          acoef<-coef(fit.1)			#pull coef a from regression output
          bcoef<-coef(fit.2)			#pull coef b, c' from regression output
          ab<-acoef[2]*bcoef[3]		#calc ab
          med<-c(acoef[2], bcoef[3], ab, ccoef[2], bcoef[2]) 		#create vector: a,b,ab,c',c
          return(med)					#output med to the boot function
          
        }
        
        # bootstrapping with R=1000 replications; only use R=1000 when testing the script; then, when you have it working properly, use R=100000
        abboots <- boot(data=srm, statistic=abfun,  R=10000)
        
        
        #Print the path coefficients from the original sample
        Coefficient<-c("a","b","ab","c","c'")
        NormalEsts<- data.frame(Coefficient, abboots$t0)
        names(NormalEsts)[c(2)] <- c("Value")
        NormalEsts
        
        
        # Get a sequence of confidence intervals for ab, from 50% to 99.9%; NOTE: R
        # calculates an ever-widening sequence of confidence intervals. At some point in
        # the sequence an interval will include zero. To find out the obtained p-value:
        # (1) find the first confidence interval in the sequence that includes zero; (2)
        # count back one interval; (3) subtract that interval's % from 100%. Example: If
        # the first confidence interval to include zero is the 98% interval, then the
        # obtained p-value is 100-97=3% or p=.03.
        cl<-c(0.5,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,0.991,0.992,0.993,0.994,0.995,0.996,0.997,0.998,0.999)
        
        
        #Examine means, variances, covariance of a, b estimates
        plot(abboots$t[,1], abboots$t[,2])
        cor(abboots$t[,1], abboots$t[,2])
        cov(abboots$t[,1], abboots$t[,2])
        mean(abboots$t[, 1])
        mean(abboots$t[, 2])
        mean(abboots$t[, 1])*mean(abboots$t[, 2])
        mean(abboots$t[, 3]) #ab products
        
        mean(abboots$t[, 1])*mean(abboots$t[, 2]) +  cov(abboots$t[,1], abboots$t[,2]) #mean(a)*mean(b) plus their covariance
        
        
        # get 95% confidence intervals
        abci<-boot.ci(abboots, conf=cl, type=c("perc","bca"), index=3)  #Calc Percentile and BCA CIs for ab
        abci				#Show the output of boot.ci
        
        # Filled Density Plots of a,b,a*b,c,c'
        den1 <- density(abboots$t[,1])
        den2 <- density(abboots$t[,2])
        den3 <- density(abboots$t[,3])
        den4 <- density(abboots$t[,4])
        den5 <- density(abboots$t[,5])
        
        par(mar=par("mar")/2)
        # 5 figures arranged in 3 rows and 3 columns
        layout(matrix(c(1,2,3,3,4,5), 3, 2, byrow = TRUE))
        plot(den1, main="a")
        polygon(den1, col="red", border="blue")
        plot(den2, main="b")
        polygon(den2, col="red", border="blue")
        plot(den3, main="ab")
        polygon(den3, col="red", border="blue")
        plot(den4, main="c")
        polygon(den4, col="red", border="blue")
        plot(den5, main="c'")
        polygon(den5, col="red", border="blue")
        mtext("Kernel Density Plots", side=3, outer=TRUE, line=-2)
        
        # create a dataframe with Normal Theory and Bootstrap coefficients
        name<-c("a","b","c","c'") 
        normse<-c(sqrt(vcov(LinearModel.1)[2,2]), sqrt(vcov(LinearModel.2)[3,3]), sqrt(vcov(LinearModel.0)[2,2]),sqrt(vcov(LinearModel.2)[2,2]))
        bootse<-c(sd(abboots$t[,1]),sd(abboots$t[,2]),sd(abboots$t[,4]),sd(abboots$t[,5]))
        compBN <- data.frame(name,normse,bootse)
        compBN
        
        #Calculate the proportionate discrepancies using absolute deviations
        ((abs(normse-bootse))/normse)*100
        {sum({(abs(normse-bootse))/normse}*100)}/4
        
        sd(normse-bootse)		#Calculate SD of discrepancies
        mad(normse-bootse)		#Calculate Mean Absolute Deviation of discrepancies
        
        #Computer the correlation between the two sets of estimates
        cor(compBN[,c("bootse","normse")], use="complete.obs")
        
        