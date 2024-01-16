# Shiny Heatmap
library(shiny)
library(shinyWidgets)
library(heatmaply)
library(scales)
library(shinythemes)
library(shinyjs)
library(rsconnect)
library(gplots)
library(readxl)
library(Hmisc)
library(RColorBrewer)
library(circlize)
library(colorspace)
library(GetoptLong)
library(scales)
library(dplyr)
library(extrafont)

# Data management
# setwd("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/shiny/Application publish")
### Initial data management ####
# setwd("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/shiny/Atlas application All heatmaps ver2/data")
datIDconf = read_excel("data/dataIDconf.xlsx")

psych<-datIDconf[1:20,]
cm<-datIDconf[21:30,]
bd<-datIDconf[31:44,]
ne<-datIDconf[45:57,]
ai<-datIDconf[58:90,]
mis<-datIDconf[91:92,]

# order by coefficient size
psychIDconf<-psych[order(-psych$sib1_sex0_est),]
cmIDconf<-cm[order(-cm$sib1_sex0_est),]
bdIDconf<-bd[order(-bd$sib1_sex0_est),]
neIDconf<-ne[order(-ne$sib1_sex0_est),]
aiIDconf<-ai[order(-ai$sib1_sex0_est),]
misIDconf<-mis[order(-mis$sib1_sex0_est),]

# combine
dat2IDconf<-rbind(psychIDconf,neIDconf,cmIDconf,bdIDconf,aiIDconf,misIDconf)

# get relevant cols
est_colsIDconf = which(sapply(colnames(datIDconf), function(x) substr(x, nchar(x)-3, nchar(x))) == "_est")
se_colsIDconf = which(sapply(colnames(datIDconf), function(x) substr(x, nchar(x)-2, nchar(x))) == "_se")

# calculate z scores and p-values
zIDconf<-abs(dat2IDconf[,c(unname(est_colsIDconf))]/dat2IDconf[,c(unname(est_colsIDconf)+1)])
pvalIDconf<-lapply(zIDconf,function(x) 2*pnorm(-abs(x)))
sigIDconf<-as.data.frame(lapply(pvalIDconf,function(x) ifelse(x<=0.05,"*","")))
sigIDconf[is.na(sigIDconf)]<-""
low95IDconf<-round(exp(dat2IDconf[,c(unname(est_colsIDconf))]-qnorm(.975)*dat2IDconf[,c(unname(se_colsIDconf))]),digits = 2)
high95IDconf<-round(exp(dat2IDconf[,c(unname(est_colsIDconf))]+qnorm(.975)*dat2IDconf[,c(unname(se_colsIDconf))]),digits = 2)


# create matrix of estimates
XIDconf = as.matrix(round(exp(dat2IDconf[,est_colsIDconf]),digits=2))

#Disorder names
diagnosesIDconf=as.matrix(dat2IDconf[,"diagnosis"])
# rownames(X) <- diagnosesIDconf

nice_diagnosesIDconf<-diagnosesIDconf

#for other and td1 (repeat diag names_ rename first then remove the first word)
nice_diagnosesIDconf[nice_diagnosesIDconf=="cm_t1d"]<-"cm_t1d_cm"
nice_diagnosesIDconf[nice_diagnosesIDconf=="bd_other"]<-"bd_otherbd"
nice_diagnosesIDconf[nice_diagnosesIDconf=="ne_other"]<-"ne_otherne"
nice_diagnosesIDconf[nice_diagnosesIDconf=="bd_skin"]<-"skinbd"
nice_diagnosesIDconf[nice_diagnosesIDconf=="ai_skin"]<-"skinai"


nice_diagnosesIDconf<-gsub(nice_diagnosesIDconf,pattern="ai_",replacement="")
nice_diagnosesIDconf<-gsub(nice_diagnosesIDconf,pattern="ne_",replacement="")
nice_diagnosesIDconf<-gsub(nice_diagnosesIDconf,pattern="bd_",replacement="")
nice_diagnosesIDconf<-gsub(nice_diagnosesIDconf,pattern="cm_",replacement="")
nice_diagnosesIDconf<-gsub(nice_diagnosesIDconf,pattern="mental_",replacement="")

#################### Pretty the names up ####
nice_diagnosesIDconf[nice_diagnosesIDconf=="asd"]<-"ASD"
nice_diagnosesIDconf[nice_diagnosesIDconf=="development"]<-"Psych dev dis-not ASD"
nice_diagnosesIDconf[nice_diagnosesIDconf=="emotional_adhd"]<-"ADHD"
nice_diagnosesIDconf[nice_diagnosesIDconf=="emotional"]<-"Behav dis-child onset"
nice_diagnosesIDconf[nice_diagnosesIDconf=="organic"]<-"Organic mental"
nice_diagnosesIDconf[nice_diagnosesIDconf=="mental"]<- "Any mental"
nice_diagnosesIDconf[nice_diagnosesIDconf=="adult"]<-"Adult personality disorder"
nice_diagnosesIDconf[nice_diagnosesIDconf=="mood_bipolar"]<-"Bipolar disorder"
nice_diagnosesIDconf[nice_diagnosesIDconf=="unspecified"]<-"Mental-unspecified"
nice_diagnosesIDconf[nice_diagnosesIDconf=="schizo_spectrum"]<-"Schizophrenia spectrum"
nice_diagnosesIDconf[nice_diagnosesIDconf=="retardation"]<-"Intellectual disability"
nice_diagnosesIDconf[nice_diagnosesIDconf=="mood"]<-"Any mood"
nice_diagnosesIDconf[nice_diagnosesIDconf=="schizo"]<-"Schizophrenia"
nice_diagnosesIDconf[nice_diagnosesIDconf=="emotional_tic"]<-"Tic disorder"
nice_diagnosesIDconf[nice_diagnosesIDconf=="mood_depression"]<-"Depression"
nice_diagnosesIDconf[nice_diagnosesIDconf=="neurotic"]<-"Neurotic/stress disorder"
nice_diagnosesIDconf[nice_diagnosesIDconf=="psychoactive"]<-"Psychoactive sub use"
nice_diagnosesIDconf[nice_diagnosesIDconf=="behavioral"]<-"Behav synd-physiol"
nice_diagnosesIDconf[nice_diagnosesIDconf=="behavioral_anex"]<-"Anorexia nervosa"
nice_diagnosesIDconf[nice_diagnosesIDconf=="neurotic_ocd"]<-"OCD"
nice_diagnosesIDconf[nice_diagnosesIDconf=="t2d"]<-"Type 2 diabetes"
nice_diagnosesIDconf[nice_diagnosesIDconf=="dinpreg"]<-"Gestational diabetes"
nice_diagnosesIDconf[nice_diagnosesIDconf=="diabetes"]<-"Any diabetes"
nice_diagnosesIDconf[nice_diagnosesIDconf=="obesity"]<-"Obesity"
nice_diagnosesIDconf[nice_diagnosesIDconf=="doutpreg"]<-"Diabetes outside preg."
nice_diagnosesIDconf[nice_diagnosesIDconf=="preeclampsia"]<-"Preeclam/eclam"
nice_diagnosesIDconf[nice_diagnosesIDconf=="hypinpred"]<-"Hypertension in preg"
nice_diagnosesIDconf[nice_diagnosesIDconf=="hypertension"]<-"Any hyper"
nice_diagnosesIDconf[nice_diagnosesIDconf=="hypoutpred"]<-"Hyper outside preg"
nice_diagnosesIDconf[nice_diagnosesIDconf=="t1d_cm"]<-"Type 1 diabetes"
nice_diagnosesIDconf[nice_diagnosesIDconf=="asdspecific"]<-"Chro/gene dis-ASD spe"
nice_diagnosesIDconf[nice_diagnosesIDconf=="Lip"]<-"Lip"
nice_diagnosesIDconf[nice_diagnosesIDconf=="otherbd"]<-"Other/chromos"
nice_diagnosesIDconf[nice_diagnosesIDconf=="digestive"]<-"Digestive system"
nice_diagnosesIDconf[nice_diagnosesIDconf=="skinbd"]<-"Skin"
nice_diagnosesIDconf[nice_diagnosesIDconf=="bd"]<-"Any birth defect"
nice_diagnosesIDconf[nice_diagnosesIDconf=="heart"]<-"Heart"
nice_diagnosesIDconf[nice_diagnosesIDconf=="musculoskeletal"]<-"Musculoskeletal"
nice_diagnosesIDconf[nice_diagnosesIDconf=="ear"]<-"Ear"
nice_diagnosesIDconf[nice_diagnosesIDconf=="respiratory"]<-"Respiratory"
nice_diagnosesIDconf[nice_diagnosesIDconf=="cns"]<-"CNS"
nice_diagnosesIDconf[nice_diagnosesIDconf=="urinary"]<-"Urinary tract"
nice_diagnosesIDconf[nice_diagnosesIDconf=="eye"]<-"Eye"
nice_diagnosesIDconf[nice_diagnosesIDconf=="genital"]<-"Genital"

nice_diagnosesIDconf[nice_diagnosesIDconf=="extrapyramid"]<-"Extrapyramid"
nice_diagnosesIDconf[nice_diagnosesIDconf=="systemic"]<-"Systemic atrophies"
nice_diagnosesIDconf[nice_diagnosesIDconf=="episodic"]<-"Episodic"
nice_diagnosesIDconf[nice_diagnosesIDconf=="episodic_epilep"]<-"Epilepsy"
nice_diagnosesIDconf[nice_diagnosesIDconf=="ne"]<-"Any neurologic"
nice_diagnosesIDconf[nice_diagnosesIDconf=="nerve"]<-"Nerve disorder"
nice_diagnosesIDconf[nice_diagnosesIDconf=="cerebralpal"]<-"Cerebral palsy"
nice_diagnosesIDconf[nice_diagnosesIDconf=="otherne"]<-"Other neurologic"
nice_diagnosesIDconf[nice_diagnosesIDconf=="inflammatory"]<-"Inflammatory of CNS"
nice_diagnosesIDconf[nice_diagnosesIDconf=="demyelinating"]<-"Demyelinating of CNS"
nice_diagnosesIDconf[nice_diagnosesIDconf=="polynepathi"]<-"Polyneuropath"
nice_diagnosesIDconf[nice_diagnosesIDconf=="myoneural"]<-"Myoneural"
nice_diagnosesIDconf[nice_diagnosesIDconf=="otherdegene"]<-"Other degenerative"

nice_diagnosesIDconf[nice_diagnosesIDconf=="thyroiditis"]<-"Thyroiditis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="celiac"]<-"Celiac"
nice_diagnosesIDconf[nice_diagnosesIDconf=="blood"]<-"Any blood"
nice_diagnosesIDconf[nice_diagnosesIDconf=="connective"]<-"Any connective"
nice_diagnosesIDconf[nice_diagnosesIDconf=="juvenile"]<-"Juvenile arthritis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="gastrointest"]<-"Any gastrointest."
nice_diagnosesIDconf[nice_diagnosesIDconf=="purpura"]<-"Purpura"
nice_diagnosesIDconf[nice_diagnosesIDconf=="rheumatoid"]<-"Rheumatoid arthritis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="autoimmune"]<-"Any autoimmune"
nice_diagnosesIDconf[nice_diagnosesIDconf=="colitis"]<-"Ulcerative colitis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="crohn"]<-"Crohn"
nice_diagnosesIDconf[nice_diagnosesIDconf=="endocrine"]<-"Any endocrine"
nice_diagnosesIDconf[nice_diagnosesIDconf=="thyrotoxico"]<-"Thyrotoxicosis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="skinai"]<-"Any skin"
nice_diagnosesIDconf[nice_diagnosesIDconf=="psoriasis"]<-"Psoriasis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="t1d"]<-"Type 1 diabetes "
nice_diagnosesIDconf[nice_diagnosesIDconf=="nervous"]<-"Any nervous"
nice_diagnosesIDconf[nice_diagnosesIDconf=="adrenocortical"]<-"Pri adrenocortical"
nice_diagnosesIDconf[nice_diagnosesIDconf=="dermatopolymyo"]<-"Dermatopolymyositis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="polymyalgia"]<-"Polymyalgia"
nice_diagnosesIDconf[nice_diagnosesIDconf=="scleroderma"]<-"Scleroderma"
nice_diagnosesIDconf[nice_diagnosesIDconf=="erythemato"]<-"Lupus erythema"
nice_diagnosesIDconf[nice_diagnosesIDconf=="sjogren"]<-"Sjogren"
nice_diagnosesIDconf[nice_diagnosesIDconf=="spondili"]<-"Ankylos spondil."
nice_diagnosesIDconf[nice_diagnosesIDconf=="pernicious"]<-"Pernicious anem"
nice_diagnosesIDconf[nice_diagnosesIDconf=="hemolytic"]<-"Hemolytic anem"
nice_diagnosesIDconf[nice_diagnosesIDconf=="sclerosis"]<-"Multple sclerosis"
nice_diagnosesIDconf[nice_diagnosesIDconf=="guillainbar"]<-"Guillain-Bar"
nice_diagnosesIDconf[nice_diagnosesIDconf=="gravis"]<-"Myasthen grav."
nice_diagnosesIDconf[nice_diagnosesIDconf=="areata"]<-"Alopecia areata"
nice_diagnosesIDconf[nice_diagnosesIDconf=="vitiligo"]<-"Vitiligo"
# nice_diagnosesIDconf
rownames(XIDconf) <- diagnosesIDconf


#Family member type names
raw_names = colnames(XIDconf)
nice_namesIDconf = c("Index child (f)", "Index child (m)", "Sister", "Brother", "Mat. half sister", "Pat. half sister",
                    "Mat. half brother", "Pat. half brother",
                    "Mother", "Father", "Mat. grandmother", "Mat. grandfather", "Pat. grandmother", "Pat. grandfather",
                    "Mat. aunt", "Mat. uncle", "Pat. aunt", "Pat. uncle", "Mat. cousin (f)", "Mat. cousin (m)","Pat. cousin (f)", "Pat. cousin (m)","")


#### Efigure 17 ####

order = c(1:4 ,9:10,5,7,6,8,11:22)
nice_names_orderIDconf<-c(1:4 ,9:10,5,7,6,8,11:22)

XIDconf = XIDconf[,order]
nice_namesIDconf = nice_namesIDconf[nice_names_orderIDconf]
sigIDconf<-sigIDconf[,order]
sig2IDconf<-as.matrix(sigIDconf)

# inkluder stjerne hvis isg
XIDconf_fix=format(round(XIDconf,2), nsmall=2)
# XIDconf_fix
XIDconf_fix[XIDconf_fix=="   NA"]<-""


XIDconf_fix2<-matrix(paste(XIDconf_fix,sig2IDconf,sep=""),nrow=nrow(XIDconf_fix),dimnames=dimnames(XIDconf_fix))
# XIDconf_fix2
XIDconf_fix2<-cbind(XIDconf_fix2[,1:2],
                   # matrix(NA,nrow=92),
                   XIDconf_fix2[,3:22])
XIDconf_fix2[is.na(XIDconf_fix2)]<-""


#drop diseases
drop<-c("ai_pemphigus","ai_granulomato")
XIDconf<-XIDconf[!rownames(XIDconf) %in% drop,]
XIDconf_fix2<-XIDconf_fix2[!rownames(XIDconf_fix2) %in% drop,]

del_row<-c(82,88)
nice_diagnosesIDconf<-as.matrix(nice_diagnosesIDconf[-del_row])

nice_diagnosesIDconf<-capitalize(nice_diagnosesIDconf)

the_palette = rainbow(300, s = 1, v = 1, start = 0, alpha = 1)
n = 90
values = (1:n)/n
values2 =((1:n)/n)/1.06
the_palette1 = hsv(h=1   , s = 1*values, v = 1)
the_palette2 = hsv(h=0.67, s = 1*values2[n:1], v = 1)
the_palette = c(the_palette2, the_palette1)

col_breaks = exp(c(seq(-max(abs(log(XIDconf)),na.rm=T),0,length=90), 
                   seq(0.01,max(abs(log(XIDconf)),na.rm=T),length=90)))

col_fun=colorRamp2(colors=the_palette,breaks=as.numeric(col_breaks), space = "RGB")


# Get relevant subcategories
any_names = which(sapply(nice_diagnosesIDconf, function(x) substr(x, 1, 4)) == "Any ")


bold_anynames=c("plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain"
                ,"plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain","plain")

rownames(XIDconf)<-nice_diagnosesIDconf
colnames(XIDconf)<-nice_namesIDconf

diagnosesIDconf<-rownames(XIDconf)

XIDconf<-as.data.frame(XIDconf)

rownames(low95IDconf) <- as.matrix(dat2IDconf[,"diagnosis"])
rownames(high95IDconf) <- as.matrix(dat2IDconf[,"diagnosis"])
low95IDconf<-low95IDconf[!rownames(low95IDconf) %in% drop,]
high95IDconf<-high95IDconf[!rownames(high95IDconf) %in% drop,]
low95IDconf<-low95IDconf[,order]
high95IDconf<-high95IDconf[,order]

matIDconf<- XIDconf
for(col in 1:dim(low95IDconf)[2]){
  for(row in 1:dim(low95IDconf)[1]){
    # print(matIDconf[row,col])
    if(is.na(matIDconf[row,col])){
      matIDconf[row,col]<-" "
    }else{matIDconf[row,col]<-paste("95%CI: ", low95IDconf[row,col], " - ", high95IDconf[row,col], "",sep="")}
    # print(mat[row,col])
  }
}
matIDconf
# class(XIDconfIDconf)
heat_asdIDconf<-
  heatmaply(XIDconf,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
            cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD overall\n by each disorder in the respective family member type",
            margins =  c(50,50,60,0),custom_hovertext = matIDconf,
            label_names=c("Diagnosis", "Family member", "aHR"),
            scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
              colours = c("blue", "white","red","darkred" ),
              limits = c(0, 17),
              values = rescale(c(0, 1,3,17)),
              oob = squish))
