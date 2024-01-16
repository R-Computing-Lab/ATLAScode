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
datWomen = read_excel("data/dataWomen.xlsx")

# setwd("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/")
# datWomen = read_excel("Data/output_cox_women_final.xlsx")

datWomen$child_1_est<-as.numeric(datWomen$child_1_est)
datWomen$child_1_se<-as.numeric(datWomen$child_1_se)
datWomen$child_1_n<-as.numeric(datWomen$child_1_n)

psych<-datWomen[1:20,]
cm<-datWomen[21:30,]
bd<-datWomen[31:44,]
ne<-datWomen[45:57,]
ai<-datWomen[58:90,]
mis<-datWomen[91:92,]

# order by coefficient size
psychWomen<-psych[order(-psych$sib1_sex0_est),]
cmWomen<-cm[order(-cm$sib1_sex0_est),]
bdWomen<-bd[order(-bd$sib1_sex0_est),]
neWomen<-ne[order(-ne$sib1_sex0_est),]
aiWomen<-ai[order(-ai$sib1_sex0_est),]
misWomen<-mis[order(-mis$sib1_sex0_est),]

# combine
dat2Women<-rbind(psychWomen,neWomen,cmWomen,bdWomen,aiWomen,misWomen)

# get relevant cols
est_colsWomen = which(sapply(colnames(datWomen), function(x) substr(x, nchar(x)-3, nchar(x))) == "_est")
se_colsWomen = which(sapply(colnames(datWomen), function(x) substr(x, nchar(x)-2, nchar(x))) == "_se")

# calculate z scores and p-values
zWomen<-abs(dat2Women[,c(unname(est_colsWomen))]/dat2Women[,c(unname(est_colsWomen)+1)])
pvalWomen<-lapply(zWomen,function(x) 2*pnorm(-abs(x)))
sigWomen<-as.data.frame(lapply(pvalWomen,function(x) ifelse(x<=0.05,"*","")))
sigWomen[is.na(sigWomen)]<-""
low95Women<-round(exp(dat2Women[,c(unname(est_colsWomen))]-qnorm(.975)*dat2Women[,c(unname(se_colsWomen))]),digits = 2)
high95Women<-round(exp(dat2Women[,c(unname(est_colsWomen))]+qnorm(.975)*dat2Women[,c(unname(se_colsWomen))]),digits = 2)


# create matrix of estimates
XWomen = as.matrix(round(exp(dat2Women[,est_colsWomen]),digits=2))

#Disorder names
diagnosesWomen=as.matrix(dat2Women[,"diagnosis"])
# rownames(X) <- diagnosesWomen

nice_diagnosesWomen<-diagnosesWomen

#for other and td1 (repeat diag names_ rename first then remove the first word)
nice_diagnosesWomen[nice_diagnosesWomen=="cm_t1d"]<-"cm_t1d_cm"
nice_diagnosesWomen[nice_diagnosesWomen=="bd_other"]<-"bd_otherbd"
nice_diagnosesWomen[nice_diagnosesWomen=="ne_other"]<-"ne_otherne"
nice_diagnosesWomen[nice_diagnosesWomen=="bd_skin"]<-"skinbd"
nice_diagnosesWomen[nice_diagnosesWomen=="ai_skin"]<-"skinai"


nice_diagnosesWomen<-gsub(nice_diagnosesWomen,pattern="ai_",replacement="")
nice_diagnosesWomen<-gsub(nice_diagnosesWomen,pattern="ne_",replacement="")
nice_diagnosesWomen<-gsub(nice_diagnosesWomen,pattern="bd_",replacement="")
nice_diagnosesWomen<-gsub(nice_diagnosesWomen,pattern="cm_",replacement="")
nice_diagnosesWomen<-gsub(nice_diagnosesWomen,pattern="mental_",replacement="")

#################### Pretty the names up ####
nice_diagnosesWomen[nice_diagnosesWomen=="asd"]<-"ASD"
nice_diagnosesWomen[nice_diagnosesWomen=="development"]<-"Psych dev dis-not ASD"
nice_diagnosesWomen[nice_diagnosesWomen=="emotional_adhd"]<-"ADHD"
nice_diagnosesWomen[nice_diagnosesWomen=="emotional"]<-"Behav dis-child onset"
nice_diagnosesWomen[nice_diagnosesWomen=="organic"]<-"Organic mental"
nice_diagnosesWomen[nice_diagnosesWomen=="mental"]<- "Any mental"
nice_diagnosesWomen[nice_diagnosesWomen=="adult"]<-"Adult personality disorder"
nice_diagnosesWomen[nice_diagnosesWomen=="mood_bipolar"]<-"Bipolar disorder"
nice_diagnosesWomen[nice_diagnosesWomen=="unspecified"]<-"Mental-unspecified"
nice_diagnosesWomen[nice_diagnosesWomen=="schizo_spectrum"]<-"Schizophrenia spectrum"
nice_diagnosesWomen[nice_diagnosesWomen=="retardation"]<-"Intellectual disability"
nice_diagnosesWomen[nice_diagnosesWomen=="mood"]<-"Any mood"
nice_diagnosesWomen[nice_diagnosesWomen=="schizo"]<-"Schizophrenia"
nice_diagnosesWomen[nice_diagnosesWomen=="emotional_tic"]<-"Tic disorder"
nice_diagnosesWomen[nice_diagnosesWomen=="mood_depression"]<-"Depression"
nice_diagnosesWomen[nice_diagnosesWomen=="neurotic"]<-"Neurotic/stress disorder"
nice_diagnosesWomen[nice_diagnosesWomen=="psychoactive"]<-"Psychoactive sub use"
nice_diagnosesWomen[nice_diagnosesWomen=="behavioral"]<-"Behav synd-physiol"
nice_diagnosesWomen[nice_diagnosesWomen=="behavioral_anex"]<-"Anorexia nervosa"
nice_diagnosesWomen[nice_diagnosesWomen=="neurotic_ocd"]<-"OCD"
nice_diagnosesWomen[nice_diagnosesWomen=="t2d"]<-"Type 2 diabetes"
nice_diagnosesWomen[nice_diagnosesWomen=="dinpreg"]<-"Gestational diabetes"
nice_diagnosesWomen[nice_diagnosesWomen=="diabetes"]<-"Any diabetes"
nice_diagnosesWomen[nice_diagnosesWomen=="obesity"]<-"Obesity"
nice_diagnosesWomen[nice_diagnosesWomen=="doutpreg"]<-"Diabetes outside preg."
nice_diagnosesWomen[nice_diagnosesWomen=="preeclampsia"]<-"Preeclam/eclam"
nice_diagnosesWomen[nice_diagnosesWomen=="hypinpred"]<-"Hypertension in preg"
nice_diagnosesWomen[nice_diagnosesWomen=="hypertension"]<-"Any hyper"
nice_diagnosesWomen[nice_diagnosesWomen=="hypoutpred"]<-"Hyper outside preg"
nice_diagnosesWomen[nice_diagnosesWomen=="t1d_cm"]<-"Type 1 diabetes"
nice_diagnosesWomen[nice_diagnosesWomen=="asdspecific"]<-"Chro/gene dis-ASD spe"
nice_diagnosesWomen[nice_diagnosesWomen=="Lip"]<-"Lip"
nice_diagnosesWomen[nice_diagnosesWomen=="otherbd"]<-"Other/chromos"
nice_diagnosesWomen[nice_diagnosesWomen=="digestive"]<-"Digestive system"
nice_diagnosesWomen[nice_diagnosesWomen=="skinbd"]<-"Skin"
nice_diagnosesWomen[nice_diagnosesWomen=="bd"]<-"Any birth defect"
nice_diagnosesWomen[nice_diagnosesWomen=="heart"]<-"Heart"
nice_diagnosesWomen[nice_diagnosesWomen=="musculoskeletal"]<-"Musculoskeletal"
nice_diagnosesWomen[nice_diagnosesWomen=="ear"]<-"Ear"
nice_diagnosesWomen[nice_diagnosesWomen=="respiratory"]<-"Respiratory"
nice_diagnosesWomen[nice_diagnosesWomen=="cns"]<-"CNS"
nice_diagnosesWomen[nice_diagnosesWomen=="urinary"]<-"Urinary tract"
nice_diagnosesWomen[nice_diagnosesWomen=="eye"]<-"Eye"
nice_diagnosesWomen[nice_diagnosesWomen=="genital"]<-"Genital"

nice_diagnosesWomen[nice_diagnosesWomen=="extrapyramid"]<-"Extrapyramid"
nice_diagnosesWomen[nice_diagnosesWomen=="systemic"]<-"Systemic atrophies"
nice_diagnosesWomen[nice_diagnosesWomen=="episodic"]<-"Episodic"
nice_diagnosesWomen[nice_diagnosesWomen=="episodic_epilep"]<-"Epilepsy"
nice_diagnosesWomen[nice_diagnosesWomen=="ne"]<-"Any neurologic"
nice_diagnosesWomen[nice_diagnosesWomen=="nerve"]<-"Nerve disorder"
nice_diagnosesWomen[nice_diagnosesWomen=="cerebralpal"]<-"Cerebral palsy"
nice_diagnosesWomen[nice_diagnosesWomen=="otherne"]<-"Other neurologic"
nice_diagnosesWomen[nice_diagnosesWomen=="inflammatory"]<-"Inflammatory of CNS"
nice_diagnosesWomen[nice_diagnosesWomen=="demyelinating"]<-"Demyelinating of CNS"
nice_diagnosesWomen[nice_diagnosesWomen=="polynepathi"]<-"Polyneuropath"
nice_diagnosesWomen[nice_diagnosesWomen=="myoneural"]<-"Myoneural"
nice_diagnosesWomen[nice_diagnosesWomen=="otherdegene"]<-"Other degenerative"

nice_diagnosesWomen[nice_diagnosesWomen=="thyroiditis"]<-"Thyroiditis"
nice_diagnosesWomen[nice_diagnosesWomen=="celiac"]<-"Celiac"
nice_diagnosesWomen[nice_diagnosesWomen=="blood"]<-"Any blood"
nice_diagnosesWomen[nice_diagnosesWomen=="connective"]<-"Any connective"
nice_diagnosesWomen[nice_diagnosesWomen=="juvenile"]<-"Juvenile arthritis"
nice_diagnosesWomen[nice_diagnosesWomen=="gastrointest"]<-"Any gastrointest."
nice_diagnosesWomen[nice_diagnosesWomen=="purpura"]<-"Purpura"
nice_diagnosesWomen[nice_diagnosesWomen=="rheumatoid"]<-"Rheumatoid arthritis"
nice_diagnosesWomen[nice_diagnosesWomen=="autoimmune"]<-"Any autoimmune"
nice_diagnosesWomen[nice_diagnosesWomen=="colitis"]<-"Ulcerative colitis"
nice_diagnosesWomen[nice_diagnosesWomen=="crohn"]<-"Crohn"
nice_diagnosesWomen[nice_diagnosesWomen=="endocrine"]<-"Any endocrine"
nice_diagnosesWomen[nice_diagnosesWomen=="thyrotoxico"]<-"Thyrotoxicosis"
nice_diagnosesWomen[nice_diagnosesWomen=="skinai"]<-"Any skin"
nice_diagnosesWomen[nice_diagnosesWomen=="psoriasis"]<-"Psoriasis"
nice_diagnosesWomen[nice_diagnosesWomen=="t1d"]<-"Type 1 diabetes "
nice_diagnosesWomen[nice_diagnosesWomen=="nervous"]<-"Any nervous"
nice_diagnosesWomen[nice_diagnosesWomen=="adrenocortical"]<-"Pri adrenocortical"
nice_diagnosesWomen[nice_diagnosesWomen=="dermatopolymyo"]<-"Dermatopolymyositis"
nice_diagnosesWomen[nice_diagnosesWomen=="polymyalgia"]<-"Polymyalgia"
nice_diagnosesWomen[nice_diagnosesWomen=="scleroderma"]<-"Scleroderma"
nice_diagnosesWomen[nice_diagnosesWomen=="erythemato"]<-"Lupus erythema"
nice_diagnosesWomen[nice_diagnosesWomen=="sjogren"]<-"Sjogren"
nice_diagnosesWomen[nice_diagnosesWomen=="spondili"]<-"Ankylos spondil."
nice_diagnosesWomen[nice_diagnosesWomen=="pernicious"]<-"Pernicious anem"
nice_diagnosesWomen[nice_diagnosesWomen=="hemolytic"]<-"Hemolytic anem"
nice_diagnosesWomen[nice_diagnosesWomen=="sclerosis"]<-"Multple sclerosis"
nice_diagnosesWomen[nice_diagnosesWomen=="guillainbar"]<-"Guillain-Bar"
nice_diagnosesWomen[nice_diagnosesWomen=="gravis"]<-"Myasthen grav."
nice_diagnosesWomen[nice_diagnosesWomen=="areata"]<-"Alopecia areata"
nice_diagnosesWomen[nice_diagnosesWomen=="vitiligo"]<-"Vitiligo"
# nice_diagnosesWomen
rownames(XWomen) <- diagnosesWomen


#Family member type names
raw_names = colnames(XWomen)
nice_namesWomen = c("Index child (f)", "Index child (m)", "Sister", "Brother", "Mat. half sister", "Pat. half sister",
               "Mat. half brother", "Pat. half brother",
               "Mother", "Father", "Mat. grandmother", "Mat. grandfather", "Pat. grandmother", "Pat. grandfather",
               "Mat. aunt", "Mat. uncle", "Pat. aunt", "Pat. uncle", "Mat. cousin (f)", "Mat. cousin (m)","Pat. cousin (f)", "Pat. cousin (m)","")


#### Efigure 17 ####

order = c(1:4 ,9:10,5,7,6,8,11:22)
nice_names_orderWomen<-c(1:4 ,9:10,5,7,6,8,11:22)

XWomen = XWomen[,order]
nice_namesWomen = nice_namesWomen[nice_names_orderWomen]
sigWomen<-sigWomen[,order]
sig2Women<-as.matrix(sigWomen)

# inkluder stjerne hvis isg
XWomen_fix=format(round(XWomen,2), nsmall=2)
# XWomen_fix
XWomen_fix[XWomen_fix=="   NA"]<-""


XWomen_fix2<-matrix(paste(XWomen_fix,sig2Women,sep=""),nrow=nrow(XWomen_fix),dimnames=dimnames(XWomen_fix))
# XWomen_fix2
XWomen_fix2<-cbind(XWomen_fix2[,1:2],
              # matrix(NA,nrow=92),
              XWomen_fix2[,3:22])
XWomen_fix2[is.na(XWomen_fix2)]<-""


#drop diseases
drop<-c("ai_pemphigus","ai_granulomato")
XWomen<-XWomen[!rownames(XWomen) %in% drop,]
XWomen_fix2<-XWomen_fix2[!rownames(XWomen_fix2) %in% drop,]

del_row<-c(82,88)
nice_diagnosesWomen<-as.matrix(nice_diagnosesWomen[-del_row])

nice_diagnosesWomen<-capitalize(nice_diagnosesWomen)

the_palette = rainbow(300, s = 1, v = 1, start = 0, alpha = 1)
n = 90
values = (1:n)/n
values2 =((1:n)/n)/1.06
the_palette1 = hsv(h=1   , s = 1*values, v = 1)
the_palette2 = hsv(h=0.67, s = 1*values2[n:1], v = 1)
the_palette = c(the_palette2, the_palette1)

col_breaks = exp(c(seq(-max(abs(log(XWomen)),na.rm=T),0,length=90), 
                   seq(0.01,max(abs(log(XWomen)),na.rm=T),length=90)))

col_fun=colorRamp2(colors=the_palette,breaks=as.numeric(col_breaks), space = "RGB")


# Get relevant subcategories
any_names = which(sapply(nice_diagnosesWomen, function(x) substr(x, 1, 4)) == "Any ")


bold_anynames=c("plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain"
                ,"plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain","plain")

rownames(XWomen)<-nice_diagnosesWomen
colnames(XWomen)<-nice_namesWomen

diagnosesWomen<-rownames(XWomen)

XWomen<-as.data.frame(XWomen)

rownames(low95Women) <- as.matrix(dat2Women[,"diagnosis"])
rownames(high95Women) <- as.matrix(dat2Women[,"diagnosis"])
low95Women<-low95Women[!rownames(low95Women) %in% drop,]
high95Women<-high95Women[!rownames(high95Women) %in% drop,]
low95Women<-low95Women[,order]
high95Women<-high95Women[,order]

matWomen<- XWomen
for(col in 1:dim(low95Women)[2]){
  for(row in 1:dim(low95Women)[1]){
    # print(matWomen[row,col])
    if(is.na(matWomen[row,col])){
      matWomen[row,col]<-" "
      }else{matWomen[row,col]<-paste("95%CI: ", low95Women[row,col], " - ", high95Women[row,col], "",sep="")}
    # print(mat[row,col])
  }
}
matWomen
# class(XWomenWomen)
heat_asdWomen<-
  heatmaply(XWomen,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
            cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in women\n by each disorder in the respective family member type",
            margins =  c(50,50,60,0),custom_hovertext = matWomen,
            label_names=c("Diagnosis", "Family member", "aHR"),
            scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
              colours = c("blue", "white","red","darkred" ),
              limits = c(0, 17),
              values = rescale(c(0, 1,3,17)),
              oob = squish))
