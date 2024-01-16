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
datASDID = read_excel("data/dataASDID.xlsx")

# setwd("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/")
# datASDID = read_excel("Data/output_cox_id_asd_final.xlsx")

datASDID$child_1_est<-as.numeric(datASDID$child_1_est)
datASDID$child_1_se<-as.numeric(datASDID$child_1_se)
datASDID$child_1_n<-as.numeric(datASDID$child_1_n)

psych<-datASDID[1:20,]
cm<-datASDID[21:30,]
bd<-datASDID[31:44,]
ne<-datASDID[45:57,]
ai<-datASDID[58:90,]
mis<-datASDID[91:92,]

# order by coefficient size
psychASDID<-psych[order(-psych$sib1_sex0_est),]
cmASDID<-cm[order(-cm$sib1_sex0_est),]
bdASDID<-bd[order(-bd$sib1_sex0_est),]
neASDID<-ne[order(-ne$sib1_sex0_est),]
aiASDID<-ai[order(-ai$sib1_sex0_est),]
misASDID<-mis[order(-mis$sib1_sex0_est),]

# combine
dat2ASDID<-rbind(psychASDID,neASDID,cmASDID,bdASDID,aiASDID,misASDID)

# get relevant cols
est_colsASDID = which(sapply(colnames(datASDID), function(x) substr(x, nchar(x)-3, nchar(x))) == "_est")
se_colsASDID = which(sapply(colnames(datASDID), function(x) substr(x, nchar(x)-2, nchar(x))) == "_se")

# calculate z scores and p-values
zASDID<-abs(dat2ASDID[,c(unname(est_colsASDID))]/dat2ASDID[,c(unname(est_colsASDID)+1)])
pvalASDID<-lapply(zASDID,function(x) 2*pnorm(-abs(x)))
sigASDID<-as.data.frame(lapply(pvalASDID,function(x) ifelse(x<=0.05,"*","")))
sigASDID[is.na(sigASDID)]<-""
low95ASDID<-round(exp(dat2ASDID[,c(unname(est_colsASDID))]-qnorm(.975)*dat2ASDID[,c(unname(se_colsASDID))]),digits = 2)
high95ASDID<-round(exp(dat2ASDID[,c(unname(est_colsASDID))]+qnorm(.975)*dat2ASDID[,c(unname(se_colsASDID))]),digits = 2)


# create matrix of estimates
XASDID = as.matrix(round(exp(dat2ASDID[,est_colsASDID]),digits=2))

#Disorder names
diagnosesASDID=as.matrix(dat2ASDID[,"diagnosis"])
# rownames(X) <- diagnosesASDID

nice_diagnosesASDID<-diagnosesASDID

#for other and td1 (repeat diag names_ rename first then remove the first word)
nice_diagnosesASDID[nice_diagnosesASDID=="cm_t1d"]<-"cm_t1d_cm"
nice_diagnosesASDID[nice_diagnosesASDID=="bd_other"]<-"bd_otherbd"
nice_diagnosesASDID[nice_diagnosesASDID=="ne_other"]<-"ne_otherne"
nice_diagnosesASDID[nice_diagnosesASDID=="bd_skin"]<-"skinbd"
nice_diagnosesASDID[nice_diagnosesASDID=="ai_skin"]<-"skinai"


nice_diagnosesASDID<-gsub(nice_diagnosesASDID,pattern="ai_",replacement="")
nice_diagnosesASDID<-gsub(nice_diagnosesASDID,pattern="ne_",replacement="")
nice_diagnosesASDID<-gsub(nice_diagnosesASDID,pattern="bd_",replacement="")
nice_diagnosesASDID<-gsub(nice_diagnosesASDID,pattern="cm_",replacement="")
nice_diagnosesASDID<-gsub(nice_diagnosesASDID,pattern="mental_",replacement="")

#################### Pretty the names up ####
nice_diagnosesASDID[nice_diagnosesASDID=="asd"]<-"ASD"
nice_diagnosesASDID[nice_diagnosesASDID=="development"]<-"Psych dev dis-not ASD"
nice_diagnosesASDID[nice_diagnosesASDID=="emotional_adhd"]<-"ADHD"
nice_diagnosesASDID[nice_diagnosesASDID=="emotional"]<-"Behav dis-child onset"
nice_diagnosesASDID[nice_diagnosesASDID=="organic"]<-"Organic mental"
nice_diagnosesASDID[nice_diagnosesASDID=="mental"]<- "Any mental"
nice_diagnosesASDID[nice_diagnosesASDID=="adult"]<-"Adult personality disorder"
nice_diagnosesASDID[nice_diagnosesASDID=="mood_bipolar"]<-"Bipolar disorder"
nice_diagnosesASDID[nice_diagnosesASDID=="unspecified"]<-"Mental-unspecified"
nice_diagnosesASDID[nice_diagnosesASDID=="schizo_spectrum"]<-"Schizophrenia spectrum"
nice_diagnosesASDID[nice_diagnosesASDID=="retardation"]<-"Intellectual disability"
nice_diagnosesASDID[nice_diagnosesASDID=="mood"]<-"Any mood"
nice_diagnosesASDID[nice_diagnosesASDID=="schizo"]<-"Schizophrenia"
nice_diagnosesASDID[nice_diagnosesASDID=="emotional_tic"]<-"Tic disorder"
nice_diagnosesASDID[nice_diagnosesASDID=="mood_depression"]<-"Depression"
nice_diagnosesASDID[nice_diagnosesASDID=="neurotic"]<-"Neurotic/stress disorder"
nice_diagnosesASDID[nice_diagnosesASDID=="psychoactive"]<-"Psychoactive sub use"
nice_diagnosesASDID[nice_diagnosesASDID=="behavioral"]<-"Behav synd-physiol"
nice_diagnosesASDID[nice_diagnosesASDID=="behavioral_anex"]<-"Anorexia nervosa"
nice_diagnosesASDID[nice_diagnosesASDID=="neurotic_ocd"]<-"OCD"
nice_diagnosesASDID[nice_diagnosesASDID=="t2d"]<-"Type 2 diabetes"
nice_diagnosesASDID[nice_diagnosesASDID=="dinpreg"]<-"Gestational diabetes"
nice_diagnosesASDID[nice_diagnosesASDID=="diabetes"]<-"Any diabetes"
nice_diagnosesASDID[nice_diagnosesASDID=="obesity"]<-"Obesity"
nice_diagnosesASDID[nice_diagnosesASDID=="doutpreg"]<-"Diabetes outside preg."
nice_diagnosesASDID[nice_diagnosesASDID=="preeclampsia"]<-"Preeclam/eclam"
nice_diagnosesASDID[nice_diagnosesASDID=="hypinpred"]<-"Hypertension in preg"
nice_diagnosesASDID[nice_diagnosesASDID=="hypertension"]<-"Any hyper"
nice_diagnosesASDID[nice_diagnosesASDID=="hypoutpred"]<-"Hyper outside preg"
nice_diagnosesASDID[nice_diagnosesASDID=="t1d_cm"]<-"Type 1 diabetes"
nice_diagnosesASDID[nice_diagnosesASDID=="asdspecific"]<-"Chro/gene dis-ASD spe"
nice_diagnosesASDID[nice_diagnosesASDID=="Lip"]<-"Lip"
nice_diagnosesASDID[nice_diagnosesASDID=="otherbd"]<-"Other/chromos"
nice_diagnosesASDID[nice_diagnosesASDID=="digestive"]<-"Digestive system"
nice_diagnosesASDID[nice_diagnosesASDID=="skinbd"]<-"Skin"
nice_diagnosesASDID[nice_diagnosesASDID=="bd"]<-"Any birth defect"
nice_diagnosesASDID[nice_diagnosesASDID=="heart"]<-"Heart"
nice_diagnosesASDID[nice_diagnosesASDID=="musculoskeletal"]<-"Musculoskeletal"
nice_diagnosesASDID[nice_diagnosesASDID=="ear"]<-"Ear"
nice_diagnosesASDID[nice_diagnosesASDID=="respiratory"]<-"Respiratory"
nice_diagnosesASDID[nice_diagnosesASDID=="cns"]<-"CNS"
nice_diagnosesASDID[nice_diagnosesASDID=="urinary"]<-"Urinary tract"
nice_diagnosesASDID[nice_diagnosesASDID=="eye"]<-"Eye"
nice_diagnosesASDID[nice_diagnosesASDID=="genital"]<-"Genital"

nice_diagnosesASDID[nice_diagnosesASDID=="extrapyramid"]<-"Extrapyramid"
nice_diagnosesASDID[nice_diagnosesASDID=="systemic"]<-"Systemic atrophies"
nice_diagnosesASDID[nice_diagnosesASDID=="episodic"]<-"Episodic"
nice_diagnosesASDID[nice_diagnosesASDID=="episodic_epilep"]<-"Epilepsy"
nice_diagnosesASDID[nice_diagnosesASDID=="ne"]<-"Any neurologic"
nice_diagnosesASDID[nice_diagnosesASDID=="nerve"]<-"Nerve disorder"
nice_diagnosesASDID[nice_diagnosesASDID=="cerebralpal"]<-"Cerebral palsy"
nice_diagnosesASDID[nice_diagnosesASDID=="otherne"]<-"Other neurologic"
nice_diagnosesASDID[nice_diagnosesASDID=="inflammatory"]<-"Inflammatory of CNS"
nice_diagnosesASDID[nice_diagnosesASDID=="demyelinating"]<-"Demyelinating of CNS"
nice_diagnosesASDID[nice_diagnosesASDID=="polynepathi"]<-"Polyneuropath"
nice_diagnosesASDID[nice_diagnosesASDID=="myoneural"]<-"Myoneural"
nice_diagnosesASDID[nice_diagnosesASDID=="otherdegene"]<-"Other degenerative"

nice_diagnosesASDID[nice_diagnosesASDID=="thyroiditis"]<-"Thyroiditis"
nice_diagnosesASDID[nice_diagnosesASDID=="celiac"]<-"Celiac"
nice_diagnosesASDID[nice_diagnosesASDID=="blood"]<-"Any blood"
nice_diagnosesASDID[nice_diagnosesASDID=="connective"]<-"Any connective"
nice_diagnosesASDID[nice_diagnosesASDID=="juvenile"]<-"Juvenile arthritis"
nice_diagnosesASDID[nice_diagnosesASDID=="gastrointest"]<-"Any gastrointest."
nice_diagnosesASDID[nice_diagnosesASDID=="purpura"]<-"Purpura"
nice_diagnosesASDID[nice_diagnosesASDID=="rheumatoid"]<-"Rheumatoid arthritis"
nice_diagnosesASDID[nice_diagnosesASDID=="autoimmune"]<-"Any autoimmune"
nice_diagnosesASDID[nice_diagnosesASDID=="colitis"]<-"Ulcerative colitis"
nice_diagnosesASDID[nice_diagnosesASDID=="crohn"]<-"Crohn"
nice_diagnosesASDID[nice_diagnosesASDID=="endocrine"]<-"Any endocrine"
nice_diagnosesASDID[nice_diagnosesASDID=="thyrotoxico"]<-"Thyrotoxicosis"
nice_diagnosesASDID[nice_diagnosesASDID=="skinai"]<-"Any skin"
nice_diagnosesASDID[nice_diagnosesASDID=="psoriasis"]<-"Psoriasis"
nice_diagnosesASDID[nice_diagnosesASDID=="t1d"]<-"Type 1 diabetes "
nice_diagnosesASDID[nice_diagnosesASDID=="nervous"]<-"Any nervous"
nice_diagnosesASDID[nice_diagnosesASDID=="adrenocortical"]<-"Pri adrenocortical"
nice_diagnosesASDID[nice_diagnosesASDID=="dermatopolymyo"]<-"Dermatopolymyositis"
nice_diagnosesASDID[nice_diagnosesASDID=="polymyalgia"]<-"Polymyalgia"
nice_diagnosesASDID[nice_diagnosesASDID=="scleroderma"]<-"Scleroderma"
nice_diagnosesASDID[nice_diagnosesASDID=="erythemato"]<-"Lupus erythema"
nice_diagnosesASDID[nice_diagnosesASDID=="sjogren"]<-"Sjogren"
nice_diagnosesASDID[nice_diagnosesASDID=="spondili"]<-"Ankylos spondil."
nice_diagnosesASDID[nice_diagnosesASDID=="pernicious"]<-"Pernicious anem"
nice_diagnosesASDID[nice_diagnosesASDID=="hemolytic"]<-"Hemolytic anem"
nice_diagnosesASDID[nice_diagnosesASDID=="sclerosis"]<-"Multple sclerosis"
nice_diagnosesASDID[nice_diagnosesASDID=="guillainbar"]<-"Guillain-Bar"
nice_diagnosesASDID[nice_diagnosesASDID=="gravis"]<-"Myasthen grav."
nice_diagnosesASDID[nice_diagnosesASDID=="areata"]<-"Alopecia areata"
nice_diagnosesASDID[nice_diagnosesASDID=="vitiligo"]<-"Vitiligo"
# nice_diagnosesASDID
rownames(XASDID) <- diagnosesASDID


#Family member type names
raw_names = colnames(XASDID)
nice_namesASDID = c("Index child (f)", "Index child (m)", "Sister", "Brother", "Mat. half sister", "Pat. half sister",
                    "Mat. half brother", "Pat. half brother",
                    "Mother", "Father", "Mat. grandmother", "Mat. grandfather", "Pat. grandmother", "Pat. grandfather",
                    "Mat. aunt", "Mat. uncle", "Pat. aunt", "Pat. uncle", "Mat. cousin (f)", "Mat. cousin (m)","Pat. cousin (f)", "Pat. cousin (m)","")


#### Efigure 17 ####

order = c(1:4 ,9:10,5,7,6,8,11:22)
nice_names_orderASDID<-c(1:4 ,9:10,5,7,6,8,11:22)

XASDID = XASDID[,order]
nice_namesASDID = nice_namesASDID[nice_names_orderASDID]
sigASDID<-sigASDID[,order]
sig2ASDID<-as.matrix(sigASDID)

# inkluder stjerne hvis isg
XASDID_fix=format(round(XASDID,2), nsmall=2)
# XASDID_fix
XASDID_fix[XASDID_fix=="   NA"]<-""


XASDID_fix2<-matrix(paste(XASDID_fix,sig2ASDID,sep=""),nrow=nrow(XASDID_fix),dimnames=dimnames(XASDID_fix))
# XASDID_fix2
XASDID_fix2<-cbind(XASDID_fix2[,1:2],
                   # matrix(NA,nrow=92),
                   XASDID_fix2[,3:22])
XASDID_fix2[is.na(XASDID_fix2)]<-""


#drop diseases
drop<-c("ai_pemphigus","ai_granulomato")
XASDID<-XASDID[!rownames(XASDID) %in% drop,]
XASDID_fix2<-XASDID_fix2[!rownames(XASDID_fix2) %in% drop,]

del_row<-c(82,88)
nice_diagnosesASDID<-as.matrix(nice_diagnosesASDID[-del_row])

nice_diagnosesASDID<-capitalize(nice_diagnosesASDID)

the_palette = rainbow(300, s = 1, v = 1, start = 0, alpha = 1)
n = 90
values = (1:n)/n
values2 =((1:n)/n)/1.06
the_palette1 = hsv(h=1   , s = 1*values, v = 1)
the_palette2 = hsv(h=0.67, s = 1*values2[n:1], v = 1)
the_palette = c(the_palette2, the_palette1)

col_breaks = exp(c(seq(-max(abs(log(XASDID)),na.rm=T),0,length=90), 
                   seq(0.01,max(abs(log(XASDID)),na.rm=T),length=90)))

col_fun=colorRamp2(colors=the_palette,breaks=as.numeric(col_breaks), space = "RGB")


# Get relevant subcategories
any_names = which(sapply(nice_diagnosesASDID, function(x) substr(x, 1, 4)) == "Any ")


bold_anynames=c("plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain"
                ,"plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain","plain")

rownames(XASDID)<-nice_diagnosesASDID
colnames(XASDID)<-nice_namesASDID

diagnosesASDID<-rownames(XASDID)

XASDID<-as.data.frame(XASDID)

rownames(low95ASDID) <- as.matrix(dat2ASDID[,"diagnosis"])
rownames(high95ASDID) <- as.matrix(dat2ASDID[,"diagnosis"])
low95ASDID<-low95ASDID[!rownames(low95ASDID) %in% drop,]
high95ASDID<-high95ASDID[!rownames(high95ASDID) %in% drop,]
low95ASDID<-low95ASDID[,order]
high95ASDID<-high95ASDID[,order]

matASDID<- XASDID
for(col in 1:dim(low95ASDID)[2]){
  for(row in 1:dim(low95ASDID)[1]){
    # print(matASDID[row,col])
    if(is.na(matASDID[row,col])){
      matASDID[row,col]<-" "
    }else{matASDID[row,col]<-paste("95%CI: ", low95ASDID[row,col], " - ", high95ASDID[row,col], "",sep="")}
    # print(mat[row,col])
  }
}
matASDID
# class(XASDIDASDID)
heat_asdASDID<-
  heatmaply(XASDID,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
            cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD and ID \n by each disorder in the respective family member type",
            margins =  c(50,50,60,0),custom_hovertext = matASDID,
            label_names=c("Diagnosis", "Family member", "aHR"),
            scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
              colours = c("blue", "white","red","darkred" ),
              limits = c(0, 17),
              values = rescale(c(0, 1,3,17)),
              oob = squish))
