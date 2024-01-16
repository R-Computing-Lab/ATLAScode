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
# setwd("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/")
# datMen = read_excel("Data/output_cox_men.xlsx")
datMen = read_excel("data/dataMen.xlsx")

datMen$child_0_est<-as.numeric(datMen$child_0_est)
datMen$child_0_se<-as.numeric(datMen$child_0_se)
datMen$child_0_n<-as.numeric(datMen$child_0_n)

psych<-datMen[1:20,]
cm<-datMen[21:30,]
bd<-datMen[31:44,]
ne<-datMen[45:57,]
ai<-datMen[58:90,]
mis<-datMen[91:92,]

# order by coefficient size
psychMen<-psych[order(-psych$sib1_sex0_est),]
cmMen<-cm[order(-cm$sib1_sex0_est),]
bdMen<-bd[order(-bd$sib1_sex0_est),]
neMen<-ne[order(-ne$sib1_sex0_est),]
aiMen<-ai[order(-ai$sib1_sex0_est),]
misMen<-mis[order(-mis$sib1_sex0_est),]

# combine
dat2Men<-rbind(psychMen,neMen,cmMen,bdMen,aiMen,misMen)

# get relevant cols
est_colsMen = which(sapply(colnames(datMen), function(x) substr(x, nchar(x)-3, nchar(x))) == "_est")
se_colsMen = which(sapply(colnames(datMen), function(x) substr(x, nchar(x)-2, nchar(x))) == "_se")

# calculate z scores and p-values
zMen<-abs(dat2Men[,c(unname(est_colsMen))]/dat2Men[,c(unname(est_colsMen)+1)])
pvalMen<-lapply(zMen,function(x) 2*pnorm(-abs(x)))
sigMen<-as.data.frame(lapply(pvalMen,function(x) ifelse(x<=0.05,"*","")))
sigMen[is.na(sigMen)]<-""
low95Men<-round(exp(dat2Men[,c(unname(est_colsMen))]-qnorm(.975)*dat2Men[,c(unname(se_colsMen))]),digits = 2)
high95Men<-round(exp(dat2Men[,c(unname(est_colsMen))]+qnorm(.975)*dat2Men[,c(unname(se_colsMen))]),digits = 2)


# create matrix of estimates
XMen = as.matrix(round(exp(dat2Men[,est_colsMen]),digits=2))

#Disorder names
diagnosesMen=as.matrix(dat2Men[,"diagnosis"])
# rownames(X) <- diagnosesMen

nice_diagnosesMen<-diagnosesMen

#for other and td1 (repeat diag names_ rename first then remove the first word)
nice_diagnosesMen[nice_diagnosesMen=="cm_t1d"]<-"cm_t1d_cm"
nice_diagnosesMen[nice_diagnosesMen=="bd_other"]<-"bd_otherbd"
nice_diagnosesMen[nice_diagnosesMen=="ne_other"]<-"ne_otherne"
nice_diagnosesMen[nice_diagnosesMen=="bd_skin"]<-"skinbd"
nice_diagnosesMen[nice_diagnosesMen=="ai_skin"]<-"skinai"


nice_diagnosesMen<-gsub(nice_diagnosesMen,pattern="ai_",replacement="")
nice_diagnosesMen<-gsub(nice_diagnosesMen,pattern="ne_",replacement="")
nice_diagnosesMen<-gsub(nice_diagnosesMen,pattern="bd_",replacement="")
nice_diagnosesMen<-gsub(nice_diagnosesMen,pattern="cm_",replacement="")
nice_diagnosesMen<-gsub(nice_diagnosesMen,pattern="mental_",replacement="")

#################### Pretty the names up ####
nice_diagnosesMen[nice_diagnosesMen=="asd"]<-"ASD"
nice_diagnosesMen[nice_diagnosesMen=="development"]<-"Psych dev dis-not ASD"
nice_diagnosesMen[nice_diagnosesMen=="emotional_adhd"]<-"ADHD"
nice_diagnosesMen[nice_diagnosesMen=="emotional"]<-"Behav dis-child onset"
nice_diagnosesMen[nice_diagnosesMen=="organic"]<-"Organic mental"
nice_diagnosesMen[nice_diagnosesMen=="mental"]<- "Any mental"
nice_diagnosesMen[nice_diagnosesMen=="adult"]<-"Adult personality disorder"
nice_diagnosesMen[nice_diagnosesMen=="mood_bipolar"]<-"Bipolar disorder"
nice_diagnosesMen[nice_diagnosesMen=="unspecified"]<-"Mental-unspecified"
nice_diagnosesMen[nice_diagnosesMen=="schizo_spectrum"]<-"Schizophrenia spectrum"
nice_diagnosesMen[nice_diagnosesMen=="retardation"]<-"Intellectual disability"
nice_diagnosesMen[nice_diagnosesMen=="mood"]<-"Any mood"
nice_diagnosesMen[nice_diagnosesMen=="schizo"]<-"Schizophrenia"
nice_diagnosesMen[nice_diagnosesMen=="emotional_tic"]<-"Tic disorder"
nice_diagnosesMen[nice_diagnosesMen=="mood_depression"]<-"Depression"
nice_diagnosesMen[nice_diagnosesMen=="neurotic"]<-"Neurotic/stress disorder"
nice_diagnosesMen[nice_diagnosesMen=="psychoactive"]<-"Psychoactive sub use"
nice_diagnosesMen[nice_diagnosesMen=="behavioral"]<-"Behav synd-physiol"
nice_diagnosesMen[nice_diagnosesMen=="behavioral_anex"]<-"Anorexia nervosa"
nice_diagnosesMen[nice_diagnosesMen=="neurotic_ocd"]<-"OCD"
nice_diagnosesMen[nice_diagnosesMen=="t2d"]<-"Type 2 diabetes"
nice_diagnosesMen[nice_diagnosesMen=="dinpreg"]<-"Gestational diabetes"
nice_diagnosesMen[nice_diagnosesMen=="diabetes"]<-"Any diabetes"
nice_diagnosesMen[nice_diagnosesMen=="obesity"]<-"Obesity"
nice_diagnosesMen[nice_diagnosesMen=="doutpreg"]<-"Diabetes outside preg."
nice_diagnosesMen[nice_diagnosesMen=="preeclampsia"]<-"Preeclam/eclam"
nice_diagnosesMen[nice_diagnosesMen=="hypinpred"]<-"Hypertension in preg"
nice_diagnosesMen[nice_diagnosesMen=="hypertension"]<-"Any hyper"
nice_diagnosesMen[nice_diagnosesMen=="hypoutpred"]<-"Hyper outside preg"
nice_diagnosesMen[nice_diagnosesMen=="t1d_cm"]<-"Type 1 diabetes"
nice_diagnosesMen[nice_diagnosesMen=="asdspecific"]<-"Chro/gene dis-ASD spe"
nice_diagnosesMen[nice_diagnosesMen=="Lip"]<-"Lip"
nice_diagnosesMen[nice_diagnosesMen=="otherbd"]<-"Other/chromos"
nice_diagnosesMen[nice_diagnosesMen=="digestive"]<-"Digestive system"
nice_diagnosesMen[nice_diagnosesMen=="skinbd"]<-"Skin"
nice_diagnosesMen[nice_diagnosesMen=="bd"]<-"Any birth defect"
nice_diagnosesMen[nice_diagnosesMen=="heart"]<-"Heart"
nice_diagnosesMen[nice_diagnosesMen=="musculoskeletal"]<-"Musculoskeletal"
nice_diagnosesMen[nice_diagnosesMen=="ear"]<-"Ear"
nice_diagnosesMen[nice_diagnosesMen=="respiratory"]<-"Respiratory"
nice_diagnosesMen[nice_diagnosesMen=="cns"]<-"CNS"
nice_diagnosesMen[nice_diagnosesMen=="urinary"]<-"Urinary tract"
nice_diagnosesMen[nice_diagnosesMen=="eye"]<-"Eye"
nice_diagnosesMen[nice_diagnosesMen=="genital"]<-"Genital"

nice_diagnosesMen[nice_diagnosesMen=="extrapyramid"]<-"Extrapyramid"
nice_diagnosesMen[nice_diagnosesMen=="systemic"]<-"Systemic atrophies"
nice_diagnosesMen[nice_diagnosesMen=="episodic"]<-"Episodic"
nice_diagnosesMen[nice_diagnosesMen=="episodic_epilep"]<-"Epilepsy"
nice_diagnosesMen[nice_diagnosesMen=="ne"]<-"Any neurologic"
nice_diagnosesMen[nice_diagnosesMen=="nerve"]<-"Nerve disorder"
nice_diagnosesMen[nice_diagnosesMen=="cerebralpal"]<-"Cerebral palsy"
nice_diagnosesMen[nice_diagnosesMen=="otherne"]<-"Other neurologic"
nice_diagnosesMen[nice_diagnosesMen=="inflammatory"]<-"Inflammatory of CNS"
nice_diagnosesMen[nice_diagnosesMen=="demyelinating"]<-"Demyelinating of CNS"
nice_diagnosesMen[nice_diagnosesMen=="polynepathi"]<-"Polyneuropath"
nice_diagnosesMen[nice_diagnosesMen=="myoneural"]<-"Myoneural"
nice_diagnosesMen[nice_diagnosesMen=="otherdegene"]<-"Other degenerative"

nice_diagnosesMen[nice_diagnosesMen=="thyroiditis"]<-"Thyroiditis"
nice_diagnosesMen[nice_diagnosesMen=="celiac"]<-"Celiac"
nice_diagnosesMen[nice_diagnosesMen=="blood"]<-"Any blood"
nice_diagnosesMen[nice_diagnosesMen=="connective"]<-"Any connective"
nice_diagnosesMen[nice_diagnosesMen=="juvenile"]<-"Juvenile arthritis"
nice_diagnosesMen[nice_diagnosesMen=="gastrointest"]<-"Any gastrointest."
nice_diagnosesMen[nice_diagnosesMen=="purpura"]<-"Purpura"
nice_diagnosesMen[nice_diagnosesMen=="rheumatoid"]<-"Rheumatoid arthritis"
nice_diagnosesMen[nice_diagnosesMen=="autoimmune"]<-"Any autoimmune"
nice_diagnosesMen[nice_diagnosesMen=="colitis"]<-"Ulcerative colitis"
nice_diagnosesMen[nice_diagnosesMen=="crohn"]<-"Crohn"
nice_diagnosesMen[nice_diagnosesMen=="endocrine"]<-"Any endocrine"
nice_diagnosesMen[nice_diagnosesMen=="thyrotoxico"]<-"Thyrotoxicosis"
nice_diagnosesMen[nice_diagnosesMen=="skinai"]<-"Any skin"
nice_diagnosesMen[nice_diagnosesMen=="psoriasis"]<-"Psoriasis"
nice_diagnosesMen[nice_diagnosesMen=="t1d"]<-"Type 1 diabetes "
nice_diagnosesMen[nice_diagnosesMen=="nervous"]<-"Any nervous"
nice_diagnosesMen[nice_diagnosesMen=="adrenocortical"]<-"Pri adrenocortical"
nice_diagnosesMen[nice_diagnosesMen=="dermatopolymyo"]<-"Dermatopolymyositis"
nice_diagnosesMen[nice_diagnosesMen=="polymyalgia"]<-"Polymyalgia"
nice_diagnosesMen[nice_diagnosesMen=="scleroderma"]<-"Scleroderma"
nice_diagnosesMen[nice_diagnosesMen=="erythemato"]<-"Lupus erythema"
nice_diagnosesMen[nice_diagnosesMen=="sjogren"]<-"Sjogren"
nice_diagnosesMen[nice_diagnosesMen=="spondili"]<-"Ankylos spondil."
nice_diagnosesMen[nice_diagnosesMen=="pernicious"]<-"Pernicious anem"
nice_diagnosesMen[nice_diagnosesMen=="hemolytic"]<-"Hemolytic anem"
nice_diagnosesMen[nice_diagnosesMen=="sclerosis"]<-"Multple sclerosis"
nice_diagnosesMen[nice_diagnosesMen=="guillainbar"]<-"Guillain-Bar"
nice_diagnosesMen[nice_diagnosesMen=="gravis"]<-"Myasthen grav."
nice_diagnosesMen[nice_diagnosesMen=="areata"]<-"Alopecia areata"
nice_diagnosesMen[nice_diagnosesMen=="vitiligo"]<-"Vitiligo"
# nice_diagnosesMen
rownames(XMen) <- diagnosesMen


#Family member type names
raw_names = colnames(XMen)
nice_namesMen = c("Index child (f)", "Index child (m)", "Sister", "Brother", "Mat. half sister", "Pat. half sister",
                    "Mat. half brother", "Pat. half brother",
                    "Mother", "Father", "Mat. grandmother", "Mat. grandfather", "Pat. grandmother", "Pat. grandfather",
                    "Mat. aunt", "Mat. uncle", "Pat. aunt", "Pat. uncle", "Mat. cousin (f)", "Mat. cousin (m)","Pat. cousin (f)", "Pat. cousin (m)","")


#### Efigure 17 ####

order = c(1:4 ,9:10,5,7,6,8,11:22)
nice_names_orderMen<-c(1:4 ,9:10,5,7,6,8,11:22)

XMen = XMen[,order]
nice_namesMen = nice_namesMen[nice_names_orderMen]
sigMen<-sigMen[,order]
sig2Men<-as.matrix(sigMen)

# inkluder stjerne hvis isg
XMen_fix=format(round(XMen,2), nsmall=2)
# XMen_fix
XMen_fix[XMen_fix=="   NA"]<-""


XMen_fix2<-matrix(paste(XMen_fix,sig2Men,sep=""),nrow=nrow(XMen_fix),dimnames=dimnames(XMen_fix))
# XMen_fix2
XMen_fix2<-cbind(XMen_fix2[,1:2],
                   # matrix(NA,nrow=92),
                   XMen_fix2[,3:22])
XMen_fix2[is.na(XMen_fix2)]<-""


#drop diseases
drop<-c("ai_pemphigus","ai_granulomato")
XMen<-XMen[!rownames(XMen) %in% drop,]
XMen_fix2<-XMen_fix2[!rownames(XMen_fix2) %in% drop,]

del_row<-c(82,88)
nice_diagnosesMen<-as.matrix(nice_diagnosesMen[-del_row])

nice_diagnosesMen<-capitalize(nice_diagnosesMen)

the_palette = rainbow(300, s = 1, v = 1, start = 0, alpha = 1)
n = 90
values = (1:n)/n
values2 =((1:n)/n)/1.06
the_palette1 = hsv(h=1   , s = 1*values, v = 1)
the_palette2 = hsv(h=0.67, s = 1*values2[n:1], v = 1)
the_palette = c(the_palette2, the_palette1)

col_breaks = exp(c(seq(-max(abs(log(XMen)),na.rm=T),0,length=90), 
                   seq(0.01,max(abs(log(XMen)),na.rm=T),length=90)))

col_fun=colorRamp2(colors=the_palette,breaks=as.numeric(col_breaks), space = "RGB")


# Get relevant subcategories
any_names = which(sapply(nice_diagnosesMen, function(x) substr(x, 1, 4)) == "Any ")


bold_anynames=c("plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain"
                ,"plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold","plain","plain")

rownames(XMen)<-nice_diagnosesMen
colnames(XMen)<-nice_namesMen

diagnosesMen<-rownames(XMen)

XMen<-as.data.frame(XMen)

rownames(low95Men) <- as.matrix(dat2Men[,"diagnosis"])
rownames(high95Men) <- as.matrix(dat2Men[,"diagnosis"])
low95Men<-low95Men[!rownames(low95Men) %in% drop,]
high95Men<-high95Men[!rownames(high95Men) %in% drop,]
low95Men<-low95Men[,order]
high95Men<-high95Men[,order]

matMen<- XMen
for(col in 1:dim(low95Men)[2]){
  for(row in 1:dim(low95Men)[1]){
    # print(matMen[row,col])
    if(is.na(matMen[row,col])){
      matMen[row,col]<-" "
    }else{matMen[row,col]<-paste("95%CI: ", low95Men[row,col], " - ", high95Men[row,col], "",sep="")}
    # print(mat[row,col])
  }
}
matMen
# class(XMenMen)
heat_asdMen<-
  heatmaply(XMen,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
            cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in men\n by each disorder in the respective family member type",
            margins =  c(50,50,60,0),custom_hovertext = matMen,
            label_names=c("Diagnosis", "Family member", "aHR"),
            scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
              colours = c("blue", "white","red","darkred" ),
              limits = c(0, 17),
              values = rescale(c(0, 1,3,17)),
              oob = squish))