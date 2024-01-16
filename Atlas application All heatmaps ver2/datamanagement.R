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
#### Initial data management ####
dat = read_excel("data/data.xlsx")

psych<-dat[1:20,]
cm<-dat[21:30,]
bd<-dat[31:44,]
ne<-dat[45:57,]
ai<-dat[58:90,]
mis<-dat[91:92,]

# order by coefficient size
psych<-psych[order(-psych$sib1_sex0_est),]
cm<-cm[order(-cm$sib1_sex0_est),]
bd<-bd[order(-bd$sib1_sex0_est),]
ne<-ne[order(-ne$sib1_sex0_est),]
ai<-ai[order(-ai$sib1_sex0_est),]
mis<-mis[order(-mis$sib1_sex0_est),]

# combine
dat2<-rbind(psych,ne,cm,bd,ai,mis)

# get relevant cols
est_cols = which(sapply(colnames(dat), function(x) substr(x, nchar(x)-3, nchar(x))) == "_est")
se_cols = which(sapply(colnames(dat), function(x) substr(x, nchar(x)-2, nchar(x))) == "_se")

# calculate z scores and p-values
z<-abs(dat2[,c(unname(est_cols))]/dat2[,c(unname(est_cols)+1)])
pval<-lapply(z,function(x) 2*pnorm(-abs(x)))
sig<-as.data.frame(lapply(pval,function(x) ifelse(x<=0.05,"*","")))
sig[is.na(sig)]<-""
low95<-round(exp(dat2[,c(unname(est_cols))]-qnorm(.975)*dat2[,c(unname(se_cols))]),digits = 2)
high95<-round(exp(dat2[,c(unname(est_cols))]+qnorm(.975)*dat2[,c(unname(se_cols))]),digits = 2)


 
# create matrix of estimates
X = as.matrix(round(exp(dat2[,est_cols]),digits=2))

# disorder names ####
diagnoses=as.matrix(dat2[,"diagnosis"])
# rownames(X) <- diagnoses

nice_diagnoses<-diagnoses

#for other and td1 (repeat diag names_ rename first then remove the first word)
nice_diagnoses[nice_diagnoses=="cm_t1d"]<-"cm_t1d_cm"
nice_diagnoses[nice_diagnoses=="bd_other"]<-"bd_otherbd"
nice_diagnoses[nice_diagnoses=="ne_other"]<-"ne_otherne"
nice_diagnoses[nice_diagnoses=="bd_skin"]<-"skinbd"
nice_diagnoses[nice_diagnoses=="ai_skin"]<-"skinai"


nice_diagnoses<-gsub(nice_diagnoses,pattern="ai_",replacement="")
nice_diagnoses<-gsub(nice_diagnoses,pattern="ne_",replacement="")
nice_diagnoses<-gsub(nice_diagnoses,pattern="bd_",replacement="")
nice_diagnoses<-gsub(nice_diagnoses,pattern="cm_",replacement="")
nice_diagnoses<-gsub(nice_diagnoses,pattern="mental_",replacement="")

#################### Pretty the names up ####
nice_diagnoses[nice_diagnoses=="asd"]<-"ASD"
nice_diagnoses[nice_diagnoses=="development"]<-"Psych dev dis-not ASD"
nice_diagnoses[nice_diagnoses=="emotional_adhd"]<-"ADHD"
nice_diagnoses[nice_diagnoses=="emotional"]<-"Behav dis-child onset"
nice_diagnoses[nice_diagnoses=="organic"]<-"Organic mental"
nice_diagnoses[nice_diagnoses=="mental"]<- "Any mental"
nice_diagnoses[nice_diagnoses=="adult"]<-"Adult personality disorder"
nice_diagnoses[nice_diagnoses=="mood_bipolar"]<-"Bipolar disorder"
nice_diagnoses[nice_diagnoses=="unspecified"]<-"Mental-unspecified"
nice_diagnoses[nice_diagnoses=="schizo_spectrum"]<-"Schizophrenia spectrum"
nice_diagnoses[nice_diagnoses=="retardation"]<-"Intellectual disability"
nice_diagnoses[nice_diagnoses=="mood"]<-"Any mood"
nice_diagnoses[nice_diagnoses=="schizo"]<-"Schizophrenia"
nice_diagnoses[nice_diagnoses=="emotional_tic"]<-"Tic disorder"
nice_diagnoses[nice_diagnoses=="mood_depression"]<-"Depression"
nice_diagnoses[nice_diagnoses=="neurotic"]<-"Neurotic/stress disorder"
nice_diagnoses[nice_diagnoses=="psychoactive"]<-"Psychoactive sub use"
nice_diagnoses[nice_diagnoses=="behavioral"]<-"Behav synd-physiol"
nice_diagnoses[nice_diagnoses=="behavioral_anex"]<-"Anorexia nervosa"
nice_diagnoses[nice_diagnoses=="neurotic_ocd"]<-"OCD"
nice_diagnoses[nice_diagnoses=="t2d"]<-"Type 2 diabetes"
nice_diagnoses[nice_diagnoses=="dinpreg"]<-"Gestational diabetes"
nice_diagnoses[nice_diagnoses=="diabetes"]<-"Any diabetes"
nice_diagnoses[nice_diagnoses=="obesity"]<-"Obesity"
nice_diagnoses[nice_diagnoses=="doutpreg"]<-"Diabetes outside preg."
nice_diagnoses[nice_diagnoses=="preeclampsia"]<-"Preeclam/eclam"
nice_diagnoses[nice_diagnoses=="hypinpred"]<-"Hypertension in preg"
nice_diagnoses[nice_diagnoses=="hypertension"]<-"Any hyper"
nice_diagnoses[nice_diagnoses=="hypoutpred"]<-"Hyper outside preg"
nice_diagnoses[nice_diagnoses=="t1d_cm"]<-"Type 1 diabetes"
nice_diagnoses[nice_diagnoses=="asdspecific"]<-"Chro/gene dis-ASD spe"
nice_diagnoses[nice_diagnoses=="Lip"]<-"Lip"
nice_diagnoses[nice_diagnoses=="otherbd"]<-"Other/chromos"
nice_diagnoses[nice_diagnoses=="digestive"]<-"Digestive system"
nice_diagnoses[nice_diagnoses=="skinbd"]<-"Skin"
nice_diagnoses[nice_diagnoses=="bd"]<-"Any birth defect"
nice_diagnoses[nice_diagnoses=="heart"]<-"Heart"
nice_diagnoses[nice_diagnoses=="musculoskeletal"]<-"Musculoskeletal"
nice_diagnoses[nice_diagnoses=="ear"]<-"Ear"
nice_diagnoses[nice_diagnoses=="respiratory"]<-"Respiratory"
nice_diagnoses[nice_diagnoses=="cns"]<-"CNS"
nice_diagnoses[nice_diagnoses=="urinary"]<-"Urinary tract"
nice_diagnoses[nice_diagnoses=="eye"]<-"Eye"
nice_diagnoses[nice_diagnoses=="genital"]<-"Genital"

nice_diagnoses[nice_diagnoses=="extrapyramid"]<-"Extrapyramid"
nice_diagnoses[nice_diagnoses=="systemic"]<-"Systemic atrophies"
nice_diagnoses[nice_diagnoses=="episodic"]<-"Episodic"
nice_diagnoses[nice_diagnoses=="episodic_epilep"]<-"Epilepsy"
nice_diagnoses[nice_diagnoses=="ne"]<-"Any neurologic"
nice_diagnoses[nice_diagnoses=="nerve"]<-"Nerve disorder"
nice_diagnoses[nice_diagnoses=="cerebralpal"]<-"Cerebral palsy"
nice_diagnoses[nice_diagnoses=="otherne"]<-"Other neurologic"
nice_diagnoses[nice_diagnoses=="inflammatory"]<-"Inflammatory of CNS"
nice_diagnoses[nice_diagnoses=="demyelinating"]<-"Demyelinating of CNS"
nice_diagnoses[nice_diagnoses=="polynepathi"]<-"Polyneuropath"
nice_diagnoses[nice_diagnoses=="myoneural"]<-"Myoneural"
nice_diagnoses[nice_diagnoses=="otherdegene"]<-"Other degenerative"

nice_diagnoses[nice_diagnoses=="thyroiditis"]<-"Thyroiditis"
nice_diagnoses[nice_diagnoses=="celiac"]<-"Celiac"
nice_diagnoses[nice_diagnoses=="blood"]<-"Any blood"
nice_diagnoses[nice_diagnoses=="connective"]<-"Any connective"
nice_diagnoses[nice_diagnoses=="juvenile"]<-"Juvenile arthritis"
nice_diagnoses[nice_diagnoses=="gastrointest"]<-"Any gastrointest."
nice_diagnoses[nice_diagnoses=="purpura"]<-"Purpura"
nice_diagnoses[nice_diagnoses=="rheumatoid"]<-"Rheumatoid arthritis"
nice_diagnoses[nice_diagnoses=="autoimmune"]<-"Any autoimmune"
nice_diagnoses[nice_diagnoses=="colitis"]<-"Ulcerative colitis"
nice_diagnoses[nice_diagnoses=="crohn"]<-"Crohn"
nice_diagnoses[nice_diagnoses=="endocrine"]<-"Any endocrine"
nice_diagnoses[nice_diagnoses=="thyrotoxico"]<-"Thyrotoxicosis"
nice_diagnoses[nice_diagnoses=="skinai"]<-"Any skin"
nice_diagnoses[nice_diagnoses=="psoriasis"]<-"Psoriasis"
nice_diagnoses[nice_diagnoses=="t1d"]<-"Type 1 diabetes "
nice_diagnoses[nice_diagnoses=="nervous"]<-"Any nervous"
nice_diagnoses[nice_diagnoses=="adrenocortical"]<-"Pri adrenocortical"
nice_diagnoses[nice_diagnoses=="dermatopolymyo"]<-"Dermatopolymyositis"
nice_diagnoses[nice_diagnoses=="polymyalgia"]<-"Polymyalgia"
nice_diagnoses[nice_diagnoses=="scleroderma"]<-"Scleroderma"
nice_diagnoses[nice_diagnoses=="erythemato"]<-"Lupus erythema"
nice_diagnoses[nice_diagnoses=="sjogren"]<-"Sjogren"
nice_diagnoses[nice_diagnoses=="spondili"]<-"Ankylos spondil."
nice_diagnoses[nice_diagnoses=="pernicious"]<-"Pernicious anem"
nice_diagnoses[nice_diagnoses=="hemolytic"]<-"Hemolytic anem"
nice_diagnoses[nice_diagnoses=="sclerosis"]<-"Multple sclerosis"
nice_diagnoses[nice_diagnoses=="guillainbar"]<-"Guillain-Bar"
nice_diagnoses[nice_diagnoses=="gravis"]<-"Myasthen grav."
nice_diagnoses[nice_diagnoses=="areata"]<-"Alopecia areata"
nice_diagnoses[nice_diagnoses=="vitiligo"]<-"Vitiligo"

#########
rownames(X) <- diagnoses


#Family member type names #################
raw_names = colnames(X)

nice_names = c("Index child (f)", "Index child (m)", "Brother", "Sister", "Mat. half sister", "Mat. half brother"
               , "Pat. half sister", "Pat. half brother",
               "Mother", "Father", "Mat. grandmother", "Mat. grandfather", "Pat. grandmother", "Pat. grandfather",
               "Mat. aunt", "Mat. uncle", "Pat. aunt", "Pat. uncle", "Mat. cousin (f)", "Mat. cousin (m)","Pat. cousin (f)", "Pat. cousin (m)","")


#### Main ####
order = c(1:2, 4,3,9:10,5,7,6,8,11:22)

X = X[,order]
nice_names = nice_names[order]
sig<-sig[,order]
sig2<-as.matrix(sig)

# inkluder stjerne hvis isg
X_fix=format(round(X,2), nsmall=2)
# X_fix
X_fix[X_fix=="   NA"]<-""
# X_fix[,1:4]<-""
# X_fix[,13:16]<-""

X_fix2<-matrix(paste(X_fix,sig2,sep=""),nrow=nrow(X_fix),dimnames=dimnames(X_fix))
# X_fix2
X_fix2[is.na(X_fix2)]<-""

# dim(X_fix2)
# dim(X)
# t(X)


#drop diseases
drop<-c("ai_pemphigus","ai_granulomato")
X<-X[!rownames(X) %in% drop,]

X_fix2<-X_fix2[!rownames(X_fix2) %in% drop,]
# nice_diagnoses<-nice_diagnoses[!rownames(nice_diagnoses) %in% c("pemphigus","granulomato"),]
del_row<-c(82,88)
nice_diagnoses<-as.matrix(nice_diagnoses[-del_row])
nice_diagnoses<-capitalize(nice_diagnoses)

rownames_main<-X

the_palette = rainbow(300, s = 1, v = 1, start = 0, alpha = 1)
n = 90
values = (1:n)/n
values2 =((1:n)/n)/1.06
the_palette1 = hsv(h=1   , s = 1*values, v = 1)
the_palette2 = hsv(h=0.67, s = 1*values2[n:1], v = 1)
the_palette = c(the_palette2, the_palette1)

col_breaks = exp(c(seq(-max(abs(log(X)),na.rm=T),0,length=90), 
                   seq(0.01,max(abs(log(X)),na.rm=T),length=90)))


# col_breaks<-c(seq(0.00,1,length=99),seq(1.01,9,length=97),seq(9.01,20,length=4))

col_fun=colorRamp2(colors=the_palette,breaks=as.numeric(col_breaks), space = "RGB")

# col_fun=colorRamp2(colors=c("lightskyblue","white","red","red"),breaks=c(0,1,5,30))

# Get relevant subcategories
any_names = which(sapply(nice_diagnoses, function(x) substr(x, 1, 4)) == "Any ")

# bold_anynames<-noquote(paste(c(rep('"plain"',5), rep('"bold"',1)), collapse=","))
# bold_anynames<-gsub(bold_anynames="""",replacement="")

bold_anynames=c("plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"plain","plain","plain","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain"
                ,"bold","plain","plain","plain","plain","bold","plain","plain","plain","plain","plain","plain","plain","bold","plain"
                ,"plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","plain","bold")

source("chooser.R")
# setwd("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/shiny")


rownames(X)<-nice_diagnoses
colnames(X)<-nice_names

# par( oma = c(0,0,0,0))

diagnoses<-rownames(X)

X<-as.data.frame(X)

rownames(low95) <- as.matrix(dat2[,"diagnosis"])
rownames(high95) <- as.matrix(dat2[,"diagnosis"])
low95<-low95[!rownames(low95) %in% drop,]
high95<-high95[!rownames(high95) %in% drop,]
low95<-low95[,order]
high95<-high95[,order]


matOverall <- as.data.frame(X)


for(col in 1:dim(low95)[2]){
  for(row in 1:dim(low95)[1]){
    # print(matOverall[row,col])
    if(is.na(matOverall[row,col])){
      matOverall[row,col]<-" "
    }else{matOverall[row,col]<-paste("95%CI: ", low95[row,col], " - ", high95[row,col], "",sep="")}
    
    # print(mat[row,col])
  }
}

heat_asd<-
  heatmaply(X,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
            cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in index child\n by each disorder in the respective family member type",
            margins =  c(50,50,60,0),custom_hovertext = matOverall,
            label_names=c("Diagnosis", "Family member", "aHR"),
            scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
              colours = c("blue", "white","red","darkred" ),
              limits = c(0, 17),
              values = rescale(c(0, 1,3,17)),
              oob = squish))


