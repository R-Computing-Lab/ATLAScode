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

XWomen_tab = as.matrix(cbind(dat2Women[3],round(dat2Women[4:69],digits=2)))

# disorder names ####
diagnoses=as.matrix(dat2Women[,"diagnosis"])
# rownames(XWomen_tab) <- diagnoses

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
rownames(XWomen_tab) <- diagnoses


#Family member type names #################
raw_names = colnames(XWomen_tab)

nice_names = c("Disorder names","Index child (f)\nlog(HR)","SE","n", "Index child (m)\nlog(HR)","SE","n", "Brother\nlog(HR)","SE","n", "Sister\nlog(HR)","SE","n"
               , "Mat. half sister\nlog(HR)","SE","n", "Mat. half brother\nlog(HR)","SE","n"
               , "Pat. half sister\nlog(HR)","SE","n", "Pat. half brother\nlog(HR)","SE","n",
               "Mother\nlog(HR)","SE","n", "Father\nlog(HR)","SE","n", "Mat. grandmother\nlog(HR)","SE","n", "Mat. grandfather\nlog(HR)","SE","n"
               , "Pat. grandmother\nlog(HR)","SE","n", "Pat. grandfather\nlog(HR)","SE","n",
               "Mat. aunt\nlog(HR)","SE","n", "Mat. uncle\nlog(HR)","SE","n", "Pat. aunt\nlog(HR)","SE","n", "Pat. uncle\nlog(HR)","SE","n", "Mat. cousin (f)\nlog(HR)","SE","n"
               , "Mat. cousin (m)\nlog(HR)","SE","n",
               "Pat. cousin (f)\nlog(HR)","SE","n", "Pat. cousin (m)\nlog(HR)","SE","n")

length(nice_names)
dim(XWomen_tab)
#### Main ####
order = c(1:7, 11:13,8:10,26:31,14:16,22:24,17:19,23:25,32:67)
XWomen_tab = XWomen_tab[,order]
nice_names = nice_names[order]
#drop diseases
drop<-c("ai_pemphigus","ai_granulomato")
del_row<-c(82,88)
nice_diagnoses<-as.matrix(nice_diagnoses[-del_row])
nice_diagnoses<-capitalize(nice_diagnoses)
XWomen_tab<-XWomen_tab[!rownames(XWomen_tab) %in% drop,]
rownames(XWomen_tab)<-NULL
XWomen_tab[,1]<-nice_diagnoses
colnames(XWomen_tab)<-nice_names
XWomen_tab<-as.data.frame(XWomen_tab)