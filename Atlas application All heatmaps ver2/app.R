# install.packages("gplots")
# install.packages("circlize");install.packages("circlize")
# install.packages("extrafont")

library(shiny)
library(shinyWidgets)
library(heatmaply)
library(scales)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(rsconnect)

# source("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/shiny/Application publish/Datamanagement HM main.R")
# source("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/shiny/Application publish/datamanagement.R")
# source("C:/Users/au281223/Dropbox/Postdoc/ASD/Family history atlas/shiny/Application publish/chooser.R")
source("datamanagement.R")
source("datamanagement2.R")
source("datamanagementWomen.R")
source("datamanagementWomen2.R")
source("datamanagementIDconf.R")
source("datamanagementIDconf2.R")
source("datamanagementMen.R")
source("datamanagementMen2.R")
source("datamanagementASDID.R")
source("datamanagementASDID2.R")
source("chooser.R")


server<-function(input,output, session){
  
  ######## Data tables ####
  # sorted columns are colored now because CSS are attached to them
  output$mytable_X_tab <- DT::renderDataTable({
    DT::datatable(X_tab, options = list(orderClasses = TRUE))
  })
  
  output$mytable_XWomen_tab <- DT::renderDataTable({
    DT::datatable(XWomen_tab, options = list(orderClasses = TRUE))
  })
  
  output$mytable_XMen_tab <- DT::renderDataTable({
    DT::datatable(XMen_tab, options = list(orderClasses = TRUE))
  })
  
  output$mytable_XASDID_tab <- DT::renderDataTable({
    DT::datatable(XASDID_tab, options = list(orderClasses = TRUE))
  })
  
  output$mytable_XIDConf_tab <- DT::renderDataTable({
    DT::datatable(XIDconf_tab, options = list(orderClasses = TRUE))
  })
  
  #  Buttons Overall####
  observeEvent(input$diagnoses_groups, {
    # message("update button was pressed")
    groups_diag<-list()
    groups_list<-NA
    group_diag<-lapply(input$diagnoses_groups,function(diagnoses_groups){
      if (diagnoses_groups=="Mental"){
        input<-nice_diagnoses[1:20]
      } else if (diagnoses_groups=="Neurologic") {
        input<-nice_diagnoses[45:57]
      }else if (diagnoses_groups=="Cardiometabolic") {
        input<-nice_diagnoses[21:30]
      }else if (diagnoses_groups=="Birth defect") {
        input<-nice_diagnoses[31:44]
      }else if (diagnoses_groups=="Autoimmune") {
        input<-nice_diagnoses[58:88]
      }else if (diagnoses_groups=="Other") {
        input<-nice_diagnoses[89:90]
      }
      groups_list<-c(groups_list,input)
    }
    )
    diag_group<-unlist(group_diag)
    print(diag_group)
    updateSelectizeInput(session,"selectdis",choices =rownames(X),selected=rownames(X[diag_group,]))
  })
  observeEvent(input$resetDis, {
    reset("selectdis")
    reset("diagnoses_groups")
  })
  observeEvent(input$resetFam, {
    reset("selectfam")
  })
  
  #################################### Buttons Women ####################
  observeEvent(input$diagnoses_groupsWomen, {
    # message("update button was pressed")
    groups_diagWomen<-list()
    groups_listWomen<-NA
    group_diagWomen<-lapply(input$diagnoses_groupsWomen,function(diagnoses_groupsWomen){
      if (diagnoses_groupsWomen=="Mental"){
        input<-nice_diagnosesWomen[1:20]
      } else if (diagnoses_groupsWomen=="Neurologic") {
        input<-nice_diagnosesWomen[45:57]
      }else if (diagnoses_groupsWomen=="Cardiometabolic") {
        input<-nice_diagnosesWomen[21:30]
      }else if (diagnoses_groupsWomen=="Birth defect") {
        input<-nice_diagnosesWomen[31:44]
      }else if (diagnoses_groupsWomen=="Autoimmune") {
        input<-nice_diagnosesWomen[58:88]
      }else if (diagnoses_groupsWomen=="Other") {
        input<-nice_diagnosesWomen[89:90]
      }
      groups_listWomen<-c(groups_listWomen,input)
    }
    )
    diag_groupWomen<-unlist(group_diagWomen)
    print(diag_groupWomen)
    updateSelectizeInput(session,"selectdisWomen",choices =rownames(XWomen),selected=rownames(X[diag_groupWomen,]))
  })
  
  observeEvent(input$resetDisWomen, {
    reset("selectdisWomen")
    reset("diagnoses_groupsWomen")
  })
  observeEvent(input$resetFamWomen, {
    reset("selectfamWomen")
  })
  
  
  #################################### Buttons Men ####################
  observeEvent(input$diagnoses_groupsMen, {
    # message("update button was pressed")
    groups_diagMen<-list()
    groups_listMen<-NA
    group_diagMen<-lapply(input$diagnoses_groupsMen,function(diagnoses_groupsMen){
      if (diagnoses_groupsMen=="Mental"){
        input<-nice_diagnosesMen[1:20]
      } else if (diagnoses_groupsMen=="Neurologic") {
        input<-nice_diagnosesMen[45:57]
      }else if (diagnoses_groupsMen=="Cardiometabolic") {
        input<-nice_diagnosesMen[21:30]
      }else if (diagnoses_groupsMen=="Birth defect") {
        input<-nice_diagnosesMen[31:44]
      }else if (diagnoses_groupsMen=="Autoimmune") {
        input<-nice_diagnosesMen[58:88]
      }else if (diagnoses_groupsMen=="Other") {
        input<-nice_diagnosesMen[89:90]
      }
      groups_listMen<-c(groups_listMen,input)
    }
    )
    diag_groupMen<-unlist(group_diagMen)
    print(diag_groupMen)
    updateSelectizeInput(session,"selectdisMen",choices =rownames(XMen),selected=rownames(X[diag_groupMen,]))
  })
  
  observeEvent(input$resetDisMen, {
    reset("selectdisMen")
    reset("diagnoses_groupsMen")
  })
  observeEvent(input$resetFamMen, {
    reset("selectfamMen")
  })
  
  #################################### Buttons ASDID ####################
  observeEvent(input$diagnoses_groupsASDID, {
    # message("update button was pressed")
    groups_diagASDID<-list()
    groups_listASDID<-NA
    group_diagASDID<-lapply(input$diagnoses_groupsASDID,function(diagnoses_groupsASDID){
      if (diagnoses_groupsASDID=="ASDIDtal"){
        input<-nice_diagnosesASDID[1:20]
      } else if (diagnoses_groupsASDID=="Neurologic") {
        input<-nice_diagnosesASDID[45:57]
      }else if (diagnoses_groupsASDID=="Cardiometabolic") {
        input<-nice_diagnosesASDID[21:30]
      }else if (diagnoses_groupsASDID=="Birth defect") {
        input<-nice_diagnosesASDID[31:44]
      }else if (diagnoses_groupsASDID=="Autoimmune") {
        input<-nice_diagnosesASDID[58:88]
      }else if (diagnoses_groupsASDID=="Other") {
        input<-nice_diagnosesASDID[89:90]
      }
      groups_listASDID<-c(groups_listASDID,input)
    }
    )
    diag_groupASDID<-unlist(group_diagASDID)
    print(diag_groupASDID)
    updateSelectizeInput(session,"selectdisASDID",choices =rownames(XASDID),selected=rownames(X[diag_groupASDID,]))
  })
  
  observeEvent(input$resetDisASDID, {
    reset("selectdisASDID")
    reset("diagnoses_groupsASDID")
  })
  observeEvent(input$resetFamASDID, {
    reset("selectfamASDID")
  })
  
  #################################### Buttons IDconf ####################
  observeEvent(input$diagnoses_groupsIDconf, {
    # message("update button was pressed")
    groups_diagIDconf<-list()
    groups_listIDconf<-NA
    group_diagIDconf<-lapply(input$diagnoses_groupsIDconf,function(diagnoses_groupsIDconf){
      if (diagnoses_groupsIDconf=="IDconftal"){
        input<-nice_diagnosesIDconf[1:20]
      } else if (diagnoses_groupsIDconf=="Neurologic") {
        input<-nice_diagnosesIDconf[45:57]
      }else if (diagnoses_groupsIDconf=="Cardiometabolic") {
        input<-nice_diagnosesIDconf[21:30]
      }else if (diagnoses_groupsIDconf=="Birth defect") {
        input<-nice_diagnosesIDconf[31:44]
      }else if (diagnoses_groupsIDconf=="Autoimmune") {
        input<-nice_diagnosesIDconf[58:88]
      }else if (diagnoses_groupsIDconf=="Other") {
        input<-nice_diagnosesIDconf[89:90]
      }
      groups_listIDconf<-c(groups_listIDconf,input)
    }
    )
    diag_groupIDconf<-unlist(group_diagIDconf)
    print(diag_groupIDconf)
    updateSelectizeInput(session,"selectdisIDconf",choices =rownames(XIDconf),selected=rownames(X[diag_groupIDconf,]))
  })
  
  observeEvent(input$resetDisIDconf, {
    reset("selectdisIDconf")
    reset("diagnoses_groupsIDconf")
  })
  observeEvent(input$resetFamIDconf, {
    reset("selectfamIDconf")
  })
  
  ######################### Overall go heatmap ####
  observeEvent(input$go, {
    # init start col
    famtype<-colnames(X)
    diagnoses<-rownames(X)
    
    
    diagnoses<-input$selectdis
    famtype<-input$selectfam 
    
    #Select col - OVERALL
    X2<- X %>%
      select(famtype)
    # select row - OVERALL
    X2<-X2[diagnoses,]
    class(X2)
    
    
    
    matOverall2 <- matOverall %>%
      select(famtype)
    
    matOverall2<-matOverall2[diagnoses,]
    
    output$heatmap <- renderPlotly({
      heatmaply(X2,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
                cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in index child\n by each disorder in the respective family member type",
                margins =  c(50,50,60,0),custom_hovertext = matOverall2,
                label_names=c("Diagnosis", "Family member", "aHR"),
                scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
                  colours = c("blue", "white","red","darkred" ),
                  limits = c(0, 17),
                  values = rescale(c(0, 1,3,17)),
                  oob = squish))
    })
    
  })
  
  ####################### Women go heatmap ######
  observeEvent(input$goWomen, {
    # init start col
    diagnosesWomen<-input$selectdisWomen
    famtypeWomen<-input$selectfamWomen
    
    #Select col - OVERALL
    X2Women<- XWomen%>%
      select(famtypeWomen)
    # select row - OVERALL
    X2Women<-X2Women[diagnosesWomen,]
    
    matWomen2 <- matWomen %>%
      select(famtype)
    
    matWomen2<-matWomen2[diagnoses,]
    
    # X2<-as.data.frame(X2)
    output$heatmapWomen <- renderPlotly({
      heatmaply(X2Women,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
                cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in index child\n by each disorder in the respective family member type",
                margins =  c(50,50,60,0),custom_hovertext = matWomen2,
                label_names=c("Diagnosis", "Family member", "aHR"),
                scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
                  colours = c("blue", "white","red","darkred" ),
                  limits = c(0, 17),
                  values = rescale(c(0, 1,3,17)),
                  oob = squish))
    })
    
  })
  
  output$heatmap <- renderPlotly({
    heat_asd
  })
  
  output$heatmapWomen <- renderPlotly({
    heat_asdWomen
  })
  
  ####################### Men go heatmap ######
  observeEvent(input$goMen, {
    # init start col
    diagnosesMen<-input$selectdisMen
    famtypeMen<-input$selectfamMen
    
    #Select col - OVERALL
    X2Men<- XMen%>%
      select(famtypeMen)
    # select row - OVERALL
    X2Men<-X2Men[diagnosesMen,]
    
    matMen2 <- matMen %>%
      select(famtype)
    
    matMen2<-matMen2[diagnoses,]
    
    # X2<-as.data.frame(X2)
    output$heatmapMen <- renderPlotly({
      heatmaply(X2Men,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
                cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in index child\n by each disorder in the respective family member type",
                margins =  c(50,50,60,0),custom_hovertext = matMen2,
                label_names=c("Diagnosis", "Family member", "aHR"),
                scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
                  colours = c("blue", "white","red","darkred" ),
                  limits = c(0, 17),
                  values = rescale(c(0, 1,3,17)),
                  oob = squish))
    })
    
  })
  
  output$heatmap <- renderPlotly({
    heat_asd
  })
  
  output$heatmapMen <- renderPlotly({
    heat_asdMen
  })
  
  ####################### ASDID go heatmap ######
  observeEvent(input$goASDID, {
    # init start col
    diagnosesASDID<-input$selectdisASDID
    famtypeASDID<-input$selectfamASDID
    
    #Select col - OVERALL
    X2ASDID<- XASDID%>%
      select(famtypeASDID)
    # select row - OVERALL
    X2ASDID<-X2ASDID[diagnosesASDID,]
    
    matASDID2 <- matASDID %>%
      select(famtype)
    
    matASDID2<-matASDID2[diagnoses,]
    
    # X2<-as.data.frame(X2)
    output$heatmapASDID <- renderPlotly({
      heatmaply(X2ASDID,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
                cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in index child\n by each disorder in the respective family member type",
                margins =  c(50,50,60,0),custom_hovertext = matASDID2,
                label_names=c("Diagnosis", "Family member", "aHR"),
                scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
                  colours = c("blue", "white","red","darkred" ),
                  limits = c(0, 17),
                  values = rescale(c(0, 1,3,17)),
                  oob = squish))
    })
    
  })
  
  output$heatmap <- renderPlotly({
    heat_asd
  })
  
  output$heatmapASDID <- renderPlotly({
    heat_asdASDID
  })
  
  ####################### IDconf go heatmap ######
  observeEvent(input$goIDconf, {
    # init start col
    diagnosesIDconf<-input$selectdisIDconf
    famtypeIDconf<-input$selectfamIDconf
    
    #Select col - OVERALL
    X2IDconf<- XIDconf%>%
      select(famtypeIDconf)
    # select row - OVERALL
    X2IDconf<-X2IDconf[diagnosesIDconf,]
    
    matIDconf2 <- matIDconf %>%
      select(famtype)
    
    matIDconf2<-matIDconf2[diagnoses,]
    
    # X2<-as.data.frame(X2)
    output$heatmapIDconf <- renderPlotly({
      heatmaply(X2IDconf,Rowv = FALSE, Colv=FALSE,legend=FALSE,hide_colorbar =F,
                cexRow=0.8, main = "Adjusted Hazard Ratio (aHR) of ASD in index child\n by each disorder in the respective family member type",
                margins =  c(50,50,60,0),custom_hovertext = matIDconf2,
                label_names=c("Diagnosis", "Family member", "aHR"),
                scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
                  colours = c("blue", "white","red","darkred" ),
                  limits = c(0, 17),
                  values = rescale(c(0, 1,3,17)),
                  oob = squish))
    })
    
  })
  
  output$heatmap <- renderPlotly({
    heat_asd
  })
  
  output$heatmapIDconf <- renderPlotly({
    heat_asdIDconf
  })
  
  #################### Buttons go to ################
  observeEvent(input$jumpToResults, {
    updateTabsetPanel(session, "MainOverall",
                      selected = "Results")
  })
  
  observeEvent(input$jumpToMethods, {
    updateTabsetPanel(session, "MainOverall",
                      selected = "Methods")
  })
  
  observeEvent(input$jumpToMethods2, {
    updateTabsetPanel(session, "MainOverall",
                      selected = "Methods")
  })
  
  
  observeEvent(input$jumpToMethods3, {
    updateTabsetPanel(session, "MainOverall",
                      selected = "Methods")
  })
  
  
  observeEvent(input$jumpToOverallHeat, {
    updateTabsetPanel(session, "MainOverall",
                      selected = "OverallHeat")
  })
  
}

# shinyApp(ui=ui,server=server)

# names(X)


############# UI ###########################
ui<-tagList(
  includeCSS("www/style.css"),
  tags$head(tags$link(includeScript("www/func.js"))),
  tags$head(tags$style("a{cursor:pointer;}")),
  # includeCSS(path = "AdminLTE.css"), #added 
  # includeCSS(path = "shinydashboard.css"), #added
  
  #add this file and collapsible nature should work.
  includeScript(path = "www/app.js"), # 
   # shinythemes::themeSelector(),
  navbarPage(
    
            title=div("",style="padding-bottom: 5px"
              # img(src="ASDlog.png",width=120,height=50,style="padding-top: 15px")
              ),
            
            # "The ASD Family\n Morbidity Risk Atlas",
             # theme=shinytheme("flatly"),
             id="MainOverall", #cosmo
             
             
             footer=fluidRow( align="center" ,
                              br(), br(),
                              column(4, offset=4,
                                     hr(),
                                     br(), br(),
                                     "Created by", strong(a("Linda Ejlskov", style="color:lightblue", href="https://scholar.google.dk/citations?user=smPboAcAAAAJ&hl=da")), ".",
                                     br(),
                                     # "Source code available on", strong(a("Github", style="color:lightblue", href="https://github.com/holtzy/the-NB-COMO-Project")), ".",
                                     br(),
                                     "Copyright 2020 The ASD Risk Atlas Project",
                                     br(), br(),br()
                                     
                              ),
                              br(),br()
             ),
             
             # header=	fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",
             #                  hr(),
             #                  helpText(
             #                           "The", style="color:black ; font-family: 'Montserrat'; font-size:35pt;text-transform: uppercase;
             #                            	text-align: center;
             #                            	letter-spacing: .1em;
             #                            	line-height: 1.2;",
             #                           strong("ASD Family Risk Atlas", style="color:black; font-size:45pt"),
             #                           "project", style="color:black ; font-family: 'Montserrat'; font-size:35pt"
             #                  ),
             #                  column(8, offset=2, hr())
             # ),
             
             ##############
             ### Intro ####
             ##############
             tabPanel("Introduction",
                      
                      fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",
                               hr(),
                               # column(width = 2, align = "center",
                               #        img(src="heatmapleft.png", 
                               #            width=100)),
                               column(width = 12, helpText(
                                 "An",br(), style="color:black ; font-family: 'Montserrat'; font-size:30pt;text-transform: uppercase;
                                                          	text-align: center;
                                                          	letter-spacing: .1em;
                                                          	line-height: 1.2;",
                                 strong("Atlas of Risk for Autism", style="color:black; font-size:40pt"),br(),
                                 "from Family Morbidity", style="color:black ; font-family: 'Montserrat'; font-size:30pt"
                               ))
                               # column(width = 2, align = "center",
                               #        img(src="heatmap.png", 
                               #            width=100))
                               ),br(),br(),br(),
                      wellPanel(tags$div(id="pane",fluidRow(width=8,align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%; ",
                                   column(6, offset=3, align="center",
                                          h4("As part of our studies of mental and non-mental disorders in the family and risk for autism, 
                                             we have developed an online interactive tool to better display the results. We hope the tool will enhance your 
                                             navigation through the results, make it easier to make comparisons with results from other studies and stimulate 
                                             hypotheses for future studies.",br(),
                                          "The results we currently show comprise estimates of risk for ASD associated with 90"
                                          ,HTML("<a onclick=","customHref('Methods')" ,">",
                                                "disorders","</a>"),
                                          "in 20 family member types over 3"
                                          ,HTML("<a onclick=","customHref('Methods')" ,">",
                                                "generations.","</a>"),
                                          ),br(),br()
                                          ,bsCollapsePanel("Further information", 
                                                           h4(
                                                             "We estimated a risk for ASD from each disorder in 
                                          each family member type, 1-by-1 for each disorder-family member type combination. For example, 
                                          we calculated an estimate of risk for ASD when a mother had a diagnosis of hypertension, as well as 
                                          risk for ASD when a paternal grandmother had a diagnosis of hypertension. 
                                              We made several different estimates of risk, such as:",
                                                             tags$ul(
                                                               tags$li("risk for ASD overall"),
                                                               tags$li("risk for ASD overall, after accounting for the effects of a family 
                                              member having intellectual disability"),
                                                               tags$li("risk for ASD in males"),
                                                               tags$li("risk for ASD in females"),
                                                               tags$li("risk for ASD with intellectual disability.")),
                                                             "We also made estimates risk for a female or male with ASD to 
                                            be diagnosed with one of the 90 disorders.
                                              ")                                                       
                                                           , style = "default")),
                               tags$style(type="text/css","#pane{font-size:16px;}")
                               )))
                               , hr(),
                      
                     # br(),
                     
                      fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",
                               
                               column(8, offset=2, align="center",
                                      column(3, offset=0, align="center",
                                             
                                             div(img(src="invesLogo.png" 
                                                     , height =80, width =80
                                                 )),
                                             h3("How did we calculate risk?"),
                                             br(),
                                             ),
                                      column(3, offset=0, align="center",
                                             div(img(src="Poplogo.png" 
                                                     , height = 80, width =80
                                             )),
                                             h3("Who was in the study?"),
                                            ),
                                      column(3, offset=0, align="center",
                                             div(img(src="heatLogo.png" 
                                                     , height = 80, width =80
                                             )),
                                             h3("What did we find?"),
                                             ),
                                      column(3, offset=0, align="center",
                                             div(img(src="ideaLogo2.png" 
                                                     , height = 80, width =80
                                             )),
                                             h3("How did we get diagnosis information?"),
                                      ),)
                                  ),
                     
                     fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",

                              column(8, offset=2, align="center",
                                     column(3, offset=0, align="center",
                                            wellPanel("If you are interested in how we calculated the risk estimates",
                                                      br(),br(),br(),br(),br(),
                                                      actionButton('jumpToMethods', 'View methods >>'))

                                     ),
                                     column(3, offset=0, align="center",
                                            wellPanel("If you are interested in who was in the 
                                                      study and how we gathered their morbidity information",
                                                      br(),br(),br(),
                                                      actionButton('jumpToMethods2', 'View methods >>'))
                                     ),
                                     column(3, offset=0, align="center",
                                            wellPanel("If you want to view the results and create your own heat map",
                                                      br(),br(),br(),br(),br(),
                                                      actionButton('jumpToOverallHeat', 'View heatmaps >>'))
                                     ),
                                     column(3, offset=0, align="center",
                                            wellPanel("If you are interested in how we linked family members and diagnoses",
                                                      br(),br(),br(),br(),br(),
                                                      actionButton('jumpToMethods3', 'View methods >>'))
                                     ),
                              )),
                      fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",
                               
                               column(10, offset=1, align="center",
                                      br(),
                                      br(),
                                      br(),
                                      br(), br(), br(), br(),
                                      br(), br(), br(), br(),
                                      br(), br(), br(), br(),
                                      div(img(src="ncrrlog2.png" 
                                              , width = "30%") ,
                                          img(src="ipsy.png" 
                                              , width = "30%") ,
                                          img(src="aulog.png" 
                                              , width = "30%"
                                              # , height = 40, width = 220
                                              ) ,
                                          style="text-align: center;")
                               ))
                               
                      
             ),
             ######################################################################################################
             #### Methods ####
             #################
             tabPanel("Methods",value="Methods",
                      # box(
                      #   title = "Histogram", status = "warning", solidHeader = TRUE,
                      #   collapsible = TRUE,
                      #   "Box content here", br(), "More box content"
                      # )
                      fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",
                               hr(),
                               # column(width = 2, align = "center",
                               #        img(src="heatmapleft.png", 
                               #            width=100)),
                               column(width = 12, helpText(
                                 style="color:black ; font-family: 'Montserrat'; font-size:35pt;text-transform: uppercase;
                                                          	text-align: center;
                                                          	letter-spacing: .1em;
                                                          	line-height: 1.2;",
                                 strong("Methods", style="color:black; font-size:45pt")
                               ))
                               # column(width = 2, align = "center",
                               #        img(src="heatmap.png", 
                               #            width=100))
                      ),br(),
                      fluidPage(
                        fluidRow(column(width=3),column(width=6,
                            bsCollapse(id = "collapseExample",
                                       bsCollapsePanel("Step 1 - Identification of study sample",fluidRow(
                                         column(width=6, 
                                                "Identification of study sample",br(),
                                                "The study sample included all live births born in Denmark from 1 January 1980 through 31 December 2012 and had:",
                                                   tags$ul(
                                                     tags$li("a Denmark identification number, and"),
                                                     tags$li("known parents who were themselves born in Denmark, and"),
                                                     tags$li("birth information in the Danish Medical Birth Register")),

                                                            "The number of births who fulfilled all these criteria and were included in the study = 1,697,231.
                                                            
                                                            The Figure shows the number of births who were excluded at each step of the selection process.
                                                            ")
                                       ,column(width=6,align="center",img(src="Flowchart.png", width ="100%"))), style = "primary"),
                                       bsCollapsePanel("Step 2 - Linkage to family members",fluidRow(
                                         column(width=6, 
                                                "Once we identified the sample of eligible births, we linked each birth to 20 different family member types (alive or dead) 
                                                over 3 generations via linkage of family members through the Danish Civil Registration Service
                                                (to see the list of family member types click below)")
                                         ,column(width=6,align="center",img(src="Familytree2.png", width ="100%"))), style = "primary"),       
                                       bsCollapsePanel("Step 3 - Linkage to diagnoses", 
                                                       "Once we had the family linkages, we linked each person in the family to their health information in the Danish Psychiatric Central
                                                       Research Registry and Danish National Patient Registry.
                                                                ",br(),
                                                            
                                                            "Each person was tracked for each diagnosis over time, beginning at the start of electronic health registries in the 1970s 
                                                       until the person received the diagnosis, emigrated, died or reached the end of the follow-up period at April 10, 2017, which ever 
                                                       event came first. The tracking process was repeated for each diagnosis and each person",br(),
                                                             
                                                            "For each study birth, if more than one sibling, aunt, uncle or cousin had received the same specific morbidity diagnosis (e.g., two full siblings had 
                                                       received an ADHD diagnosis), the family member with the earliest reported diagnosis was chosen for analysis for that specific morbidity."                                        
                                                       , style = "primary"),
                                       bsCollapsePanel("- An overview of included disorders", 
                                                       img(src="DisorderOverview.png", width ="100%")                                                     
                                                       , style = "info"),
                                       bsCollapsePanel("Step 4 - Risk estimate calculation", 
                                                       "To estimate the risk for ASD associated with a specific family member type with a specific diagnosis, we used a statistical method called Cox regression. 
                                                       For each of the 20 family member types, we carried out a separate Cox regression analysis for each of the 90 diagnoses. As part of the analysis we accounted 
                                                       for other factors which could be associated with the risk for ASD in the index child (specifically: year of birth of the child and the child's sex, birth weight,
                                                       gestational age at birth and parent's age at birth of the child. We also took into account the size of the family).",
                                                       br(),br(),
                                                       "Each Cox regression analysis produced a risk estimate for ASD, called an adjusted Hazard Ratio (aHR), associated with a specific family member type-diagnosis 
                                                       combination. For example, an aHR of 2.0 for a diagnosis of hypertension in the mother indicates that the risk for ASD may be twice as high in persons whose mother 
                                                       was diagnosed with hypertension compared to persons whose mother did not have the diagnosis.",
                                                       br(),br(),
                                                       "Separate risk estimates (aHRs) were also calculated for the risk in females and males with ASD of being diagnosed with a specific condition, such as the risk in 
                                                       autistic females of being diagnosed with hypertension.", style = "primary")),column(width=3)
                          ))
                        )
                      
                      
             ),
             
             ######################################################################################################
             #################
             #### HEATMAPS ####
             ################# Guidance ####
             navbarMenu("Heatmaps",
                        tabPanel("Guidance",value="ResDecription",hr(),
                                 fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",
                                          # hr(),
                                          # column(width = 2, align = "center",
                                          #        img(src="heatmapleft.png", 
                                          #            width=100)),
                                          column(width = 12, helpText(
                                            style="color:black ; font-family: 'Montserrat'; font-size:35pt;text-transform: uppercase;
                                                          	text-align: center;
                                                          	letter-spacing: .1em;
                                                          	line-height: 1.2;",
                                            strong("Guidance for using the interactive heatmaps", style="color:black; font-size:45pt")
                                          ))
                                          # column(width = 2, align = "center",
                                          #        img(src="heatmap.png", 
                                          #            width=100))
                                 ),br(),
                                 
                                 fluidPage(
                                   fluidRow(column(width=3),column(width=6,
                                                                   bsCollapse(id = "collapseExample",
                                                                              bsCollapsePanel("What do the colors mean?",
                                                                                       "The heat maps are color-coded. Cells with darker shades of red indicate higher risk estimates (higher aHRs) and a higher risk for ASD. 
                                                                                       Blue shades indicate aHRs below 1.0 and reduced risk for ASD. White cells mean that there were less than  5 persons with ASD in the study 
                                                                                       who had that family member type with that specific diagnosis - that sample size is too small to report a risk estimate.
                                                                                      ",style = "danger"),
                                                                              bsCollapsePanel("Are you looking for the exact risk estimate?",
                                                                                       "Hover your mouse over a cell in a heat map to get a small pop-up window that will present the specific diagnosis, the specific family 
                                                                                       member type and the exact aHR and 95% confidence limits. "),
                                                                              bsCollapsePanel("Do you want a closer look at the heat map?", 
                                                                                              "To zoom into a heat map, simply highlight the area you want to see with the mouse. The resulting section of the heatmap can be 
                                                                                              downloaded by clicking on the camera icon that will appear if you hover your mouse to the right of the heat map title."                                                       
                                                                                              , style = "primary"),
                                                                              bsCollapsePanel("Do you want to create your own heat map?", 
                                                                                              "In the grey panel to the left of a heat map are two tabs:",br(),
                                                                                              "Select diagnoses and Select family member types.",br(), 
                                                                                              "You can use these tabs to custom-make your own heat map.",br(), 
                                                                                              "Click on 'Select diagnosis' to select the specific diagnoses you want to display. You can either select all diagnoses in a major 
                                                                                              diagnosis category or, by clicking on 'Select one or more diagnoses' in the box, you will get a drop down menu of individual diagnoses 
                                                                                              to choose from. ",br(),
                                                                                              "Click on 'Select family member types' to select the family member types you want to display. ",br(),
                                                                                              "After selecting the diagnoses and family member types, click on 'Plot Heatmap '.The resulting heatmap can be downloaded by clicking on 
                                                                                              the camera icon that will appear if you hover your mouse to the right of the heat map title.
                                                                                              "
                                                                                              , style = "info")
                                                                              ))))),
                        
                        tabPanel("ASD overall",value="OverallHeat",hr(),
                                 br(),br(),
                                 # ===  HEATMAP OVERALL##################################
                                 fluidRow(align="center",
                                          column(12, offset=0, align="center",
                                                 column(3, offset=0, align="center",
                                                        # h3("Create your own graph"),
                                                        # HTML("First, select the specific diagnosis you want to display
                                                        #   in the box below.<br> After selecting the diagnoses click on the family member types
                                                        #   tab and select the family member types you want to display the diagnoses for. 
                                                        #   <br>Then click on 'Plot Heatmap' "),
                                                        # br(),
                                                  ),
                                                 column(1, offset=0, align="center"
                                                        ),
                                                 column(7, offset=0, align="center",
                                                        h3("The risk atlas for ASD overall"),
                                                        HTML("Each cell in the heat map displays the adjusted hazard ratio (aHR) for ASD from a specific family member 
                                                        type-diagnosis combination.
                                                        <br>Darker shades of red indicate higher aHR's (higher risk for ASD). 
                                                        <br>Hover your mouse over a cell to get the exact aHR.
                                                        <br>To zoom in, highlight an area with the mouse.
                                                        <br>The graph can be downloaded by clicking on the camera icon that will appear if you hover 
                                                             your mouse to the right of the heat map title")
                                                    ),
                                                 column(1, offset=0, align="center"
                                                 ),
                                                 )
                                             ),
                                 useShinyjs(),
                                 sidebarLayout(
                                   sidebarPanel(width=3,
                                                h3("Create your own graph"),
                                                HTML("First, in the box below select the specific diagnosis you want to display.<br> 
                                                      After selecting diagnoses click on the family member types tab and select the family 
                                                      member types you want to display. 
                                                          <br>Then click on 'Plot Heatmap' "),
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Select entire diagnosis category",
                                                          div(br(),br(),
                                                              checkboxGroupInput("diagnoses_groups","Select entire diagnosis type:",
                                                                                 c("Mental", "Neurologic",
                                                                                   "Cardiometabolic","Birth defect"
                                                                                   ,"Autoimmune","Other"),inline=F),
                                                              selectizeInput("selectdis", "Select specific diagnoses. To remove a diagnosis, click on it and press delete", rownames(X),width = "100%", multiple = TRUE
                                                                             , options = list(placeholder = 'Select one or more diagnoses')),br(),br(),
                                                              actionButton("resetDis", "Clear diagnoses"))
                                                 ),br(),
                                                 tabPanel("Select Family types",
                                                          div(
                                                            selectInput("selectfam", "Select specific family member types (at least 2)", colnames(X), multiple = TRUE),
                                                            verbatimTextOutput("select")
                                                          ),
                                                          actionButton("resetFam", "Clear family types"),
                                                          br(),
                                                          br(),
                                                          br(),
                                                          br(),
                                                          actionButton("go", "Plot heatmap", icon("paper-plane")
                                                                       ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                 )
                                     )
                                     
                                     
                                   ),
                                   mainPanel(width=9,align="center",
                                     plotlyOutput("heatmap", height = 900, width = 1000)
                                   )
                                 )
                        ),
                        # ===  HEATMAP Women ##################################
                        tabPanel("ASD in females",value="OverallHeatWomen",hr(),
                                 br(),br(),
                                 fluidRow(align="center",
                                          column(12, offset=0, align="center",
                                                 column(3, offset=0, align="center",
                                                 ),
                                                 column(1, offset=0, align="center"
                                                 ),
                                                 column(7, offset=0, align="center",
                                                        h3("The risk atlas for ASD in females"),
                                                        HTML("Each cell in the heat map displays the adjusted hazard ratio (aHR) for ASD from a 
                                                        specific family member type-diagnosis combination
                                                        <br>Darker shades of red indicate higher aHR's (higher risk for ASD). 
                                                        <br>Hover your mouse over a cell to get the exact aHR.
                                                        <br>To zoom in, highlight an area with the mouse.
                                                        <br>The graph can be downloaded by clicking on the camera icon that will appear if you hover 
                                                             your mouse to the right of the heat map title")
                                                 ),
                                                 column(1, offset=0, align="center"
                                                 ),)),
                                 useShinyjs(),
                                 sidebarLayout(
                                   sidebarPanel(width=3,
                                                h3("Create your own graph"),
                                                HTML("First, in the box below select the specific diagnosis you want to display.<br> 
                                                      After selecting diagnoses click on the family member types tab and select the family 
                                                      member types you want to display. 
                                                          <br>Then click on 'Plot Heatmap' "),
                                                tabsetPanel(type="tabs",
                                                            tabPanel("Select diagnoses",
                                                                     div(br(),br(),
                                                                         checkboxGroupInput("diagnoses_groupsWomen","Select entire diagnosis type:",
                                                                                            c("Mental", "Neurologic",
                                                                                              "Cardiometabolic","Birth defect"
                                                                                              ,"Autoimmune","Other"),inline=F),
                                                                         selectizeInput("selectdisWomen", "Select specific diagnoses. To remove a diagnosis, click on it and press delete", rownames(XWomen),
                                                                                        width = "100%", multiple = TRUE
                                                                                        , options = list(placeholder = 'Select one or more diagnoses')),br(),br(),
                                                                         actionButton("resetDisWomen", "Clear diagnoses"))
                                                            ),br(),
                                                            tabPanel("Select Family types",
                                                                     div(
                                                                       selectInput("selectfamWomen", "Select specific family member types (at least 2)", colnames(XWomen), multiple = TRUE),
                                                                       verbatimTextOutput("selectWomenFam")
                                                                     ),
                                                                     actionButton("resetFamWomen", "Clear family types"),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     actionButton("goWomen", "Plot heatmap", icon("paper-plane")
                                                                                  ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                            ))),
                                   mainPanel(width=9,align="center",
                                             plotlyOutput("heatmapWomen", height = 900, width = 1000)
                                   ))),
                        
                        # ===  HEATMAP Men ##################################
                        tabPanel("ASD in males",value="OverallHeatMen",hr(),
                                 br(),br(),
                                 fluidRow(align="center",
                                          column(12, offset=0, align="center",
                                                 column(3, offset=0, align="center",
                                                 ),
                                                 column(1, offset=0, align="center"
                                                 ),
                                                 column(7, offset=0, align="center",
                                                        h3("The risk atlas for ASD in males"),
                                                        HTML("Each cell in the heat map displays the adjusted hazard ratio (aHR) for ASD from a 
                                                        specific family member type-diagnosis combination
                                                        <br>Darker shades of red indicate higher aHR's (higher risk for ASD). 
                                                        <br>Hover your mouse over a cell to get the exact aHR.
                                                        <br>To zoom in, highlight an area with the mouse.
                                                        <br>The graph can be downloaded by clicking on the camera icon that will appear if you hover 
                                                             your mouse to the right of the heat map title.")
                                                 ),
                                                 column(1, offset=0, align="center"
                                                 ),)),
                                 useShinyjs(),
                                 sidebarLayout(
                                   sidebarPanel(width=3,
                                                h3("Create your own graph"),
                                                HTML("First, in the box below select the specific diagnosis you want to display.<br> 
                                                      After selecting diagnoses click on the family member types tab and select the family 
                                                      member types you want to display. 
                                                          <br>Then click on 'Plot Heatmap' "),
                                                tabsetPanel(type="tabs",
                                                            tabPanel("Select diagnoses",
                                                                     div(br(),br(),
                                                                         checkboxGroupInput("diagnoses_groupsMen","Select entire diagnosis type:",
                                                                                            c("Mental", "Neurologic",
                                                                                              "Cardiometabolic","Birth defect"
                                                                                              ,"Autoimmune","Other"),inline=F),
                                                                         selectizeInput("selectdisMen", "Select specific diagnoses. To remove a diagnosis, click on it and press delete", rownames(XMen),
                                                                                        width = "100%", multiple = TRUE
                                                                                        , options = list(placeholder = 'Select one or more diagnoses')),br(),br(),
                                                                         actionButton("resetDisMen", "Clear diagnoses"))
                                                            ),br(),
                                                            tabPanel("Select Family types",
                                                                     div(
                                                                       selectInput("selectfamMen", "Select specific family member types (at least 2)", colnames(XMen), multiple = TRUE),
                                                                       verbatimTextOutput("selectMenFam")
                                                                     ),
                                                                     actionButton("resetFamMen", "Clear family types"),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     actionButton("goMen", "Plot heatmap", icon("paper-plane")
                                                                                  ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                            ))),
                                   mainPanel(width=9,align="center",
                                             plotlyOutput("heatmapMen", height = 900, width = 1000)
                                   ))),
                        # ===  HEATMAP ASDID ##################################
                        tabPanel("ASD with intellectual disability (ID)",value="OverallHeatASDID",hr(),
                                 br(),br(),
                                 fluidRow(align="center",
                                          column(12, offset=0, align="center",
                                                 column(3, offset=0, align="center",
                                                 ), 
                                                 column(1, offset=0, align="center"
                                                 ),
                                                 column(7, offset=0, align="center",
                                                        h3("The risk for ASD with intellectual disability"),
                                                        HTML("Each cell in the heat map displays the adjusted hazard ratio (aHR) for ASD from a 
                                                        specific family member type-diagnosis combination
                                                        <br>Darker shades of red indicate higher aHR's (higher risk for ASD). 
                                                        <br>Hover your mouse over a cell to get the exact aHR.
                                                        <br>To zoom in, highlight an area with the mouse.
                                                        <br>The graph can be downloaded by clicking on the camera icon that will appear if you hover 
                                                             your mouse to the right of the heat map title.")
                                                 ),
                                                 column(1, offset=0, align="center"
                                                 ),)),
                                 useShinyjs(),
                                 sidebarLayout(
                                   sidebarPanel(width=3,
                                                h3("Create your own graph"),
                                                HTML("First, in the box below select the specific diagnosis you want to display.<br> 
                                                      After selecting diagnoses click on the family member types tab and select the family 
                                                      member types you want to display. 
                                                          <br>Then click on 'Plot Heatmap' "),
                                                tabsetPanel(type="tabs",
                                                            tabPanel("Select diagnoses",
                                                                     div(br(),br(),
                                                                         checkboxGroupInput("diagnoses_groupsASDID","Select entire diagnosis type:",
                                                                                            c("Mental", "Neurologic",
                                                                                              "Cardiometabolic","Birth defect"
                                                                                              ,"Autoimmune","Other"),inline=F),
                                                                         selectizeInput("selectdisASDID", "Select specific diagnoses. To remove a diagnosis, click on it and press delete", rownames(XASDID),
                                                                                        width = "100%", multiple = TRUE
                                                                                        , options = list(placeholder = 'Select one or more diagnoses')),br(),br(),
                                                                         actionButton("resetDisASDID", "Clear diagnoses"))
                                                            ),br(),
                                                            tabPanel("Select Family types",
                                                                     div(
                                                                       selectInput("selectfamASDID", "Select specific family member types (at least 2)", colnames(XASDID), multiple = TRUE),
                                                                       verbatimTextOutput("selectASDIDFam")
                                                                     ),
                                                                     actionButton("resetFamASDID", "Clear family types"),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     actionButton("goASDID", "Plot heatmap", icon("paper-plane")
                                                                                  ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                            ))),
                                   mainPanel(width=9,align="center",
                                             plotlyOutput("heatmapASDID", height = 900, width = 1000)
                                   ))),
                        
                        # ===  HEATMAP IDconf ##################################
                        tabPanel("ASD overall adjusting for family intellectual disability",value="OverallHeatIDconf",hr(),
                                 br(),br(),
                                 fluidRow(align="center",
                                          column(12, offset=0, align="center",
                                                 column(3, offset=0, align="center",
                                                 ),
                                                 column(1, offset=0, align="center"
                                                 ),
                                                 column(7, offset=0, align="center",
                                                        h3("The risk atlas for ASD after accounting for intellectual disability in the family member"),
                                                        HTML("Each cell in the heat map displays the adjusted hazard ratio (aHR) for ASD from a 
                                                        specific family member type-diagnosis combination
                                                        <br>Darker shades of red indicate higher aHR's (higher risk for ASD). 
                                                        <br>Hover your mouse over a cell to get the exact aHR.
                                                        <br>To zoom in, highlight an area with the mouse.
                                                        <br>The graph can be downloaded by clicking on the camera icon that will appear if you hover 
                                                             your mouse to the right of the heat map title.")
                                                 ),
                                                 column(1, offset=0, align="center"
                                                 ),)),
                                 useShinyjs(),
                                 sidebarLayout(
                                   sidebarPanel(width=3,
                                                h3("Create your own graph"),
                                                HTML("First, in the box below select the specific diagnosis you want to display.<br> 
                                                      After selecting diagnoses click on the family member types tab and select the family 
                                                      member types you want to display. 
                                                          <br>Then click on 'Plot Heatmap' "),
                                                tabsetPanel(type="tabs",
                                                            tabPanel("Select diagnoses",
                                                                     div(br(),br(),
                                                                         checkboxGroupInput("diagnoses_groupsIDconf","Select entire diagnosis type:",
                                                                                            c("Mental", "Neurologic",
                                                                                              "Cardiometabolic","Birth defect"
                                                                                              ,"Autoimmune","Other"),inline=F),
                                                                         selectizeInput("selectdisIDconf", "Select specific diagnoses. To remove a diagnosis, click on it and press delete", rownames(XIDconf),
                                                                                        width = "100%", multiple = TRUE
                                                                                        , options = list(placeholder = 'Select one or more diagnoses')),br(),br(),
                                                                         actionButton("resetDisIDconf", "Clear diagnoses"))
                                                            ),br(),
                                                            tabPanel("Select Family types",
                                                                     div(
                                                                       selectInput("selectfamIDconf", "Select specific family member types (at least 2)", colnames(XIDconf), multiple = TRUE),
                                                                       verbatimTextOutput("selectIDconfFam")
                                                                     ),
                                                                     actionButton("resetFamIDconf", "Clear family types"),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     actionButton("goIDconf", "Plot heatmap", icon("paper-plane")
                                                                                  ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                            ))),
                                   mainPanel(width=9,align="center",
                                             plotlyOutput("heatmapIDconf", height = 900, width = 1000)
                                   ))),
                        ######## Data Source ######
                        tabPanel("Data sources",value="",
                                 # box(
                                 #   title = "Histogram", status = "warning", solidHeader = TRUE,
                                 #   collapsible = TRUE,
                                 #   "Box content here", br(), "More box content"
                                 # )
                                 fluidRow(align="center", style="opacity:0.9 ;margin-top: 0px; width: 100%;",
                                          hr(),
                                          # column(width = 2, align = "center",
                                          #        img(src="heatmapleft.png", 
                                          #            width=100)),
                                          column(width = 12, helpText(
                                            style="color:black ; font-family: 'Montserrat'; font-size:35pt;text-transform: uppercase;
                                                          	text-align: center;
                                                          	letter-spacing: .1em;
                                                          	line-height: 1.2;",
                                            strong("Data sources", style="color:black; font-size:45pt")
                                          ))
                                          # column(width = 2, align = "center",
                                          #        img(src="heatmap.png", 
                                          #            width=100))
                                 ),br(),
                                 fluidPage(
                                   title = "Data sources of the heatmaps",
                                       tabsetPanel(
                                         id = 'dataset',
                                         tabPanel("ASD overall", DT::dataTableOutput("mytable_X_tab")),
                                         tabPanel("ASD in females", DT::dataTableOutput("mytable_XWomen_tab")),
                                         tabPanel("ASD in males", DT::dataTableOutput("mytable_XMen_tab")),
                                         tabPanel("ASD with intellectual disability (ID)", DT::dataTableOutput("mytable_XASDID_tab")),
                                         tabPanel("ASD overall adjusting for family intellectual disability", DT::dataTableOutput("mytable_XIDConf_tab"))
                                       
                                     )
                                   )
                                 )
             ),
             ########################### Further information ###############################
            tabPanel("Further information",value="FurtherInfo",
                     br(),br(),
                     
                     # fluidRow( align="left",
                     #           column(6, offset=3,
                     #                  h2("Read the paper at:"),
                     #                  hr(),
                     #                  tags$ul(
                     #                    tags$li(h5(a("Journal of...",href="https://pure.au.dk/portal/da/persons/diana-schendel(2163da2c-859c-46e1-b464-d1c36b753696).html"))),
                     #                  )
                     #           )
                     # ),br(),br(),  
                     
                     fluidRow( align="left",
                               column(6, offset=3,
                                      h2("Meet the core team"),
                                      hr(),
                                      tags$ul(
                                        tags$li(h5(a("Diana Schendel",href="https://pure.au.dk/portal/da/persons/diana-schendel(2163da2c-859c-46e1-b464-d1c36b753696).html"))),
                                        tags$li(h5(a("Linda Ejlskov",href="https://pure.au.dk/portal/da/persons/linda-ejlskov(81c4e051-7d5c-4eda-88e4-7f2f5a4b0db2).html"))),
                                      )
                               )
                     ),
                     br(),br(),br(),br(),      
                     fluidRow( align="left",
                               column(6, offset=3,
                                      h2("Meet the collaborators"),
                                      hr(),
                                      tags$ul(
                                        tags$li(h5("Morten Overgaard, PhD")),
                                        tags$li(h5("Erik Parner, PhD")),
                                        tags$li(h5("Amy Kalkbrenner, PhD")),
                                        tags$li(h5("Christine Ladd Acosta, PhD")),
                                        tags$li(h5("M Danielle Fallin, PhD")),
                                        tags$li(h5("Sherlly Xie, PhD")),
                                        tags$li(h5("Preben Bo Mortensen, DrMedSci")),
                                        tags$li(h5("Brian Lee, PhD")),
                                        )
                               )
                     ),
                     br(),br(),br(),br(),      
                     fluidRow( align="left",
                               column(6, offset=3,
                                      h2("A special thanks to"),
                                      hr(),
                                      tags$ul(
                                        tags$li(h5(a("National Institute of Environmental Health Sciences (NIEHS)"))),
                                        tags$li(h5(a("Lundbeck Foundation Initiative for Integrative Psychiatric Research (iPSYCH)"))),
                                        tags$li(h5(a("National Centre for Register-based Research, Aarhus University (NCRR)"))),
                                        tags$li(h5(a("Aarhus University")))
                                      )
                               )
                     ),
                        )
            ))



shinyApp(ui=ui,server=server)

# deployApp()
