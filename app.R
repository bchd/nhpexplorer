# Neighborhood Health Profile (NHP) Correlation Explorer
# useR Conference 2022
# This code is for educational purposes and comes with no guarantees!
# Created by: Jonathan Gross, MPH, CPH. E-mail: jonathan.gross@baltimorecity.gov
# Updated on 5/12/2022.

# Before publishing, uncomment install.packages an run line of code once. Then comment back.
# Make sure ShinyApps instance is extra large.  Machine Learning techniques require more memory.
# Otherwise the app may crash, not produce results, or take a long time.
# After running, you may notice a warning message about labeling one of the PCA plots. Some CSA/neighborhood labels are suppressed/not shown.

# Install packages and load libraries
#install.packages(c("ggplot2","plotly","shiny","readxl","xlsx","magritter","DT","tidyverse","rgdal","sf","sp","leaflet"))
library(ggplot2)
library(plotly)
library(shiny)
library(readxl)
library(xlsx)
library(magrittr)
library(DT)
library(tidyverse)
library(rgdal)
library(sf) #preferred over rgdal 
library(sp)
library(leaflet)
library(leaflet.extras2)
library(factoextra)
library(gplots)
library(heatmap3)
library(Hmisc)
library(RColorBrewer)
library(statar)

# Data Import and Management
# Original dataset can be downloaded at:https://health.baltimorecity.gov/neighborhoods/neighborhood-health-profile-reports
nhp<-as.data.frame(read_excel("./data/NHP.xlsx"))

# Drops empty columns imported and makes rownames = CSAs. 
# Renames CSA column for merging with spatial data.
nhp2017<- nhp %>% select(-contains("NA"))
colnames(nhp2017)[colnames(nhp2017) == 'CSA'] <- 'CSA2010'
nhp2017$CSA2010<-as.factor(nhp2017$CSA2010)
                 
  # Saves Baltimore City overall to separate dataset. Can be used for reference lines,etc.
  # Deletes Baltimore from analysis dataset.
  baltimore<-nhp2017%>% dplyr:: filter(CSA2010=="Baltimore City")
  nhp2017<-nhp2017 %>% dplyr::filter(CSA2010!= "Baltimore City")
  rownames(nhp2017)<-nhp2017$CSA2010
  nhp_map<-nhp2017
    
    # For PCA Dropdown Menu: Variable Selector
    for_Dropdown<-nhp2017
    rownames(for_Dropdown)<-for_Dropdown$CSA2010
    for_Dropdown<-for_Dropdown %>% dplyr::select(-CSA2010,-NHPLink)
    
# Resume working on nhp2017 dataset
nhp2017<- nhp2017 %>% select(-c("CSA2010","NHPLink"))

# Convert to numeric: last two variables (lead)
# Prevent error messages from displaying during conversion. Some data format issues.
nhp2017$PercentElevatedBloodLead<-suppressWarnings(as.numeric(nhp2017$PercentElevatedBloodLead))
nhp2017$TestedForElevatedBloodLead<-suppressWarnings(as.numeric(nhp2017$TestedForElevatedBloodLead))

  #NHP Map conversion too
  nhp_map$PercentElevatedBloodLead<-suppressWarnings(as.numeric(nhp_map$PercentElevatedBloodLead))
  nhp_map$TestedForElevatedBloodLead<-suppressWarnings(as.numeric(nhp_map$TestedForElevatedBloodLead))

# Import CSA shapefile for maps and transform
shape<-st_read("./data/Vital_Signs_17_Census_Demographics.shp")
shape<-st_transform(shape,4326)

  # Merge NHP data and CSA data
  shaped<-sp::merge(shape,nhp_map,by=c("CSA2010"))
  
# User Interface
ui <- fluidPage(
  titlePanel("Baltimore City: Neighborhood Health Profiles (2017) Correlation Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      varSelectInput("select",label="Choose First Variable (Explanatory)",selected="MedianHHIncome",data=nhp2017,width=260),
      varSelectInput("select2",label="Choose Second Variable (Outcome)",selected="HomicideRate",data=nhp2017,width=260),
      h4("Explore potential patterns between social determinants of health and health outcomes. This tool uses data from Baltimore City's Neighborhood Profiles (NHP 2017) and 55 Community Statistical Areas (CSAs), which are groupings of census tracts. The NHP have continuous data on a wide range of indicators."),
      a(h4("Neighborhood Health Profiles"), href="https://health.baltimorecity.gov/neighborhoods/neighborhood-health-profile-reports",target="_blank")
                ),
    mainPanel(
      navbarPage("Menu",
                 
                 navbarMenu("Correlations",
                 tabPanel("Correlation Plot",plotlyOutput("plotted"),textOutput("result")),
                 tabPanel("Scatterplot Matrix",h4("Scatterplot matrix, method=Spearman Rank, for variables in the dataset. 
                                                  The range of variables in the matrix is determined by the variables selected at left. It uses the first variable as the start and second as the end variable."),plotOutput("scat_matrix",height=1020)) 
                 ),
                 
                 navbarMenu("Maps",
                  tabPanel("Map of Explanatory",
                           fluidRow(
                           column(12,offset=7,selectInput("sym","Symbology",choice=c("Ranked/Quartiles","Actual Data"),selected="Ranked/Quartiles"))),
                           leafletOutput("map_Exp",height=520)),
                  
                  tabPanel("Map of Outcome",
                           fluidRow(
                           column(12,offset=7,selectInput("sym2","Symbology",choice=c("Ranked/Quartiles","Actual Data"),selected="Ranked/Quartiles"))),
                           leafletOutput("map_Out",height=520)),
                  
                  tabPanel("Map Overlay",h3("Explanantory (left) vs Outcome (Right): Ranked Symbology"),leafletOutput("map_Both",height=520))),
                
                  tabPanel("K-means Clustering",h4("K-means clustering: Usually used for explanatory variables only, but can provide interesting visualizations of outcome and explanatory variables. 
                                                   So, you will likely want to change the outcome variable to an explanatory variable.
                                                   Uses Silhouette method to automatically choose the number of clusters. An unsupervised technique."),
                                                    plotOutput("ml_results",height=420)), 
              
                 navbarMenu("Principal Component Analysis (PCA)",
                  tabPanel("PCA: Use First - Select Variables!",h4("Principal Component Analysis (PCA) can help to sort through explanatory variables.  This is for exploratory data analysis and does not separate data into a training dataset--for making predictions."),
                           varSelectInput("pca1",label="Choose multiple variables for PCA",multiple=TRUE,
                                                          selected=(c("MedianHHIncome","LeadPaintViolationRate","PercentAbsentHigh","AlcoholDensity","PercentHispanic")),data=nhp2017,width=400),
                           varSelectInput("pca_Outcome",label="Choose an Outcome to Group PCA Graphical results",selected=c("HomicideRate"),data=nhp2017,width=400),
                           numericInput("rank_num","How many groups/ranks for your Outcome (Only used in last graph).",value=3),
                           a(h4("Click for More Information on PCA"),href=" http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp",target="_blank")),
                  
                  tabPanel("PCA: Scree Plot",h4("Scree plot shows how much variation is explanined by each Principal Component."),plotOutput("scree_plot")),
                  tabPanel("PCA: PC Contribution to CSAs",h4("This graph shows how much each Principal Component contributes to CSA values."), plotOutput("pca_CSA",height=520)),
                  tabPanel("PCA: Does PCA help us Cluster?",h4("Ideally, you want to see separate clusters or low amounts of overlap between clusters on this graph.",plotOutput("pca_CSA2",height=520))
                  #tabPanel("PCA Partition and Predict")
                  #tabPanel("Regression")
                 )
                  )
      ))
              )
)
                
server <- function(input, output) {
  
  # Reactive datasets
  # Tabular Data
  dataInput<-reactive({
    nhp2017
  })
  
  # For Mapping
  dataShaped<-reactive({
    shaped
  })
  
  # Correlation Plot and Statistics ----------------------------------------
  output$plotted<-renderPlotly({
    
    # Correlation Plot
    scat<-ggplot(dataInput(),aes(x=!!input$select,y=!!input$select2))+geom_point()
    p<-ggplotly(scat)
    text<-paste("CSA: ",rownames(nhp2017),"<br>",input$select,":", round(nhp2017[[input$select]],digits=2),"<br>", input$select2, ":", round(nhp2017[[input$select2]],digits=2) )
    style(p,text=text)
})
  
  # Correlation Statistics
  output$result<-renderText({
    calc_cor<-cor.test(dataInput()[[input$select]],dataInput()[[input$select2]], method="pearson",exact=FALSE)
    calc_cor2<-cor.test(dataInput()[[input$select]],dataInput()[[input$select2]],method=("spearman"),exact=FALSE)
    paste("Pearson's R: ",round(calc_cor$estimate,2)," , P-value:",format(round(calc_cor$p.value,10),scientific=FALSE,digits=1),"    |  Spearman's Rho: ",round(calc_cor2$estimate,2)," , P-value:",format(round(calc_cor2$p.value,10),scientific=FALSE,digits=1))
  })

  # Scatterplot matrix
  output$scat_matrix<-renderPlot({
    nhp2017_filtered<-dataInput()%>% dplyr::select(input$select:input$select2)
    nhp2017_mat<-data.matrix(nhp2017_filtered)
    nhp_full_mat<-rcorr(nhp2017_mat,type="spearman")
    heatmap.2(nhp_full_mat[["r"]],Rowv=NA, Colv=NA,na.rm=TRUE,margins=c(10,10),dendrogram = "none",trace="none",keysize=.75,col=rev(brewer.pal(7,"RdYlBu")),cellnote = round(nhp_full_mat[["r"]],digits=2),notecol="black",notecex=.9)
  })
  
  # Maps ----------------------------------------
  
  # Explanatory Map
  output$map_Exp<-renderLeaflet({
    map_exp_label<-paste0("CSA: ",dataShaped()$CSA2010,"<br>",input$select,": ",round(dataShaped()[[input$select]],digits=2))
    exp_map<-leaflet() %>% addTiles() %>% setView(lng = -76.6122, lat = 39.2904, zoom = 12)
    
    # Changes Map Symbology based on drop-down menu
    if (input$sym=="Ranked/Quartiles"){
      exp_map %>% addPolygons(data=dataShaped(),color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", dataShaped()[[input$select]])(dataShaped()[[input$select]]),
                  highlightOptions=highlightOptions(color="white",weight=2,bringToFront = TRUE),label=lapply(map_exp_label,HTML),labelOptions=labelOptions(style=list("font-size"="14px"))) %>%
                  addLegend(data=dataShaped(),position="bottomright",pal=colorQuantile("YlOrRd",dataShaped()[[input$select]]),values=~dataShaped()[[input$select]],title = paste(input$select))
    
      } else {
      pal<-colorNumeric(palette = "Blues",domain=dataShaped()[[input$select]])
      
      exp_map %>% addPolygons(data=dataShaped(),color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                  fillColor =~ pal(dataShaped()[[input$select]]),
                  highlightOptions=highlightOptions(color="white",weight=2,bringToFront = TRUE),label=lapply(map_exp_label,HTML)) %>%
                  addLegend(data=dataShaped(),position="bottomright",pal=pal,values=dataShaped()[[input$select]],title = paste(input$select))
        
    }
})

  # Outcome Map
  output$map_Out<-renderLeaflet({
    map_out_label<-paste0("CSA: ",dataShaped()$CSA2010,"<br>",input$select2,":",round(dataShaped()[[input$select2]], digits=2))
    out_map<-leaflet() %>%  addTiles() %>% setView(lng = -76.6122, lat = 39.2904, zoom = 12) 
    
    # Changes Map Symbology based on drop-down menu
    if (input$sym2=="Ranked/Quartiles"){
      out_map %>% addPolygons(data=dataShaped(),color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                              fillColor = ~colorQuantile("YlOrRd", dataShaped()[[input$select2]])(dataShaped()[[input$select2]]),
                              highlightOptions=highlightOptions(color="white",weight=2,bringToFront = TRUE),label=lapply(map_out_label,HTML),labelOptions=labelOptions(style=list("font-size"="14px"))) %>%
        addLegend(data=dataShaped(),position="bottomright",pal=colorQuantile("YlOrRd",dataShaped()[[input$select2]]),values=~dataShaped()[[input$select2]],title = paste(input$select2))
      
    } else {
      pal<-colorNumeric(palette = "Blues",domain=dataShaped()[[input$select2]])
      
      out_map %>% addPolygons(data=dataShaped(),color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                              fillColor =~ pal(dataShaped()[[input$select2]]),
                              highlightOptions=highlightOptions(color="white",weight=2,bringToFront = TRUE),label=lapply(map_out_label,HTML)) %>%
        addLegend(data=dataShaped(),position="bottomright",pal=pal,values=dataShaped()[[input$select2]],title = paste(input$select2))
      
    }
  })
  
  # Side-by-side map 
  output$map_Both<-renderLeaflet({
    leaflet(dataShaped())%>%
      setView(lng = -76.6122, lat = 39.2904, zoom = 12) %>%
      addMapPane("left",zIndex=0)%>%
      addMapPane("right",zIndex=0)%>%
      addTiles(group="base2",layerId="baseid2",options=pathOptions(pane="left")) %>%
      addTiles(group="base",layerId="baseid",options=pathOptions(pane="right")) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", dataShaped()[[input$select]])(dataShaped()[[input$select]]),options=pathOptions(pane="right"),group="right2",dashArray = NULL)%>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", dataShaped()[[input$select2]])(dataShaped()[[input$select2]]),options=pathOptions(pane="left"),group="left2",dashArray = NULL) %>%
      addSidebyside(leftId="baseid",rightId="baseid2")
  })
  
  # Machine Learning ----------------------------------
  
  # K-means Clustering
  # Reference: https://uc-r.github.io/kmeans_clustering
  # Reference: https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
  
  output$ml_results<-renderPlot({
  k_data<-dataInput() %>% dplyr::select(input$select,input$select2)
  k_data$CSA2010<-row.names(k_data)
  k_data<-na.omit(k_data)
  k_data<-k_data %>% dplyr::select(-CSA2010)
  k_data<-scale(k_data)
  
  # Check for optimal number of clusters using silhouette method.
  # Stores variable as max_cluster
  ml_result<-fviz_nbclust(k_data, kmeans, method = c("silhouette"))+geom_vline(xintercept = 4, linetype = 2)
  clust<-ml_result$data
  max_cluster<-as.numeric(clust$clusters[which.max(clust$y)])
  
  # set seed used for random start since position in dataset can affect clustering.
  # Also allows for reproducibility. 
  set.seed(12345)
  km.res <- kmeans(k_data, max_cluster, nstart = 25)
  
  # Print the results
  #print(km.res)
  
  # Plot results
  fviz_cluster(km.res,k_data,labelsize=13,
               ggtheme=theme_bw(),main="Cluster Plot of Standardized Values")+theme(text=element_text(size=12))
 
  })
  
 
  # Principal Component Analysis (PCA) ---------------------------
  # Reference: http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp
  pca_Results<-reactive({
  pca_Data<-dataInput()
  pca_Ready<-subset(pca_Data,select=c(as.character(input$pca1))) #dplyr::select(as.character(input$pca1[1]),as.character(input$pca1[2]))
  prcomp(pca_Ready,scale=TRUE)
  })
  
  output$scree_plot<-renderPlot({
    #cat(input$pca1[1])
    fviz_eig(pca_Results())+theme(text=element_text(size=14))
  })
  
  output$pca_CSA<-renderPlot({
    pca_Data<-dataInput()
    fviz_pca_ind(pca_Results(),col.ind="cos2",repel=2,gradient.cols=c("#00AFBB","#E7B800","#FC4E07")) 
  })
  
  output$pca_CSA2<-renderPlot({
    pca_Outcome<-dataInput() %>% dplyr::select(input$pca_Outcome)
    pca_Outcome$cat<-as.character(xtile(pca_Outcome[,1],input$rank_num))
    pca_Outcome$cat<-as.factor(pca_Outcome$cat)
    fviz_pca_ind(pca_Results(),col.ind="cos2",repel=8,habillage=pca_Outcome$cat,addEllipses = TRUE,ellipse.level=0.95) 
  })
 
}

shinyApp(ui = ui, server = server)

### END OF PROGRAM