# shinyApp(ui, server)

######################################
######################################

#rm(list=ls())
#dev.off()


clust <- read.csv(file= "____/cluster_chord.csv", header = T)

clustdata <- clust[,c(1,2,7,3,4,5,6)]
names(clustdata) <- c("Health.Plan.Name", "Cluster.Number", "Cluster.Name", "Coaching", "Online.Class", "Connected", "Challenge")

client.list <- c("Standalone Employer Groups", "ExxonMobil - Health and Wellness", "Exelon", "PepsiCo", "Nordstrom, Inc", "MVP/Preferred Care", "Ingersoll Rand")

data_SEG <- clustdata[which(clustdata$Health.Plan.Name == client.list[1]),]
data_Exx <- clustdata[which(clustdata$Health.Plan.Name == client.list[2]),]
data_Exe <- clustdata[which(clustdata$Health.Plan.Name == client.list[3]),]
data_Pep <- clustdata[which(clustdata$Health.Plan.Name == client.list[4]),]
data_Nord <- clustdata[which(clustdata$Health.Plan.Name == client.list[5]),]
data_MVP <- clustdata[which(clustdata$Health.Plan.Name == client.list[6]),]
data_IR <- clustdata[which(clustdata$Health.Plan.Name == client.list[7]),]

attach(clustdata)

col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))

mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))

namecounts <- data.frame(table(Cluster.Number))

mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]

mytable_percent <- round(mytable_percent,2)

mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")

mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_all <- mytable_t
mytable_percent_t_all <- mytable_percent_t

#chorddiag(mytable_t_all, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 65)
#chorddiag(mytable_t_all, type = "bipartite", palette = "BrBG", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 65)
#chorddiag(mytable_t_all, type = "bipartite", palette = "BrBG", palette2 = "Blues", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 65)
#chorddiag(mytable_t_all, type = "bipartite", groupColors = "Reds", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 65)
#chorddiag(mytable_percent_t_all, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 65)



attach(data_SEG)
col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))
mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))
namecounts <- data.frame(table(Cluster.Number))
mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]
mytable_percent <- round(mytable_percent,2)
mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_SEG <- mytable_t
mytable_percent_t_SEG <- mytable_percent_t



attach(data_Exx)
col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))
mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))
namecounts <- data.frame(table(Cluster.Number))
mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]
mytable_percent <- round(mytable_percent,2)
mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_Exx <- mytable_t
mytable_percent_t_Exx <- mytable_percent_t



attach(data_Exe)
col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))
mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))
namecounts <- data.frame(table(Cluster.Number))
mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]
mytable_percent <- round(mytable_percent,2)
mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_Exe <- mytable_t
mytable_percent_t_Exe <- mytable_percent_t



attach(data_Pep)
col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))
mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))
namecounts <- data.frame(table(Cluster.Number))
mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]
mytable_percent <- round(mytable_percent,2)
mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_Pep <- mytable_t
mytable_percent_t_Pep <- mytable_percent_t



attach(data_Nord)
col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))
mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))
namecounts <- data.frame(table(Cluster.Number))
mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]
mytable_percent <- round(mytable_percent,2)
mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_Nord <- mytable_t
mytable_percent_t_Nord <- mytable_percent_t



attach(data_MVP)
col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))
mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))
namecounts <- data.frame(table(Cluster.Number))
mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]
mytable_percent <- round(mytable_percent,2)
mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_MVP <- mytable_t
mytable_percent_t_MVP <- mytable_percent_t



attach(data_IR)
col_Coaching <- as.vector(data.frame(table(Cluster.Number,Coaching))[c(6:10),3])
col_Online.Class <- as.vector(data.frame(table(Cluster.Number,Online.Class))[c(6:10),3])
col_Connected <- as.vector(data.frame(table(Cluster.Number,Connected))[c(6:10),3])
col_Challenge <- as.vector(data.frame(table(Cluster.Number,Challenge))[c(6:10),3])
empty <- as.vector(rep(0,5))
mytable <- cbind(col_Connected, col_Coaching, col_Online.Class, col_Challenge, empty)
row.names(mytable) <- sort(unique(Cluster.Number))
namecounts <- data.frame(table(Cluster.Number))
mytable_percent <- mytable*0
mytable_percent[1,] <- mytable[1,]/namecounts[1,2]
mytable_percent[2,] <- mytable[2,]/namecounts[2,2]
mytable_percent[3,] <- mytable[3,]/namecounts[3,2]
mytable_percent[4,] <- mytable[4,]/namecounts[4,2]
mytable_percent[5,] <- mytable[5,]/namecounts[5,2]
mytable_percent <- round(mytable_percent,2)
mytable_t <- t(mytable)
row.names(mytable_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_percent_t <- t(mytable_percent)
row.names(mytable_percent_t) <- c("Connected", "Coaching", "Online.Class", "Challenge", "")
mytable_t_IR <- mytable_t
mytable_percent_t_IR <- mytable_percent_t





library(shiny)
library(chorddiag)
library(leaflet)
library(RColorBrewer)




ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(  
      radioButtons('select_client',"Select Health Plan",inline = F,
                   choices = c("All", "Group A", "Group B", "Group C", "Group D", "Group E", "Group F", "Group G"),
                   selected = 'All') ,
      
      radioButtons('select_data_type',"Select Data Type",inline = F,
                   choices = c("Counts", "Percentages"),
                   selected = 'Counts') ,
      
      selectInput("colors_clust", "Color Scheme - Clusters",
                  rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                  selected = "YlGn"     ),
      
      selectInput("colors_prod", "Color Scheme - Products",
                  rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                  selected = "Greys" ) ,
      
      checkboxInput("legend", "Show key", FALSE) ), 
    
    mainPanel(
      chorddiagOutput("distPlot", height = 800) ) ,
    
    position = c("left", "right") ),
  
  htmlOutput("mykey") )




server <- function(input, output) {
  
  colorpal <- reactive({ colorNumeric(input$colors) })
  
  output$mykey <- renderUI({
    if (input$legend) {
      
      msg1 <- "1 - Females without METS\n"
      msg2 <- "2 - Educated, active males without METS\n"
      msg3 <- "3 - Males that are least educated, heavy drinkers\n"
      msg4 <- "4 - Females with METS\n"
      msg5 <- "5 - Males with METS and poorest metrics\n"
      HTML(paste(msg1, msg2, msg3, msg4, msg5, sep="<br/>"))
    } else "" })  
  
  
  output$distPlot <- renderChorddiag({
    
    groupnameFontsize = 18
    
    if(input$select_client =="All" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_all, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="All" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_all, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group G" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_IR, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group G" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_IR, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group E" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_Nord, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group E" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_Nord, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group A" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_SEG, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group A" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_SEG, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group C" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_Exe, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group C" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_Exe, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group D" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_Pep, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group D" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_Pep, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group F" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_MVP, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group F" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_MVP, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group B" & input$select_data_type == "Counts"){
      chorddiag(mytable_t_Exx, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } else if(
      input$select_client =="Group B" & input$select_data_type == "Percentages"){
      chorddiag(mytable_percent_t_Exx, type = "bipartite", palette = as.character(input$colors_clust), palette2 = as.character(input$colors_prod), showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 75)
    } 
    
  })
}



#shinyApp(ui, server)



# library(shiny)
# library(shinydashboard)
# 
# ui <- dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody()
# )
# 
# server <- function(input, output) { }
# 
# shinyApp(ui, server)





