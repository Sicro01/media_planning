#####################################################################
#####################################################################
##                                                                 ##
##  +-----------------------------------------------------------+  ##
##  | Walmart US - Item File Error Identification / AP Disputes |  ##
##  +-----------------------------------------------------------+  ##
##                                                                 ##
##  Copyright (C) 2019 by Genpact                                  ##
##                                                                 ##
##  Contact David Hauser (david.hauser@genpact.com) for more       ##
##  information or a demonstration.                                ##
##                                                                 ##
##  Algorithm to determine which Item File line items are likely   ##
##  to have pricing errors by demonstrating a variety of manual    ##
##  and automated forensic techniques.                             ##
##                                                                 ##
##  UNDERLYING FUNCTIONS                                           ##
##  --------------------                                           ##
##  Run this code before launching UI (below).                     ##
##                                                                 ##
#####################################################################
#####################################################################


#################################
# Step 0: Initialized Libraries #
#################################
Step_0_Initialize <- function() {
  library(ggplot2)
  library(dplyr)
  library(plyr)
  #library(ggpubr)
  library(tibble)
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(corrplot)
  library(stringr)
}


###################################
# Step 1: Import Master Item File #
###################################
Step_1_Import_Item_File <- function() {
  #--------#
  # Import #
  #--------#
  
  FileName   <- "C:/Users/770000411/OneDrive - Genpact/01 - Work/02 - Clients/Walmart/00 - Cash Associate Risk/Data/Item_file_merged.csv"
  Item_File  <- read.csv(FileName, header = TRUE, sep = ",")
  
  #------------------------------------#
  # Keep only Key Columns of Item File #
  #------------------------------------#
  Item_File <- Item_File[,c("POGENPAC.CC", 
                            "POGENPAC.BUS.TYPE", 
                            "POGENPAC.VNDR.NBR", 
                            "POGENPAC.VNDR.NAME", 
                            "WMT.ITEM.NUMBER", 
                            "SYS.ITEM.NUMBER", 
                            "UPC.NUMBER", 
                            "UPC.DESC", 
                            "STORE.NBR", 
                            "ITEM.DESC", 
                            "PRODUCT.NUMBER", 
                            "PRODUCT.DESC", 
                            "ITEM.BASE.UNIT.RETAIL", 
                            "SKU.COST.AMT", 
                            "SKU.SELL.RETAIL.AMT", 
                            "SKU.RECMD.RETAIL.AMT", 
                            "SKU.LAST.CHG.DATE")]
  
  #-------------------#
  # Remove duplicates #
  #-------------------#
  Item_File <<- unique(Item_File)
  # View(length(Item_File$UPC.NUMBER))
}

#############################
# Step 2: Process Item File #
#############################
Step_2_Process_Item_File <- function() {
  #-----------------------------#
  # Calculate Margin and Markup #
  #-----------------------------#
  Item_File$SKU.MARKUP.AMT      <<- round( Item_File$SKU.SELL.RETAIL.AMT - Item_File$SKU.COST.AMT,   6)
  Item_File$SKU.MARKUP.PCT      <<- round( Item_File$SKU.MARKUP.AMT / Item_File$SKU.COST.AMT        ,3)
  Item_File$SKU.MARGIN.PCT      <<- round( Item_File$SKU.MARKUP.AMT / Item_File$SKU.SELL.RETAIL.AMT ,3)
  Item_File$SKU.RCMD.MARKUP.AMT <<- round( Item_File$SKU.RECMD.RETAIL.AMT - Item_File$SKU.COST.AMT  ,6)
  
  #------------------#
  # Get list of UPCs #
  #------------------#
  UPC <<- unique(Item_File[,c("POGENPAC.VNDR.NBR", 
                              "POGENPAC.VNDR.NAME",
                              "UPC.NUMBER",
                              "UPC.DESC",
                              "ITEM.DESC",
                              "PRODUCT.NUMBER",
                              "PRODUCT.DESC")])
  UPC$ID <<- seq.int(nrow(UPC))
 
  #--------------------#
  # Get list of stores #
  #--------------------#
  Stores           <-  as.data.frame(unique(Item_File$STORE.NBR))
  Stores           <-  as.data.frame(Stores[order(Stores),])
  names(Stores)[1] <-  "Store"
  Stores$ID        <-  seq.int(nrow(Stores))
  Stores           <<- Stores
  
  #---------------------------------------------#
  # Calculate overall medians across all stores #
  #---------------------------------------------#
  Overall_Median <<- ddply(Item_File,
                           .(UPC.NUMBER),
                           summarize,
                           "OVERALL.SKU.COST.AMT"         = median(SKU.COST.AMT),
                           "OVERALL.SKU.SELL.RETAIL.AMT"  = median(SKU.SELL.RETAIL.AMT),
                           "OVERALL.SKU.RECMD.RETAIL.AMT" = median(SKU.RECMD.RETAIL.AMT),
                           "OVERALL.SKU.MARKUP.AMT"       = median(SKU.MARKUP.AMT),
                           "OVERALL.SKU.MARKUP.PCT"       = median(SKU.MARKUP.PCT),
                           "OVERALL.SKU.MARGIN.PCT"       = median(SKU.MARGIN.PCT),
                           "OVERALL.SKU.RCMD.MARKUP.AMT"  = median(SKU.RCMD.MARKUP.AMT))
  
  #--------------------------------#  
  # Make list of Vendor + Products #
  #--------------------------------#
  UPC$Vendor_and_Product <<- paste0(trimws(UPC$POGENPAC.VNDR.NAME), " (", 
                                    trimws(UPC$POGENPAC.VNDR.NBR ), ") | UPC ", 
                                    trimws(UPC$UPC.NUMBER        ), " | Description ", 
                                    trimws(UPC$UPC.DESC          ), " / ",
                                    trimws(UPC$PRODUCT.DESC      ), " / ",
                                    trimws(UPC$ITEM.DESC         ))
 
  #--------#
  # Counts #
  #--------#
  N_SKUs    <<- length(unique(UPC$ID))
  N_Vendors <<- length(unique(UPC$POGENPAC.VNDR.NBR))
  N_Stores  <<- length(unique(Item_File$STORE.NBR))
  N_CCs     <<- length(unique(Item_File$POGENPAC.CC))
  
}

#####################################
# Step 3: Graph Price Across Stores #
#####################################
Step_3_Graph_Prices <- function(Product_Index = 1, Which_Field = "SKU.COST.AMT") { 
  #----------------------------------#
  # Extract out the selected Product #
  #----------------------------------#
  p <- Item_File[Item_File$UPC.NUMBER == UPC$UPC.NUMBER[Product_Index],
                 c("STORE.NBR",
                   "POGENPAC.VNDR.NBR", 
                   "POGENPAC.VNDR.NAME", 
                   Which_Field)]
  
  p$Temp <- unlist(p[,Which_Field])
  names(p)[length(names(p))] <- "Y"
  
  #-------------------------------------------#
  # Basic Statistics of the Selected Variable #
  #-------------------------------------------#
  p <- p %>%
    mutate(Mean               = mean(Y))                       %>%
    mutate(Mean               = round(Mean, 5))                %>%
    mutate(Median             = median(Y))                     %>%
    mutate(Multiple.of.Median = Y / median(Y))                 %>%
    mutate(Multiple.of.Median = round(Multiple.of.Median,5))   %>%
    
    mutate(Percentile         = round(rank(Y) / length(Y)),3)  %>%
    mutate(Z                  = (Y - mean(Y)) / sd(Y))         %>%
    mutate(P.Value            = pnorm(Z))                      %>%
    
    mutate(SD.1A = mean(Y) +   sd(Y)) %>%
    mutate(SD.1B = mean(Y) -   sd(Y)) %>%
    
    mutate(SD.2A = mean(Y) + 2*sd(Y)) %>%
    mutate(SD.2B = mean(Y) - 2*sd(Y)) %>%
    
    mutate(SD.3A = mean(Y) + 3*sd(Y)) %>%
    mutate(SD.3B = mean(Y) - 3*sd(Y))
  
  #--------------------------#
  # Determine outlier stores #
  #--------------------------#
  p$Outlier <- 0
 
  p$Outlier[p$Y >= p$SD.1A | p$Y <= p$SD.1B] <- 1
  p$Outlier[p$Y >= p$SD.2A | p$Y <= p$SD.2B] <- 2
  p$Outlier[p$Y >= p$SD.3A | p$Y <= p$SD.3B] <- 3
  
  p$SHOW.STORE.NBR <- p$STORE.NBR
  p$SHOW.STORE.NBR[p$Outlier == 0] <- ""
  
  Count_Outliers <- function(v = 0) { length( p$Outlier[p$Outlier == v] ) }     
  #View(length(p$Mean))
  #View(Count_Outliers(0))
  #View(Count_Outliers(1))
  #View(Count_Outliers(2))
  #View(Count_Outliers(3))
  #------------------#
  # Graph the prices #
  #------------------#
  p1 <- ggplot(data = p) +
    geom_point(aes(x=factor(STORE.NBR), y=Y, colour = factor(Outlier))) +
    scale_colour_manual(name = paste0("Median: ", round(p$Median[1],2)),
                        values = c("0" = "black", 
                                   "1" = "chartreuse3",
                                   "2" = "darkgoldenrod2",
                                   "3" = "red"),
                        labels = c("0" = paste0(Count_Outliers(0), " in [??1s]"),
                                   "1" = paste0(Count_Outliers(1), " in [??2s]"),
                                   "2" = paste0(Count_Outliers(2), " in [??3s]"),
                                   "3" = paste0(Count_Outliers(3), " out [??3s]"))) +
    
    geom_hline(aes(yintercept = Mean[1]),  linetype="dotted",     color = "red") +
    
    geom_hline(aes(yintercept = SD.1A[1]), linetype="dashed",     color = "red") +
    geom_hline(aes(yintercept = SD.1B[1]), linetype="dashed",     color = "red") +
    
    geom_hline(aes(yintercept = SD.2A[1]), linetype="longdash",     color = "red") +
    geom_hline(aes(yintercept = SD.2B[1]), linetype="longdash",     color = "red") +
    
    geom_hline(aes(yintercept = SD.3A[1]), linetype="F1", color = "red") +
    geom_hline(aes(yintercept = SD.3B[1]), linetype="F1", color = "red") +
    
    labs(title    = paste0(Which_Field, " Across Stores"),
         subtitle = UPC_Title(Product_Index),
         y = Which_Field,
         x = "Store Number") +
    theme(axis.text.x=element_text(angle = 90, vjust = .5),
          panel.grid.major.x = element_blank()) +
    scale_x_discrete(labels = as.factor(p$SHOW.STORE.NBR))
  
  #---------------------#
  # Persist key results #
  #---------------------#
  p$Y              <- NULL
  p$SHOW.STORE.NBR <- NULL
  p$"3"            <- NULL
  pSelected        <<- p
  
  return(p1)
}



########################################
# Step 4: Graph SKUs in Selected Store #
########################################
Step_4_Scatter_Plot_In_Selected_Store <- function(Store_Index = 1, Which_Field = "SKU.COST.AMT") { 
  
  #----------------------------#
  # Get all data for one store |
  #----------------------------#
  p <- Item_File[Item_File$STORE.NBR == Stores[Store_Index,1],
                 c("UPC.NUMBER",
                   Which_Field)]
  names(p)[length(names(p))] <- "Y"
  
  #---------------------------#
  # Merge selected store with #
  #---------------------------#
  p <- merge(p, Overall_Median)
  names(p)[grep(paste0("*", Which_Field), names(p))] <- "X"
  
  Store_vs_Median            <-  p[,c("UPC.NUMBER", "Y", "X")]
  Store_vs_Median$Difference <-  Store_vs_Median$Y - Store_vs_Median$X
  Store_vs_Median$Compare    <-  Comparison(Store_vs_Median$Difference)
  p$Compare                  <-  Store_vs_Median$Compare
  
  names(Store_vs_Median)     <-  c("UPC.NUMBER", Which_Field, "Median of All Stores", "Difference", "Compare")
  Store_vs_Median            <<- Store_vs_Median
  
  #--------------#
  # Scatter Plot #
  #--------------#
  ggplot(data = p, aes(x = X, y = Y, colour = factor(Compare))) +
    geom_point(size = 3) +
    geom_smooth(method = lm, linetype = "dashed", color = "darkgray") +
    labs(title    = paste0(Which_Field, " of all SKUs between Selected Store and Median Across Stores"),
         subtitle = paste0("Store: ", Stores[Store_Index,1]),
         y = "Selected Store",
         x = "Median of all SKUs Across all Stores",
         color = "Deviation") +
    geom_abline(intercept = 0, slope = 1)
}



#############################################
# Step 5: Graph Bar Chart of Delta in Store #
#############################################
Step_5_Bar_Chart_In_Selected_Store <- function(Store_Index = 1, Which_Field = "SKU.COST.AMT") { 
  
  #----------------------------#
  # Get all data for one store #
  #----------------------------#
  p <- Item_File[Item_File$STORE.NBR == Stores[Store_Index,1],
                 c("UPC.NUMBER",
                   Which_Field)]
  names(p)[length(names(p))] <- "Y"
  
  #---------------------------#
  # Merge selected store with #
  #---------------------------#
  p <- merge(p, Overall_Median)
  names(p)[grep(paste0("*", Which_Field), names(p))] <- "X"
  
  #------------------------------------------------------#
  # Determine gap between store metric and median metric #
  #------------------------------------------------------#
  p$Difference <- p$Y - p$X
  p$Compare    <- Comparison(p$Difference)
  p            <- p[order(p$Difference, decreasing = FALSE),]
  
  #-----------#
  # Bar Chart #
  #-----------#
  p$UPC.NUMBER <- factor(p$UPC.NUMBER, levels = p$UPC.NUMBER)
  ggplot(p, aes(x = UPC.NUMBER,
                y = Difference,
                label = Compare)) + 
    geom_bar(stat = 'identity', aes(fill = Compare), width = .5)  +
    labs(subtitle = paste0("Metric Difference Between Store # ", Stores[Store_Index,1], " and the Median Across Stores"), 
         title = "Diverging Bars",
         x = "UPC Number",
         y = "Price Difference Between Store and Media Across all Stores",
         color = "Deviation") + 
    coord_flip()
} 


################################################
# Step 6: Correlate Matrix of Stores by Metric #
################################################
Step_6_Correlate_Stores <- function(Which_Field = "SKU.COST.AMT") { 
  # Extract out the selected Product
  
  #------------------------------------#
  # Get Metric for all SKUs and Stores #
  #------------------------------------#
  p <- Item_File[, c("STORE.NBR", "UPC.NUMBER", Which_Field)]
  Stores_Comparison <<- tidyr::spread(p,
                                      key = UPC.NUMBER,
                                      value = Which_Field)
  #--------------------#
  # Correlation Matrix #
  #--------------------#
  Metric_Correlation <- suppressWarnings(cor(Stores_Comparison[,-1],
                                             use = "pairwise.complete.obs"))
  Metric_Correlation[is.na(Metric_Correlation)] <- 0
  Metric_Correlation <<- Metric_Correlation 
}



##############################################
# Step 7: Compare Two SKUs Across All Stores #
##############################################
Step_7_Compare_Two_SKUs <- function(UPC_1 = 1, UPC_2 = 2, Which_Field = "SKU.COST.AMT", Normalize=TRUE) {
  
  #--------------------------------------------#
  # Get selected UPCs to graph in scatter plot #
  #--------------------------------------------#
  p <- Stores_Comparison[,c(1, UPC_1+1, UPC_2+1)]
  names(p) <- c("STORE.NBR", "X1", "X2")
  
  #----------------------------------------------------------------------#
  # Normalize (make average = 1) given vastly different price magnitudes #
  #----------------------------------------------------------------------#
  if(Normalize == TRUE) {
    p$X1 <- round(p$X1 / mean(p$X1, na.rm = TRUE), 4)
    p$X2 <- round(p$X2 / mean(p$X2, na.rm = TRUE), 4) }
  
  #-----------------------------------------#
  # Drop all NAs (keep only complete cases) #
  #-----------------------------------------#
  p <- p[complete.cases(p),]
  
  #-------------------------------------------#
  # Persist the bivariate table for reporting #
  #-------------------------------------------#
  Bivariate            <-  p
  Bivariate$Difference <-  round(p$X1 - p$X2,4)
  names(Bivariate)     <-  c("STORE.NBR",
                             paste0(Flat_UPC_Title(UPC_1), " (Median: ", median(p$X1), ")"),
                             paste0(Flat_UPC_Title(UPC_2), " (Median: ", median(p$X2), ")"),
                             paste0("Difference (Median : ", median(Bivariate$Difference), ")"))
  Bivariate            <<- Bivariate
  
  #--------------#
  # Scatter Plot #
  #--------------#
  ggplot(data = p, aes(x = X1, y = X2)) +
    geom_point(size = 3) +
    geom_smooth(method = lm, linetype = "dashed", color = "darkgray") +
    labs(title    = paste0(Which_Field, " of Selected SKUs Across Stores"),
         y = Flat_UPC_Title(UPC_2),
         x = Flat_UPC_Title(UPC_1)) +
    geom_abline(intercept = 0, slope = 1) + 
    xlim(pmin(min(p$X1, na.rm=TRUE),min(p$X2, na.rm=TRUE)), pmax(max(p$X1, na.rm=TRUE),max(p$X2, na.rm=TRUE))) +
    ylim(pmin(min(p$X1, na.rm=TRUE),min(p$X2, na.rm=TRUE)), pmax(max(p$X1, na.rm=TRUE),max(p$X2, na.rm=TRUE)))
}



###########################################################
# Step 8: Run ML and create Suggestions / Prioritizations #
###########################################################
Step_8_Run_Models <- function() {
  temp <- Item_File
  
  #-----------------------------------#
  # Convert cross sections to Factors #
  #-----------------------------------#
  temp$STORE.NBR         <- as.factor(temp$STORE.NBR)
  temp$POGENPAC.VNDR.NBR <- as.factor(temp$POGENPAC.VNDR.NBR)
  temp$POGENPAC.CC       <- as.factor(temp$POGENPAC.CC)
  temp$WMT.ITEM.NUMBER   <- as.factor(temp$WMT.ITEM.NUMBER)
  temp$SYS.ITEM.NUMBER   <- as.factor(temp$SYS.ITEM.NUMBER)
  temp$UPC.NUMBER        <- as.factor(temp$UPC.NUMBER)
  temp$PRODUCT.NUMBER    <- as.factor(temp$PRODUCT.NUMBER)
  
  #------------------#
  # Merge in Medians #
  #------------------#
  temp <- merge(Overall_Median, temp,
                all.x = TRUE,
                all.y = TRUE)

  #----------------------------------------------------------------#
  # Remove quant variables that are correlated to the dep variable #
  #----------------------------------------------------------------#
  temp$SKU.RCMD.MARKUP.AMT   <- NULL
  temp$SKU.MARGIN.PCT        <- NULL
  temp$SKU.MARKUP.PCT        <- NULL
  temp$SKU.MARKUP.AMT        <- NULL
  temp$SKU.SELL.RETAIL.AMT   <- NULL
  temp$ITEM.BASE.UNIT.RETAIL <- NULL
  temp$SKU.RECMD.RETAIL.AMT  <- NULL
  
  temp$OVERALL.SKU.MARGIN.PCT       <- NULL
  temp$OVERALL.SKU.MARKUP.AMT       <- NULL
  temp$OVERALL.SKU.RCMD.MARKUP.AMT  <- NULL
  temp$OVERALL.SKU.RECMD.RETAIL.AMT <- NULL
  temp$OVERALL.SKU.SELL.RETAIL.AMT  <- NULL
  temp$OVERALL.SKU.MARKUP.PCT       <- NULL
  View(temp)
  #---------------#
  # Run the model #
  #---------------#
  model <- lm(data = temp, SKU.COST.AMT ~ .)
  
  temp2 <- temp
  temp2$MEDIAN.SKU.COST.AMT   <- temp$OVERALL.SKU.COST.AMT
  View(temp2)
  temp2$EXPECTED.SKU.COST.AMT <- ( predict.lm(model, data = temp2) + temp2$MEDIAN.SKU.COST.AMT ) / 2
  temp2$EXPECTED.SKU.COST.AMT <- round(temp2$EXPECTED.SKU.COST.AMT, 3)
  temp2$EXPECTED.DELTA        <- temp2$EXPECTED.SKU.COST.AMT - temp2$SKU.COST.AMT
  temp2$EXPECTED.DELTA        <- round(temp2$EXPECTED.DELTA, 3)
  
  
  #-------------------------------------------#
  # Remove irrelevent fields from the outputs #
  #-------------------------------------------#
  temp2$SKU.RCMD.MARKUP.AMT   <- NULL
  temp2$SKU.MARGIN.PCT        <- NULL
  temp2$SKU.MARKUP.PCT        <- NULL
  temp2$SKU.MARKUP.AMT        <- NULL
  temp2$SKU.SELL.RETAIL.AMT   <- NULL
  temp2$ITEM.BASE.UNIT.RETAIL <- NULL
  temp2$SKU.RECMD.RETAIL.AMT  <- NULL
  
  temp2$SKU.LAST.CHG.DATE            <- NULL
  temp2$OVERALL.SKU.COST.AMT         <- NULL
  temp2$OVERALL.SKU.MARGIN.PCT       <- NULL
  temp2$OVERALL.SKU.MARKUP.AMT       <- NULL
  temp2$OVERALL.SKU.RCMD.MARKUP.AMT  <- NULL
  temp2$OVERALL.SKU.RECMD.RETAIL.AMT <- NULL
  temp2$OVERALL.SKU.SELL.RETAIL.AMT  <- NULL
  temp2$OVERALL.SKU.MARKUP.PCT       <- NULL
  
  #---------------------#
  # Reorder the columns #
  #---------------------#
  temp2 <- temp2[,c(2:8  ,1, 9, 10:16)]
  Item_File_With_Model <<- temp2
}


###############################
# Functions for recurring use #
###############################
StatusBox         <- function(x, Title, Icon, Color) {return(
  valueBox(value    = tags$p(style = "font-size: 24px;", x),
           subtitle = tags$p(style = "font-size: 18px;", Title),
           icon     = icon(Icon, "fa-1x"),
           color    = Color
           #color      = "light-blue"
  )) }

Comma_Format      <- function(x)  {return(paste(prettyNum(round(x), 
                                                          big.mark = ",",
                                                          scientific = FALSE)))}

Comparison <- function(Delta) {
  Temp <- as.data.frame(Delta)
  Temp$Compare               <- "Same"
  Temp$Compare[Temp[,1] < 0] <- "Store Below Median"
  Temp$Compare[Temp[,1] > 0] <- "Store Above Median"
  
  return(Temp[,2]) }

UPC_Title <- function(Product_Index = 1) {
  paste0("Vendor: ",    UPC$POGENPAC.VNDR.NBR[Product_Index], " (", trimws(UPC$POGENPAC.VNDR.NAME[Product_Index]), ")\n",
         "UPC #: ",     UPC$UPC.NUMBER       [Product_Index], " (", trimws(UPC$UPC.DESC          [Product_Index]), ")\n",
         "Product #: ", UPC$PRODUCT.NUMBER   [Product_Index], " (", trimws(UPC$PRODUCT.DESC      [Product_Index]), ")")}


Flat_UPC_Title <- function(Product_Index = 1) {
  paste0("UPC #: ", UPC$UPC.NUMBER[Product_Index],         " (", trimws(UPC$UPC.DESC[Product_Index]), ")")}


Get_UPC_Label <- function(UPC_Numbers) {
  t <- as.data.frame(UPC_Numbers)
  colnames(t) <- "UPC.NUMBER"
  t<- merge(t, UPC)
  
  return(
    paste0(trimws(t$POGENPAC.VNDR.NAME),
           " | UPC #: ", 
           t$UPC.NUMBER, 
           " | ", 
           #trimws(t$PRODUCT.DESC)
           #, " / ", 
           trimws(t$ITEM.DESC)
           ))
}

Items_Per <- function(A, B) {
  x <- nrow(unique(Item_File[,c(A, B)]))
  y <- length(unique(Item_File[,B]))
  
  return(round(x/y, 2))
}

Most_Common_SKUs_Across <- function (A) {
  t<-unique(Item_File[,c("WMT.ITEM.NUMBER", A)])
  names(t) <- c("UPC", "X")
  v <- ddply(t, .(UPC), summarize, "COUNT" = length(UPC))
  return(v[v$COUNT == max(v$COUNT),])
}



# End of functions #



###########################################################################################################################
###########################################################################################################################
###########################################################################################################################


1
Step_0_Initialize()
Step_1_Import_Item_File()
Step_2_Process_Item_File()


#Step_3_Graph_Prices(1, Which_Field = "SKU.COST.AMT")
#Step_4_Scatter_Plot_In_Selected_Store(10,"SKU.COST.AMT")
#Step_5_Bar_Chart_In_Selected_Store(10,"SKU.COST.AMT")
#Step_6_Correlate_Stores(Which_Field = "SKU.COST.AMT")
#Step_7_Compare_Two_SKUs(3,4, Which_Field = "SKU.COST.AMT")






##########################################################
#                                                        #
#  Walmart Item File Error Identification Demonstration  #
#                                                        #
#  USER INTERFACE                                        #
#  --------------                                        #
#  Run the Companion Functions before launching UI       #
#                                                        #
#  Copyright by Genpact (C) 2019.                        #
#  For more information Contact David Hauser, CSO        #
#                                                        #
##########################################################


##################
# User Interface #
##################

SKU_Outlier_UI <- dashboardPage(
  
  dashboardHeader(title = "Item File Outlier Detection"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("View Item Files",          tabName = "View_Item_Files",         icon = icon("database",      "fa-1x")),
      menuItem("Outliers within SKUs",     tabName = "Outliers_Within_A_SKU",   icon = icon("tags",          "fa-1x")),
      menuItem("Outliers within Stores",   tabName = "Outliers_Within_A_Store", icon = icon("shopping-cart", "fa-1x")),
      menuItem("Bivariate Comparison",     tabName = "Correlation_Matrix",      icon = icon("balance-scale", "fa-1x")),
      menuItem("Models and Insights",      tabName = "Models_and_Insights",     icon = icon("info-circle",   "fa-1x"))
    )
  ),
  
  dashboardBody(
    #------------------------------#
    # Select size of Icons in menu #
    #------------------------------#
    #      tags$head(
    #        tags$style(HTML(".fa { font-size: 16px; }"))
    #      ),    
    fluidRow(
      #---------------------------#
      # Topline Counts and Status #
      #---------------------------#
      #infoBoxOutput("Number_of_Items",                     width = 1),
      #infoBoxOutput("Number_of_CCs",                       width = 1),
      #infoBoxOutput("Number_of_Vendors",                   width = 1),
      #infoBoxOutput("Number_of_SKUs",                      width = 1),
      #infoBoxOutput("Number_of_Stores",                    width = 1),
      #infoBoxOutput("Number_of_SKUs_Common_to_All_Stores", width = 1),
      #infoBoxOutput("Number_of_SKUs_Common_to_All_CCs",    width = 1),
      #infoBoxOutput("Number_of_SKUs_per_Store",            width = 1),
      #infoBoxOutput("Number_of_SKUs_per_Vendor",           width = 1),
      #infoBoxOutput("Number_of_SKUs_per_CC",               width = 1),
      #infoBoxOutput("Number_of_Metrics",                   width = 1),
      #infoBoxOutput("Overall_Markup" ,                     width = 1)),
      
      infoBoxOutput("Number_of_Items",                     width = 2),
      infoBoxOutput("Number_of_SKUs",                      width = 2),
      infoBoxOutput("Number_of_Stores",                    width = 2),
      infoBoxOutput("Number_of_SKUs_per_Store",            width = 2),
      infoBoxOutput("Number_of_Vendors",                   width = 2),
      infoBoxOutput("Number_of_SKUs_per_Vendor",           width = 2)),
    
    
    #---------------------#
    # Selections and Tabs #
    #---------------------#
    tabItems(
      ##################
      # View Item File #
      ##################
      tabItem(tabName = "View_Item_Files",
              h2("Master Item File"),
              fluidRow(
                box(solidHeader = TRUE, status = "primary",   width = 12,
                    DT::dataTableOutput("Full_Table_of_Item_File")))),
      
      
      #########################
      # Outliers Within A SKU #
      #########################
      tabItem(tabName = "Outliers_Within_A_SKU",
              h2("Stores that are Outliers by SKU"),
              fluidRow(
                column(width = 3,
                       box(solidHeader = TRUE, status = "primary",  width = NULL,
                           selectInput("Get_Metric",
                                       "Select Metric",
                                       choices = c("SKU Cost Amount"               = "SKU.COST.AMT",
                                                   "SKU Sell Retail Amount"        = "SKU.SELL.RETAIL.AMT",
                                                   "SKU Recommended Retail Amount" = "SKU.RECMD.RETAIL.AMT",
                                                   "SKU Markup Amount"             = "SKU.MARKUP.AMT",
                                                   "SKU Markup Percent"            = "SKU.MARKUP.PCT",
                                                   "SKU Margin Percent"            = "SKU.MARGIN.PCT",
                                                   "SKU Recommended Markup Amount" = "SKU.RCMD.MARKUP.AMT")))),
                column(width = 9,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           selectInput("Get_Product",
                                       "Select a Vendor and Product",
                                       choices = UPC$Vendor_and_Product[order(UPC$Vendor_and_Product)],
                                       multiple = FALSE)))),
              fluidRow(
                column(width = 12,
                       box(solidHeader = TRUE, status = "primary", width = NULL,
                           title = "Outliers for Selected Product and Metric (dots are stores)",
                           plotOutput("Graph_of_Selected_Metric_By_Store")))),
              
              
              fluidRow(
                column(width = 12,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           DT::dataTableOutput("Outlier_Table_Beyond_3"),
                           br(),
                           DT::dataTableOutput("Outlier_Table_Within_3"),
                           br(),
                           DT::dataTableOutput("Outlier_Table_Within_2")
                       )))
      ),
      
      
      ###########################
      # Outliers Within A Store #
      ###########################
      tabItem(tabName = "Outliers_Within_A_Store",
              h2("SKUs that are Outliers by Store"),
              fluidRow(
                column(width = 3,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           selectInput("Get_Metric_2",
                                       "Select Metric",
                                       choices = c("SKU Cost Amount"               = "SKU.COST.AMT",
                                                   "SKU Sell Retail Amount"        = "SKU.SELL.RETAIL.AMT",
                                                   "SKU Recommended Retail Amount" = "SKU.RECMD.RETAIL.AMT",
                                                   "SKU Markup Amount"             = "SKU.MARKUP.AMT",
                                                   "SKU Markup Percent"            = "SKU.MARKUP.PCT",
                                                   "SKU Margin Percent"            = "SKU.MARGIN.PCT",
                                                   "SKU Recommended Markup Amount" = "SKU.RCMD.MARKUP.AMT")))),
                column(width = 3,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           selectInput("Get_Store",
                                       "Select a Store",
                                       choices = Stores$Store,
                                       multiple = FALSE)))),
              fluidRow(
                column(width = 6,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           title = "Outliers of SKUs within a Store (dots are products)",
                           plotOutput("Graph_of_Store_VS_Median_Scatterplot"))),
                column(width = 6,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           title = "Outliers of SKUs within a Store (bars are products)",
                           plotOutput("Graph_of_Store_VS_Median_Divering_Bars")))),
              fluidRow(
                column(width = 12,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           DT::dataTableOutput("Store_Level_Outlier_Table"))))
      ),
      
      ################
      # Correlations #
      ################
      tabItem(tabName = "Correlation_Matrix",
              h2("Bivariate Outliers by Stores and SKUs"),
              fluidRow(
                column(width = 3,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           selectInput("Get_Metric_3",
                                       "Select Metric",
                                       choices = c("SKU Cost Amount"               = "SKU.COST.AMT",
                                                   "SKU Sell Retail Amount"        = "SKU.SELL.RETAIL.AMT",
                                                   "SKU Recommended Retail Amount" = "SKU.RECMD.RETAIL.AMT",
                                                   "SKU Markup Amount"             = "SKU.MARKUP.AMT",
                                                   "SKU Markup Percent"            = "SKU.MARKUP.PCT",
                                                   "SKU Margin Percent"            = "SKU.MARGIN.PCT",
                                                   "SKU Recommended Markup Amount" = "SKU.RCMD.MARKUP.AMT")),
                           selectInput("Normalize",
                                       "Normalize the Axes",
                                       choices = c("Leave Unnormalized"   = FALSE,
                                                   "Normalize Means to 1" = TRUE)))),
                column(width = 2,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           radioButtons("Correlation_Sort",
                                        "Correlation Sort",
                                        c("Alphabetical"                  = "alphabet",
                                          "Angular Order of Eigenvectors" = "AOE",
                                          "First Principal Component"     = "FPC",
                                          "Hierarchical Clustering"       = "hclust")))),
                column(width = 7,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           selectInput("Get_Product_X",
                                       "Select a Vendor and Product for X Axis",
                                       choices = UPC$Vendor_and_Product[order(UPC$Vendor_and_Product)],
                                       multiple = FALSE),
                           selectInput("Get_Product_Y",
                                       "Select a Vendor and Product for Y Axis",
                                       choices = UPC$Vendor_and_Product[order(UPC$Vendor_and_Product)],
                                       multiple = FALSE)))),
              fluidRow(
                column(width = 8,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           title = "Correlation Matrix (cells are pairs of products)",
                           plotOutput("Correlation_Matrix"))),
                column(width = 4,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           title = "Bivariate (dots are stores)",
                           plotOutput("Bivariate_Graph")))),
              fluidRow(
                column(width = 12,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           DT::dataTableOutput("Bivariate_Table"))))
      ),
      
      #######################
      # Models and Insights #
      #######################
      tabItem(tabName = "Models_and_Insights",
              h2("Insights and Recommendations"),
              fluidRow(
                column(width = 12,
                       box(solidHeader = TRUE, status = "primary", width = NULL,
                           title = "Actual vs Expected SKU Cost (dots are products X store)",
                           
                           column(width = 4,
                                  plotOutput("Scatterplot_of_Model_by_CC")),
                           column(width = 4,
                                  plotOutput("Scatterplot_of_Model_by_Vendor")),
                           column(width = 4,
                                  plotOutput("Scatterplot_of_Model_by_UPC"))))),
              
              #column(width = 4,
              #       box(solidHeader = TRUE, status = "primary",   width = NULL,
              #           title = "Actual vs Expected SKU Cost (dots are products X store)",
              #           plotOutput("Scatterplot_of_Model_by_CC"))),
              #column(width = 4,
              #       box(solidHeader = TRUE, status = "primary",   width = NULL,
              #           title = "Actual vs Expected SKU Cost (dots are products X store)",
              #           plotOutput("Scatterplot_of_Model_by_Vendor"))),
              #column(width = 4,
              #       box(solidHeader = TRUE, status = "primary",   width = NULL,
              #           title = "Actual vs Expected SKU Cost (dots are products X store)",
              #           plotOutput("Scatterplot_of_Model_by_UPC")))),
              
              
              
              fluidRow(
                column(width = 12,
                       box(solidHeader = TRUE, status = "primary",   width = NULL,
                           title = "Model Result Details",
                           DT::dataTableOutput("Model_Results_Table")))
              )
      )
    )
  )
)

Step_0_Initialize()



###############################
# Server and Report Generator #
###############################

SKU_Outlier_Server <- function(input, output) {
  
  #---------------#
  # Show Statuses #
  #---------------#
  output$Number_of_Items <- renderValueBox({
    StatusBox(x     = Comma_Format(nrow(Item_File)),
              Title = "Line Items",
              Icon  = "list-ol",
              Color = "blue") })
  
  output$Number_of_SKUs <- renderValueBox({
    StatusBox(x     = Comma_Format(N_SKUs),
              Title = "SKUs", 
              Icon  = "tags",
              Color = "green") })
  
  output$Number_of_Stores <- renderValueBox({
    StatusBox(x     = Comma_Format(N_Stores),
              Title = "Stores",
              Icon  = "city",
              Color = "red") })
  
  output$Number_of_SKUs_per_Store <- renderValueBox({
    StatusBox(x     = Items_Per("WMT.ITEM.NUMBER", "STORE.NBR"),
              Title = "SKUs per Store",
              Icon  = "shopping-cart",
              Color = "purple") })
  
  output$Number_of_Vendors <- renderValueBox({
    StatusBox(x     = Comma_Format(N_Vendors),
              Title = "Vendors",
              Icon  = "truck",
              Color = "yellow") })
  
  output$Number_of_SKUs_per_Vendor <- renderValueBox({
    StatusBox(x     = Items_Per("WMT.ITEM.NUMBER", "POGENPAC.VNDR.NBR"),
              Title = "SKUs per Vendor",
              Icon  = "truck-loading",
              Color = "aqua") })
  
  #  output$Number_of_CCs <- renderValueBox({
  #    StatusBox(x     = Comma_Format(N_CCs),
  #              Title = "Countries",
  #              Icon  = "globe-americas",
  #              Color = "red") })
  
  #  t <- Most_Common_SKUs_Across("STORE.NBR")
  #  output$Number_of_SKUs_Common_to_All_Stores <- renderValueBox({
  #    StatusBox(x     = Comma_Format( t[1,2]),
  #              Title = "Max Stores",
  #              Icon  = "globe-americas",
  #              Color = "navy") })
  
  #  output$Number_of_SKUs_Common_to_All_CCs <- renderValueBox({
  #    StatusBox(x     = Comma_Format( nrow(t)),
  #              Title = "Max SKUs",
  #              Icon  = "globe-americas",
  #              Color = "light-blue") })  
  
  #  output$Number_of_SKUs_per_CC <- renderValueBox({
  #    StatusBox(x     = Items_Per("WMT.ITEM.NUMBER", "POGENPAC.CC"),
  #              Title = "SKUs / CC",
  #              Icon  = "globe-americas",
  #              Color = "orange") })
  
  #  output$Number_of_Metrics <- renderValueBox({
  #    StatusBox(x     = Comma_Format(7),
  #              Title = "Metrics",
  #              Icon  = "calculator",
  #              Color = "maroon") })
  
  #  output$Overall_Markup <- renderValueBox({
  #    StatusBox(x     = round((sum(Item_File$SKU.SELL.RETAIL.AMT) - sum(Item_File$SKU.COST.AMT)) / sum(Item_File$SKU.COST.AMT),3),
  #              Title = "Overall Margin",
  #              Icon  = "calendar",
  #              Color = "purple") })  
  
  # 1 red
  # 2 yellow
  # 3 aqua
  # 4 blue
  # 5    light-blue
  # 6 green
  # 7 navy
  # 8 teal
  # 9 olive
  # 10 lime
  # 11 orange
  # 12 fuchsia
  # 13 purple
  # 14 maroon
  # 15 black
  
  #----------------#
  # View Item File #
  #----------------#
  output$Full_Table_of_Item_File <- renderDataTable({
    
    datatable(Item_File,
              class = 'cell-border stripe',
              rownames = TRUE,
              escape = FALSE,
              options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') )
  })      
  
  #-----------------------------#
  # 3) View the Outliers by SKU #
  #-----------------------------#
  output$Graph_of_Selected_Metric_By_Store <- renderPlot({
    
    # Get SKU from menu selection
    Product_Index <- UPC[UPC$Vendor_and_Product == input$Get_Product, "ID"]
    
    # Plot the Outlier Graph
    print( Step_3_Graph_Prices(Product_Index,
                               Which_Field = input$Get_Metric) )
    
    # Render the Outlier Tables
    output$Outlier_Table_Beyond_3 <- renderDataTable({
      p3 <- pSelected[pSelected$Outlier == 3,]
      if (nrow(p3) == 0) {p3 <- NULL}
      datatable(p3,
                class = 'cell-border stripe', rownames = TRUE, escape = FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; font: Arial',
                  htmltools::h4('Outliers Beyond [??3s]')),
                options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') ) })
    
    output$Outlier_Table_Within_3 <- renderDataTable({
      p2 <- pSelected[pSelected$Outlier == 2,]
      if (nrow(p2) == 0) {p2 <- NULL}
      datatable(p2,
                class = 'cell-border stripe', rownames = TRUE, escape = FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; font: Arial',
                  htmltools::h4('Outliers Within [??3s]')),
                options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') ) })      
    
    output$Outlier_Table_Within_2 <- renderDataTable({
      p1 <- pSelected[pSelected$Outlier == 1,]
      if (nrow(p1) == 0) {p1 <- NULL}
      datatable(p1,
                class = 'cell-border stripe', rownames = TRUE, escape = FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; font: Arial',
                  htmltools::h4('Outliers Within [??2s]')),
                options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') ) })      
    
  })  
  
  
  #--------------------------------------------#
  # 4) View the Scatter Plot Outliers by Store #
  #-----------------===------------------------#
  output$Graph_of_Store_VS_Median_Scatterplot <- renderPlot({
    
    # Get SKU from menu selection
    Store_Index <- Stores[Stores$Store == input$Get_Store, "ID"]
    
    # Plot the Outlier Graph
    print( Step_4_Scatter_Plot_In_Selected_Store(Store_Index,
                                                 Which_Field = input$Get_Metric_2) )
    
    # Render the Outlier Tables
    output$Store_Level_Outlier_Table <- renderDataTable({
      temp <- Store_vs_Median
      temp$Difference <- round(temp[,2] - temp[,3], 2)
      
      temp <- merge(UPC[,c("UPC.NUMBER",
                           "UPC.DESC",
                           "PRODUCT.NUMBER",
                           "PRODUCT.DESC",
                           "ITEM.DESC")],
                    temp)
      
      datatable(temp,
                class = 'cell-border stripe', rownames = TRUE, escape = FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; font: Arial',
                  htmltools::h4('Store versus Median of All Stores')),
                options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') ) })      
  })  
  
  
  #------------------------------------#
  # 5) View the Divering Bars by Store #
  #------------------------------------#
  output$Graph_of_Store_VS_Median_Divering_Bars <- renderPlot({
    
    # Get SKU from menu selection
    Store_Index <- Stores[Stores$Store == input$Get_Store, "ID"]
    
    # Plot the Outlier Graph
    print( Step_5_Bar_Chart_In_Selected_Store(Store_Index,
                                              Which_Field = input$Get_Metric_2) )
  })
  
  
  #-----------------------#
  # 6) Correlation Matrix #
  #-----------------------#
  output$Correlation_Matrix <- renderPlot({
    
    # Get the Correlation Matrix
    Step_6_Correlate_Stores(Which_Field = input$Get_Metric_3)
    
    
    # Full labels for the matrix
    rownames(Metric_Correlation) <- Get_UPC_Label(colnames(Metric_Correlation))
    colnames(Metric_Correlation) <- rownames(Metric_Correlation)
  

    # Reorder the Matrix
    ord <- corrMatOrder(Metric_Correlation,
                        order = input$Correlation_Sort)
    M2  <- Metric_Correlation[ord, ord]

    
    # Simplify column labels
    Full_colLabels <- colnames(M2)
    v <- substr(Full_colLabels, str_locate(Full_colLabels, "UPC #: ")[,2]+1, 1000000)
    Short_ColLabels <- paste0("UPC #: ", substr(v, 1, str_locate(v, " ")[,1]-1))
    colnames(M2) <- Short_ColLabels
    
    # Plot the Matrix
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

    corrplot(M2, 
             method = "shade", shade.col = NA, 
             tl.col = "black", tl.srt = 45, tl.cex=.7,
             col=col(20), addCoef.col = "black",
             is.corr=FALSE,
             diag = FALSE)
  })
  
  
  #--------------------#
  # 7) Bivariate Graph #
  #--------------------#
  output$Bivariate_Graph <- renderPlot({
    
    # Get SKU from menu selection
    Product_Index_1 <- UPC[UPC$Vendor_and_Product == input$Get_Product_X, "ID"]
    Product_Index_2 <- UPC[UPC$Vendor_and_Product == input$Get_Product_Y, "ID"]
    
    # Get the graph
    p <- Step_7_Compare_Two_SKUs(Product_Index_1,
                                 Product_Index_2,
                                 input$Get_Metric_3,
                                 input$Normalize)
    
    # Show bivariate table
    output$Bivariate_Table <- renderDataTable({
      datatable(Bivariate,
                class = 'cell-border stripe', rownames = TRUE, escape = FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; font: Arial',
                  htmltools::h4('Bivariate Comparison of SKUs Across Stores')),
                options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') )
    })      
    
    # Plot the graph
    suppressWarnings(print(p))
  })
  
  
  #------------------#
  # 8) Model Results #
  #------------------#
  output$Scatterplot_of_Model_by_Vendor <- renderPlot({
    # Run the models and get results
    Step_8_Run_Models()
    
    #--------------------------#
    # Show Model Results table #
    #--------------------------#
    output$Model_Results_Table <- renderDataTable({
      datatable(Item_File_With_Model,
                class = 'cell-border stripe', rownames = TRUE, escape = FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; font: Arial',
                  htmltools::h4('Bivariate Comparison of SKUs Across Stores')),
                options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') )
    })
    
    #------------------------#
    # Scatter Plot by Vendor #
    #------------------------#
    t <- Item_File_With_Model[,c("SKU.COST.AMT", "EXPECTED.SKU.COST.AMT", "POGENPAC.VNDR.NAME")]
    t <- unique(t)
    print(ggplot(data = t, 
                 aes(x = SKU.COST.AMT, y = EXPECTED.SKU.COST.AMT, colour = factor(POGENPAC.VNDR.NAME))) +
            geom_point(size = 3) +
            geom_smooth(method = lm, linetype = "dashed", color = "darkgray") +
            labs(#title    = "Actual versus Expected SKU Cost Amount",
              y = "Expected SKU Cost Amount",
              x = "Actual SKU Cost Amount",
              color = "UPC") +
            theme(legend.position = c(.8,.5),
                  legend.text = element_text(size=8)) +
            geom_abline(intercept = 0, slope = 1) )
  })
  
  
  output$Scatterplot_of_Model_by_CC <- renderPlot({
    #--------------------#
    # Scatter Plot by CC #
    #--------------------#
    t <- Item_File_With_Model[,c("SKU.COST.AMT", "EXPECTED.SKU.COST.AMT", "POGENPAC.CC")]
    t <- unique(t)
    ggplot(data = t, 
           aes(x = SKU.COST.AMT, y = EXPECTED.SKU.COST.AMT, colour = factor(POGENPAC.CC))) +
      geom_point(size = 3) +
      geom_smooth(method = lm, linetype = "dashed", color = "darkgray") +
      labs(#title    = "Actual versus Expected SKU Cost Amount",
        y = "Expected SKU Cost Amount",
        x = "Actual SKU Cost Amount",
        color = "UPC") +
      theme(legend.position = c(.925,.5),
            legend.text = element_text(size=8)) +
      geom_abline(intercept = 0, slope = 1) })
  
  
  
  output$Scatterplot_of_Model_by_UPC <- renderPlot({
    #---------------------#
    # Scatter Plot by UPC #
    #---------------------#
    t <- Item_File_With_Model[,c("SKU.COST.AMT", "EXPECTED.SKU.COST.AMT", "UPC.NUMBER")]
    t <- unique(t)
    ggplot(data = t, 
           aes(x = SKU.COST.AMT, y = EXPECTED.SKU.COST.AMT, colour = factor(UPC.NUMBER))) +
      geom_point(size = 3) +
      geom_smooth(method = lm, linetype = "dashed", color = "darkgray") +
      labs(#title    = "Actual versus Expected SKU Cost Amount",
        y = "Expected SKU Cost Amount",
        x = "Actual SKU Cost Amount",
        color = "UPC") +
      theme(legend.position = c(.9,.55),
            legend.text = element_text(size=8)) +
      geom_abline(intercept = 0, slope = 1) })
}


Start_App <- function() {
  shinyApp(ui = SKU_Outlier_UI,
           server = SKU_Outlier_Server)  
}


###############
# Run the App #
###############
Start_App()
