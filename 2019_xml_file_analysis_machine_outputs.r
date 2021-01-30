#rm(list = ls())

library(XML)

setwd("C:/Users/boylanb/Brandon Boylan/____")

XMLFileNames <- read.csv(file = "C:/Users/boylanb/Brandon Boylan/____.csv")

#Create a data list so you can later append all the metadata files for Tableau
datalist = list()


#Loop to parse all the XML files and store them in a list (so they can all be appended after the loop finishes)
for (ii in 1:dim(XMLFileNames)[1]){
  MyXMLFile <- XMLFileNames[ii,]
  MyXMLFile <- paste("C:/Users/boylanb/Brandon Boylan/____", MyXMLFile, sep = "")
  myxml <- xmlParse(file = MyXMLFile)
  
  #Annotation Description
  AnnotationDesc <- sapply("AnnotationDesc", function(x) xpathSApply(myxml, '//Annotation', xmlGetAttr, x))  
  AnnotationDesc <- as.data.frame(AnnotationDesc[1:(length(AnnotationDesc) - 1)])
  AnnotationDesc <- cbind(row(AnnotationDesc), AnnotationDesc)
  colnames(AnnotationDesc) <- c("AnnotationID", "AnnotationDesc")
  
  #Annotation Title  
  AnnotationTitle <- sapply("AnnotationTitle", function(x) xpathSApply(myxml, '//Annotation', xmlGetAttr, x))  
  AnnotationTitle <- as.data.frame(AnnotationTitle[1:(length(AnnotationTitle) - 1)])
  AnnotationTitle <- cbind(row(AnnotationTitle), AnnotationTitle)
  colnames(AnnotationTitle) <- c("AnnotationID", "AnnotationTitle")
  
  #Coordinates of bounding boxes for the Annotations
  boundingbox <- xmlToDataFrame(node = getNodeSet(myxml, "//Image/ROI/Annotation"))
  boundingbox <- data.frame(boundingbox)
  MyArea1 <- data.frame(do.call('rbind', strsplit(as.character(boundingbox[1:(dim(boundingbox)[1] - 1), 1]), ',', fixed=TRUE)))
  MyArea2 <- data.frame(do.call('rbind', strsplit(as.character(boundingbox[1:(dim(boundingbox)[1] - 1), 2]), ',', fixed=TRUE)))
  MyAreaCombined <- cbind(MyArea1, MyArea2)
  colnames(MyAreaCombined) <- c("X1","Y1","X2","Y2")
  
  MyAreaCombined$X1 <- as.numeric(as.character(MyAreaCombined$X1))
  MyAreaCombined$X2 <- as.numeric(as.character(MyAreaCombined$X2))
  MyAreaCombined$Y1 <- as.numeric(as.character(MyAreaCombined$Y1))
  MyAreaCombined$Y2 <- as.numeric(as.character(MyAreaCombined$Y2))
  
  MyAreaCombined$BoundingBoxArea <- (MyAreaCombined$X2 - MyAreaCombined$X1) * (MyAreaCombined$Y1 - MyAreaCombined$Y2)
  MyAreaCombined <- cbind(c(1:length(MyAreaCombined$BoundingBoxArea)), MyAreaCombined)
  colnames(MyAreaCombined)[1] <- "AnnotationID"
  
  #Barcode Label
  BarcodeLabel <- as.vector(sapply("Name", function(x) xpathSApply(myxml, '//Image', xmlGetAttr, x)))
  BarcodeLabel <- gsub(".ndpi", "", BarcodeLabel)
  BarcodeLabel <- as.data.frame(rep(BarcodeLabel, length(MyAreaCombined$BoundingBoxArea)))
  colnames(BarcodeLabel) <- "BarcodeLabel"
  
  #Overall label/flag for the slide
  #Pathologist Classification
  PathologistClassification <- sapply("PathologistClassification", function(x) xpathSApply(myxml, '//Details', xmlGetAttr, x))
  PathologistClassification <- as.data.frame(rep(PathologistClassification, length(MyAreaCombined$BoundingBoxArea)))
  colnames(PathologistClassification) <- "PathologistClassification"
  
  #Aira Matrix's Classification (Iadss Classification)
  IadssClassification <- sapply("IadssClassification", function(x) xpathSApply(myxml, '//Details', xmlGetAttr, x))  
  IadssClassification <- as.data.frame(rep(IadssClassification, length(MyAreaCombined$BoundingBoxArea)))
  colnames(IadssClassification) <- "IadssClassification"
  
  #Combine and merge datasets
  MyDataTableau1 <- cbind(BarcodeLabel, PathologistClassification, IadssClassification)
  
  MyDataTableau2 <- merge(x = 
                            merge(x = AnnotationDesc, y = AnnotationTitle, by = "AnnotationID", all = T),
                          y = 
                            MyAreaCombined[,c(1,6)],
                          by = "AnnotationID",
                          all = T)
  
  ifelse(dim(MyDataTableau1)[1] == dim(MyDataTableau1)[1], MyDataTableauExport <- cbind(MyDataTableau1, MyDataTableau2), "error")
  
  #Annotation ID for Tableau scatter plot
  MyDataTableauExport$AnnotationID2 <- paste(MyDataTableauExport$BarcodeLabel, "_", MyDataTableauExport$AnnotationID,  sep = "")  
  MyDataTableauExport$AnnotationID1 <- MyDataTableauExport$AnnotationID 
  
  #Add the datset to the list
  datalist[[ii]] <- MyDataTableauExport
  
  #In case we want to QA the individual datasets
  assign(paste("MyDataTableauLoop", ii, sep = ""), MyDataTableauExport) #MyDataTableauLoop2
  
  #print(myxml)
  #print(MyXMLFile)
}


#Append the datasets (ie the annotations for all the slides)
MyDataTableauExportLoop = do.call(rbind, datalist)

head(MyDataTableauExportLoop)



#####################################################################################################################
##################################################################################################################### 



#Additional metadata from Fangyao
TreatmentControlLabels <- read.csv(file = "C:/Users/boylanb/Brandon Boylan/Digital Pathology/Aira Matrix/Aira Matrix vendor demo treatment_control labels.csv", header = T)

TreatmentControlLabels <- TreatmentControlLabels[c(2, 3, 7:10, 12:14, 16:20, 22, 25)]
names(TreatmentControlLabels) <- c("BarcodeLabel",
                                   "SlideType",
                                   "Control",
                                   "GroupNumber",
                                   "AnimalNumber",
                                   "Species",
                                   "Treatment",
                                   "Dosage",
                                   "Route",
                                   "Tissue",
                                   "Request",
                                   "RequestTitle",
                                   "Pathologist",
                                   "PrimaryInvestigator",
                                   "SlideDomain",
                                   "ScanDate")

MyDataTableauExportFinal <- merge(x = MyDataTableauExportLoop, y = TreatmentControlLabels, by = "BarcodeLabel", all.x = T)

#View(MyDataTableauExportFinal)

setwd("C:/Users/boylanb/Brandon Boylan/Digital Pathology/Aira Matrix/R output for Tableau")
#MyFileName <- paste(as.character(unique(MyDataTableauExportFinal$BarcodeLabel)), ".csv", sep = "")
MyFileName <- "AllSlides.csv"

write.csv(MyDataTableauExportFinal, file = MyFileName)


