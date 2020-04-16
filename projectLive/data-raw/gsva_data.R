## code to prepare `DATASET` dataset goes here
#
library(synapser)
library(AnnotationDbi)
synapser::synLogin()

#### Only NTAP published files

NTAP_published_files <- synapser::synTableQuery(glue::glue("SELECT * FROM syn21221980"))$asDataFrame()

data1 <- synapser::synTableQuery(glue::glue("SELECT * FROM {NTAP_published_files$tableId[1]}"))$asDataFrame() %>% 
  dplyr::select(totalCounts, Symbol, zScore, specimenID, individualID, sex, tumorType, studyName)

data2 <- synapser::synTableQuery(glue::glue("SELECT * FROM {NTAP_published_files$tableId[2]}"))$asDataFrame() %>% 
  dplyr::select(totalCounts, Symbol, zScore, specimenID, individualID, sex, tumorType, studyName)

data3 <- synapser::synTableQuery(glue::glue("SELECT * FROM {NTAP_published_files$tableId[3]}"))$asDataFrame() %>% 
  dplyr::select(totalCounts, Symbol, zScore, specimenID, individualID, sex, tumorType, studyName)

data4 <- synapser::synTableQuery(glue::glue("SELECT * FROM {NTAP_published_files$tableId[4]}"))$asDataFrame() %>% 
  dplyr::select(totalCounts, Symbol, zScore, specimenID, individualID, sex, tumorType, studyName)


NTAP_pub_rnaseq_data <- rbind(data1,data2,data3,data4)

NTAP_pub_rnaseq_data$sex[NTAP_pub_rnaseq_data$sex == "female"] <- "Female"
NTAP_pub_rnaseq_data$sex[NTAP_pub_rnaseq_data$sex == "male"] <- "Male"
NTAP_pub_rnaseq_data$tumorType[NTAP_pub_rnaseq_data$tumorType == "Malignant peripheral nerve sheath tumor"] <- "Malignant Peripheral Nerve Sheath Tumor"

NTAP_pub_rnaseq_data <- NTAP_pub_rnaseq_data %>% 
  filter(tumorType %in% c("Cutaneous Neurofibroma", "Plexiform Neurofibroma", "Neurofibroma", "Malignant Peripheral Nerve Sheath Tumor"),
         !grepl('xenograft', specimenID, ignore.case = T),
         !specimenID %in% c("BI386-004","CW225-001","DW356-002",
                            "JK368-003", "SK436-005"))

#### Matrix for PCA and GSVA

gene_mat<-reshape2::acast(NTAP_pub_rnaseq_data,
                          Symbol~specimenID,
                          value.var='zScore',
                          fun.aggregate = mean)

missing<-which(apply(gene_mat,1,function(x) any(is.na(x))))
gene_mat<-gene_mat[-missing,]

### libraries for GSVA

data("c2BroadSets")

#get mapping from enst to hgnc
library(org.Hs.eg.db)
library(hgu95a.db) #human

gene_map<-AnnotationDbi::select(org.Hs.eg.db,  #using human db since its all primary tumor data
                                columns=c("ENSEMBL", "SYMBOL", "ENTREZID"),
                                keys=keys(org.Hs.eg.db, keytype = "SYMBOL"),
                                keytype="SYMBOL",
                                multiVals=unique(rownames(gene_mat)))

entrez<-gene_map$ENTREZID[match(rownames(gene_mat),gene_map$SYMBOL)]
gsva_mat<-gene_mat[which(!is.na(entrez)),]
rownames(gsva_mat)<-entrez[!is.na(entrez)]

#load("data-raw/gsva_data.RData")
usethis::use_data(gsva_mat, overwrite = T)
usethis::use_data(gene_mat, overwrite = T)
