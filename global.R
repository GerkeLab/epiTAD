library(shinydashboard)
library(shiny)
library(haploR)
library(data.table)
library(biomaRt)
library(shinycssloaders)
library(jsonlite)
library(Sushi)
library(HiTC)
library(colorspace)

# Global Data
ensembl54 <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
load("data/hicData.Rdata")

tad <- fread("data/IMR90_domains_hg19.bed")
colnames(tad) <- c("chr", "start_position", "end_position")
tad$chr <- gsub("chr", "", tad$chr)
tad$chr <- as.numeric(tad$chr)
tad <- tad[!is.na(tad$chr), ]

lad <- fread("data/human.fibroblast.DamID.hg19.bed")
colnames(lad) <- c("chr", "start", "end", "dunno")
lad$chr <- gsub("chr", "", lad$chr)
lad$chr <- as.numeric(lad$chr)
lad <- lad[!is.na(lad$chr), ]

enableBookmarking("url")
