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
library(writexl)
library(tidyverse)
library(plotly)

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

# Example links
EXAMPLES <- list(
  "8q24" = list(
    url = '?_inputs_&oncoParameters1=null&oncoParameters2=null&oncoParameters3=null&oncoParameters4=null&parameters=["pos_hg38"%2C"r2"%2C"query_snp_rsid"]&parameters2=["%23chromosome"%2C"coordinate"%2C"hits"%2C"score_anno"]&plotColor="1"&plotEndBP=127810818&plotStartBP=126730818&pop="EUR"&resetBP=0&sidebarCollapsed=false&sidebarItemExpanded=null&snpList="rs1016343%2Crs10505477%2Crs9642880%2Crs13281615%2Crs1447295%2Crs1562430%2Crs16901979%2Crs16902094%2Crs2456449%2Crs4242382%2Crs4242384%2Crs445114%2Crs6983267%2Crs7014346"&tabs="tab1"&update1=7&updateBP=0&value=0.8',
    text = "Chromosome 8q24 cancer risk loci",
    tooltip = "A selection of SNPs that could be uploaded via a text file",
    input_id = NULL
  ),
  "ancestry" = list(
    url = '?_inputs_&oncoParameters1=null&oncoParameters2=null&oncoParameters3=null&oncoParameters4=null&parameters=%5B%22pos_hg38%22%2C%22r2%22%2C%22query_snp_rsid%22%5D&parameters2=%5B%22%23chromosome%22%2C%22coordinate%22%2C%22hits%22%2C%22score_anno%22%5D&plotColor=%221%22&plotEndBP=100020977&plotStartBP=99020977&pop=%22EUR%22&resetBP=0&sidebarCollapsed=false&sidebarItemExpanded=null&snpList=%22rs1229984%2C%20rs3811801%22&tabs=%22tab1%22&update1=8&updateBP=0&value=0.8',
    text = "Ancestry informative markers",
    tooltip = "rs1229984, rs3811801",
    input_id = NULL
  ),
  "protective" = list(
    url = '?_inputs_&oncoParameters1=null&oncoParameters2=null&oncoParameters3=null&oncoParameters4=null&parameters=%5B%22pos_hg38%22%2C%22r2%22%2C%22query_snp_rsid%22%5D&parameters2=%5B%22%23chromosome%22%2C%22coordinate%22%2C%22hits%22%2C%22score_anno%22%5D&plotColor=%221%22&plotEndBP=118518477&plotStartBP=117838477&pop=%22EUR%22&resetBP=0&sidebarCollapsed=false&sidebarItemExpanded=null&snpList=%22rs3754127%2C%20rs3765501%2C%20rs4658973%22&tabs=%22tab1%22&update1=47&updateBP=0&value=0.9',
    text = "Potentially protective of DNA damage",
    tooltip = "rs3754127, rs3765501, rs4658973",
    input_id = NULL
  )
)
