#######################################################################################
# working directory, options, libraries

setwd("/Volumes/Lab_Gerke/ShinyApps/LD_HIC")

options(stringsAsFactors=FALSE)

library(biomaRt)

#######################################################################################
# load ensembl54 gene location information, write to a .txt

ensembl54 <- useMart("ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl")
annot <- getBM(attributes=c("hgnc_symbol","start_position","end_position"),
               mart=ensembl54)
# many of these rows have no gene sybmol, we might want to consider subsetting
write.table(annot, file="Data/geneAnnot.txt", sep="\t", row.names=FALSE, quote=FALSE)

#######################################################################################
# load SNP location information, write to a .txt

snpmart <- useMart("ENSEMBL_MART_SNP", dataset="hsapiens_snp")
annot <- getBM(attributes = c("refsnp_id","chr_name","chrom_start"), mart=snpmart)
write.table(annot, file="Data/snpAnnot.txt", sep="\t", row.names=FALSE, quote=FALSE)
