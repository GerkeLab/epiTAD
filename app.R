library(shinydashboard)
library(shiny)
library(haploR)
library(data.table)
library(biomaRt)
library(shinycssloaders)
#library(plotly)
library(jsonlite)
library(Sushi)
library(HiCDataHumanIMR90)
library(HiTC)
library(colorspace)


###################################################################################################
ui <- dashboardPage(dashboardHeader(title="epiTAD"),
                    dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem("SNPs",tabName = "tab1"),
                                                 menuItem("Info",tabName = "tab2"))),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "tab1",
                                fluidRow(
                                  box(title = "Query SNPs",
                                      textInput("snpList","Enter SNP rsIDs (comma separated)",value=""),
                                      h5(helpText("Upload SNP List (one SNP per line)")),
                                      fileInput("file1","Choose a file",
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv')),
                                      tags$hr(),
                                      actionButton("update1", "Perform query")),
                                  tabBox(title="Select Output",
                                         tabPanel("Source",
                                                  selectInput("pop","Population",c("EUR","AFR","AMR","ASN"), selected="EUR"),
                                                  sliderInput("value","LD threshold",min=0,max=1,value=0.8)),
                                         tabPanel("HaploReg",
                                                  checkboxGroupInput("parameters","HaploR",c("Chromosome"="chr","Position"="pos_hg38","r2"="r2","Query SNP"="query_snp_rsid",                        
                                                                                             "D'"="D'","Query SNP"="is_query_snp",                       
                                                                                             "Reference allele"="ref","Alternative allele"="alt","LD(AFR)"="AFR",                        
                                                                                             "LD(AMR)"="AMR","LD(ASN)"="ASN","LD(EUR)"="EUR",                        
                                                                                             "GERP scores"="GERP_cons","SiPhy scores"="SiPhy_cons","Chromatin States" ="Chromatin_States",           
                                                                                             "Imputed Chromatin States"="Chromatin_States_Imputed","Chromatin Marks"="Chromatin_Marks","DNAse"="DNAse",                      
                                                                                             "Proteins","eQTL","GWAS study name"="gwas",                       
                                                                                             "GRASP study name"="grasp","Motifs","GENCODE transcript ID"="GENCODE_id",                 
                                                                                             "GENCODE gene name"="GENCODE_name","GENCODE direction"="GENCODE_direction","GENCODE distance"="GENCODE_distance",           
                                                                                             "NCBI Reference Sequence Accession number"="RefSeq_id","NCBI Reference Sequence name"="RefSeq_name","NCBI Reference Sequence direction"="RefSeq_direction",           
                                                                                             "NCBI Reference Sequence distance"="RefSeq_distance","Annotated proteins"="dbSNP_functional_annotation"),inline = TRUE, selected = c("query_snp_rsid","pos_hg38","r2"))),
                                         tabPanel("RegulomeDB",
                                                  checkboxGroupInput("parameters2","Regulome",c("Chromosome"="#chromosome",
                                                                                                "Coordinates"="coordinate",
                                                                                                "Hits"="hits", "Score"="score_anno"), selected = c("#chromosome","coordinate","hits","score_anno"))),
                                         tabPanel("eQTL",
                                                  h5(helpText("If no eQTLs, this section will be empty")),
                                                  uiOutput("eTissues")),
                                         tabPanel("Oncotator",
                                                  checkboxGroupInput("oncoParameters1","General",c("ACHILLES_Lineage_Results_Top_Genes","CCLE_By_Gene_total_mutations_in_gene",  
                                                                                                   "COSMIC_FusionGenes_fusion_genes",           
                                                                                                   "COSMIC_Tissue_tissue_types_affected","COSMIC_Tissue_total_alterations_in_gene",   
                                                                                                   "Familial_Cancer_Genes_Reference","Familial_Cancer_Genes_Syndrome",            
                                                                                                   "Familial_Cancer_Genes_Synonym","HGNC_Accession.Numbers",                   
                                                                                                   "HumanDNARepairGenes_Role","MutSig.Published.Results_Published_Results",
                                                                                                   "TCGAScape_Amplification_Peaks","TCGAScape_Deletion_Peaks",                  
                                                                                                   "TUMORScape_Amplification_Peaks","TUMORScape_Deletion_Peaks",                 
                                                                                                   "alt_allele",                                
                                                                                                   "build","chr",                                      
                                                                                                   "class","end",                                       
                                                                                                   "gene","protein_change",                            
                                                                                                   "ref_allele","start",                                     
                                                                                                   "strand","transcripts" ), inline=TRUE),
                                                  checkboxGroupInput("oncoParameters2","Cancer Gene Census",c("CGC_Cancer.Germline.Mut","CGC_Cancer.Molecular.Genetics",             
                                                                                                              "CGC_Cancer.Somatic.Mut","CGC_Cancer.Syndrome",                       
                                                                                                              "CGC_Chr","CGC_Chr.Band",                              
                                                                                                              "CGC_GeneID","CGC_Mutation.Type",                         
                                                                                                              "CGC_Name","CGC_Other.Germline.Mut",                    
                                                                                                              "CGC_Other.Syndrome.Disease","CGC_Tissue.Type",                           
                                                                                                              "CGC_Translocation.Partner","CGC_Tumour.Types...Somatic.Mutations.",     
                                                                                                              "CGC_Tumour.Types..Germline.Mutations."),inline = TRUE),
                                                  checkboxGroupInput("oncoParameters3","HUGO Gene Nomenclature Committee",c("HGNC_Approved.Name","HGNC_CCDS.IDs",                             
                                                                                                                            "HGNC_Chromosome","HGNC_Date.Modified",                        
                                                                                                                            "HGNC_Date.Name.Changed","HGNC_Date.Symbol.Changed",                  
                                                                                                                            "HGNC_Ensembl.Gene.ID","HGNC_Ensembl.ID.supplied.by.Ensembl.",      
                                                                                                                            "HGNC_Entrez.Gene.ID","HGNC_Entrez.Gene.ID.supplied.by.NCBI.",     
                                                                                                                            "HGNC_Enzyme.IDs","HGNC_Gene.family.description",              
                                                                                                                            "HGNC_HGNC.ID","HGNC_Locus.Group",                          
                                                                                                                            "HGNC_Locus.Type","HGNC_Name.Synonyms",                        
                                                                                                                            "HGNC_OMIM.ID.supplied.by.NCBI.","HGNC_Previous.Names",                      
                                                                                                                            "HGNC_Previous.Symbols","HGNC_Primary.IDs",                          
                                                                                                                            "HGNC_Pubmed.IDs","HGNC_Record.Type",                          
                                                                                                                            "HGNC_RefSeq.IDs","HGNC_RefSeq.supplied.by.NCBI.",            
                                                                                                                            "HGNC_Secondary.IDs","HGNC_Status",                               
                                                                                                                            "HGNC_Synonyms","HGNC_UCSC.ID.supplied.by.UCSC." ,           
                                                                                                                            "HGNC_UniProt.ID.supplied.by.UniProt.","HGNC_VEGA.IDs"), inline=TRUE),
                                                  checkboxGroupInput("oncoParameters4","UniProt",c("UniProt_AA_experimental_info","UniProt_AA_natural_variation",              
                                                                                                   "UniProt_AA_region","UniProt_AA_site",                           
                                                                                                   "UniProt_DrugBank","UniProt_GO_Biological_Process",             
                                                                                                   "UniProt_GO_Cellular_Component","UniProt_GO_Molecular_Function",            
                                                                                                   "UniProt_alt_uniprot_accessions","UniProt_uniprot_accession",                 
                                                                                                   "UniProt_uniprot_entry_name"), inline=TRUE))
                                  )
                                ),
                                fluidRow(
                                  tabBox(title = "Variant Annotation",
                                         tabPanel("HaploReg",
                                                  withSpinner(tableOutput("LDtable1"), color="#00ffff", type = 6)),
                                         tabPanel("RegulomeDB",
                                                  withSpinner(tableOutput("LDtable2"), color="#00ffff", type = 6)),
                                         tabPanel("TADs",
                                                  withSpinner(textOutput("tadBoundaries"), color="#00ffff", type = 6),
                                                  uiOutput("hic1"))
                                  ),
                                  tabBox(title="Gene Annotation",
                                         tabPanel("ENSEMBL",
                                                  h5(helpText("Genes spanned by the greater of the LD or TAD region")),
                                                  withSpinner(tableOutput("geneTable"), color="#00ffff", type = 6),
                                                  downloadButton("geneDownload","Download")),
                                         tabPanel("Oncotator",
                                                  withSpinner(tableOutput("oncoTable"), color="#00ffff", type = 6)),
                                         tabPanel("eQTL",
                                                  tableOutput("eTable1"),
                                                  uiOutput("eqtl1"))),
                                  column(12,align="center",offset=3,
                                         tabBox(title="Other",
                                                tabPanel("Links",
                                                         uiOutput("clinical1"),
                                                         uiOutput("ucsc1")),
                                                # tabPanel("Interactive",
                                                #          withSpinner(plotlyOutput("plot1",height="450px"),color = "#00ffff", type = 6)),
                                                # tabPanel("HIC",
                                                #          withSpinner(plotOutput("hicPlot",height="450px"),color = "#00ffff", type = 6)),
                                                # tabPanel("HIC with TADs",
                                                #          withSpinner(plotOutput("hicPlot2",height="450px"),color = "#00ffff", type = 6)),
                                                tabPanel("Figure",
                                                         # uiOutput("plotStart"),
                                                         # uiOutput("plotEnd"),
                                                         # actionButton("updateBP","Update Coordinates"),
                                                         #actionButton("updatePlotButton","Update Plot")
                                                         withSpinner(plotOutput("megaPlot",height="450px"),color = "#00ffff", type = 6),
                                                         h5(helpText("Coordinates must be at least 200000 BP apart")),
                                                         uiOutput("plotStart"),
                                                         uiOutput("plotEnd"),
                                                         actionButton("updateBP","Update Coordinates"),
                                                         actionButton("resetBP","Reset Plot"),
                                                         # plotOutput("ideoPlot"),
                                                         downloadButton("plotDownload","Download Plot"),
                                                         selectInput("plotColor","Color Scheme",
                                                                     choices = list("Topo" = 1,
                                                                                    "Rainbow" = 2,
                                                                                    "Heat" = 3,
                                                                                    "Terrain" = 4,
                                                                                    "CM" = 5),multiple = FALSE, selected = 1))
                                         )))
                        ),
                        tabItem(tabName = "tab2",
                                fluidRow(
                                  box(title="App Details",
                                      h5(helpText("LD is calculated from 1000 Genomes Phase 1 (http://www.internationalgenome.org), and queried from HaploReg (http://archive.broadinstitute.org/mammals/haploreg/haploreg.php)
                                                  To perform similar queries in R please check out the haploR package!
                                                  For TAD visualization check out the Yue Lab (http://promoter.bx.psu.edu/hi-c/).
                                                  TAD locations are based off of those defined by Dixon et al in 'Topological domains in mammalian genomes identified by analysis of chromatin interactions'."
                                      ))),
                                  box(title="Development Team",
                                      h5(helpText("Programming: Jordan Creed, Travis Gerke")),
                                      h5(helpText("Scientific Input: Alvaro Monteiro")),
                                      h5(helpText("Website: http://travisgerke.com"))),
                                  box(title="Other resources",
                                      h5(helpText("Aiden Lab")),
                                      a("Juicebox", href="http://www.aidenlab.org/juicebox/", target="_blank"))
                                      ),
                                fluidRow(
                                  box(title="Notes",
                                      h5(helpText("If no SNPs are in LD above the specified threshold then a range of 53500 BP is applied to either side of the SNP.
                                                  If SNPs in LD exist, then the range is based on the minimum and maximum BP of all SNPs in LD and the TAD region.
                                                  This range is used for querying data from Oncotator, ENSEMBL, ClinVar and the Genome Browser.")))
                                      ))
                                      ))
                                )

###################################################################################################
server <- function(input, output) {
  
  #ensembl = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  # ensembl54=useMart("ENSEMBL_MART_ENSEMBL",dataset = "hsapiens_gene_ensembl")
  ensembl54 = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  #dbsnp = useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp")
  data(Dixon2012_IMR90)
  
  sample<-eventReactive(input$update1,{
    samplefile<-input$file1
    if(is.null(samplefile)){return()}
    sample1<-read.table(file=samplefile$datapath, sep= "\t", header= FALSE,stringsAsFactors= FALSE)
  })
  
  dat<-eventReactive(input$update1,{
    if(input$snpList==""){
      dat<-sample()
      snps<-dat[,1]
      x<-queryHaploreg(query = snps,ldThresh = as.numeric(input$value), ldPop = input$pop)
      #x<-x[,input$parameters]
      return(x)
    }
    if(input$snpList!=""){
      snps<-as.character(unlist(strsplit(input$snpList,",")))
      snps<-trimws(snps)
      x<-queryHaploreg(query = snps,ldThresh = input$value, ldPop = input$pop)
      x$chr <- as.numeric(as.character(x$chr))
      x$pos_hg38 <- as.numeric(as.character(x$pos_hg38))
      return(x)
    }
  })
  
  output$eTissues<-renderUI({
    dat<-dat()
    etest<-unlist(strsplit(as.character(dat$eQTL),";"))
    etest <- etest[!etest %in% c(".")]
    etest2<-unlist(strsplit(etest,","))
    # etest3<-matrix(etest2,nrow=length(etest), ncol=4, byrow = TRUE)
    # etest3<-as.data.frame(etest3)
    # etest3<-etest3[!duplicated(etest3$V2),]
    # opt<-etest3$V2
    if(is.null(etest2)){
      return(verbatimTextOutput("No Tissues to Show"))
    } else {
      etest3<-matrix(etest2,nrow=length(etest), ncol=4, byrow = TRUE)
      etest3<-as.data.frame(etest3)
      etest3<-etest3[!duplicated(etest3$V2),]
      opt<-etest3$V2
      return(checkboxGroupInput("tissue","Tissues",choices=opt, inline = TRUE))
    }
    # if(nrow(etest3)>=1){
    #   etest3<-matrix(etest2,nrow=length(etest), ncol=4, byrow = TRUE)
    #   etest3<-as.data.frame(etest3)
    #   etest3<-etest3[!duplicated(etest3$V2),]
    #   opt<-etest3$V2
    #   return(checkboxGroupInput("tissue","Tissues",choices=opt, inline = TRUE))
    # } else(verbatimTextOutput("No Tissues to Show"))
  })
  
  dat2<-eventReactive(input$update1,{
    if(input$snpList==""){
      dat<-sample()
      snps<-dat[,1]
      x<-queryRegulome(query = snps)
      x<-as.data.frame(x$res.table)
      x$score<-as.character(x$score)
      x$score_anno<-NA
      for (i in nrow(x)){
        if (x$score[i]=="1a"){x$score_anno[i]<-"eQTL + TF binding + matched TF motif + matched DNase Footprint + DNase peak"}
        else if (x$score[i]=="1b"){x$score_anno[i]<-"eQTL + TF binding + any motif + DNase Footprint + DNase peak"}
        else if (x$score[i]=="1c"){x$score_anno[i]<-"eQTL + TF binding + matched TF motif + DNase peak"}
        else if (x$score[i]=="1d"){x$score_anno[i]<-"eQTL + TF binding + any motif + DNase peak"}
        else if (x$score[i]=="1e"){x$score_anno[i]<-"eQTL + TF binding + matched TF motif"}
        else if (x$score[i]=="1f"){x$score_anno[i]<-"eQTL + TF binding / DNase peak"}
        else if (x$score[i]=="2a"){x$score_anno[i]<-"TF binding + matched TF motif + matched DNase Footprint + DNase peak"}
        else if (x$score[i]=="2b"){x$score_anno[i]<-"TF binding + any motif + DNase Footprint + DNase peak"}
        else if (x$score[i]=="2c"){x$score_anno[i]<-"TF binding + matched TF motif + DNase peak"}
        else if (x$score[i]=="3a"){x$score_anno[i]<-"TF binding + any motif + DNase peak"}
        else if (x$score[i]=="3b"){x$score_anno[i]<-"TF binding + matched TF motif"}
        else if (x$score[i]=="4"){x$score_anno[i]<-"TF binding + DNase peak"}
        else if (x$score[i]=="5"){x$score_anno[i]<-"TF binding or DNase peak"}
        else {x$score_anno[i]<-"Other"}
      }
      #x<-x[,input$parameters]
      return(x)
    }
    if(input$snpList!=""){
      snps<-as.character(unlist(strsplit(input$snpList,",")))
      snps<-trimws(snps)
      x<-queryRegulome(query = snps)
      x<-as.data.frame(x$res.table)
      x$score<-as.character(x$score)
      x$score_anno<-NA
      for (i in 1:nrow(x)){
        if (x$score[i]=="1a"){x$score_anno[i]<-"eQTL + TF binding + matched TF motif + matched DNase Footprint + DNase peak"}
        else if (x$score[i]=="1b"){x$score_anno[i]<-"eQTL + TF binding + any motif + DNase Footprint + DNase peak"}
        else if (x$score[i]=="1c"){x$score_anno[i]<-"eQTL + TF binding + matched TF motif + DNase peak"}
        else if (x$score[i]=="1d"){x$score_anno[i]<-"eQTL + TF binding + any motif + DNase peak"}
        else if (x$score[i]=="1e"){x$score_anno[i]<-"eQTL + TF binding + matched TF motif"}
        else if (x$score[i]=="1f"){x$score_anno[i]<-"eQTL + TF binding / DNase peak"}
        else if (x$score[i]=="2a"){x$score_anno[i]<-"TF binding + matched TF motif + matched DNase Footprint + DNase peak"}
        else if (x$score[i]=="2b"){x$score_anno[i]<-"TF binding + any motif + DNase Footprint + DNase peak"}
        else if (x$score[i]=="2c"){x$score_anno[i]<-"TF binding + matched TF motif + DNase peak"}
        else if (x$score[i]=="3a"){x$score_anno[i]<-"TF binding + any motif + DNase peak"}
        else if (x$score[i]=="3b"){x$score_anno[i]<-"TF binding + matched TF motif"}
        else if (x$score[i]=="4"){x$score_anno[i]<-"TF binding + DNase peak"}
        else if (x$score[i]=="5"){x$score_anno[i]<-"TF binding or DNase peak"}
        else {x$score_anno[i]<-"Other"}
      }
      return(x)
    }
  })
  
  snps<-eventReactive(input$update1,{
    if(input$snpList==""){
      dat<-sample()
      snps<-dat[,1]
      return(snps)
    }
    if(input$snpList!=""){
      snps<-as.character(unlist(strsplit(input$snpList,",")))
      snps<-trimws(snps)
      return(snps)
    }
  })
  
  in_tad<-eventReactive(input$update1,{
    tad<-fread("http://compbio.med.harvard.edu/modencode/webpage/hic/IMR90_domains_hg19.bed")
    colnames(tad)<-c("chr","start_position","end_position")
    tad$chr<-gsub("chr","",tad$chr)
    tad$chr<-as.numeric(tad$chr)
    tad<-tad[!is.na(tad$chr),]
    snps<-snps()
    dat<-dat()
    dat<-dat[dat$rsID %in% snps,]
    snp_pos<-dat$pos_hg38
    tad<-tad[tad$chr==max(dat$chr,na.rm=TRUE),]
    # in_tad<-tad[between(snp_pos,tad$start_position, tad$end_position)]
    # in_tad <- as.data.frame(in_tad)
    in_tad <- tad[tad$start_position<= snp_pos & tad$end_position>=snp_pos,]
    return(in_tad)
  })
  
  in_lad<-eventReactive(input$update1,{
    lad<-fread("http://compbio.med.harvard.edu/modencode/webpage/lad/human.fibroblast.DamID.hg19.bed")
    colnames(lad)<-c("chr","start","end","dunno")
    lad$chr<-gsub("chr","",lad$chr)
    lad$chr<-as.numeric(lad$chr)
    lad<-lad[!is.na(lad$chr),]
    snps<-snps()
    dat<-dat()
    dat<-dat[dat$rsID %in% snps,]
    snp_pos<-dat$pos_hg38
    lad<-lad[lad$chr==max(dat$chr,na.rm=TRUE),]
    # in_tad<-tad[between(snp_pos,tad$start_position, tad$end_position)]
    # in_tad <- as.data.frame(in_tad)
    #in_lad <- tad[tad$start_position<= snp_pos & tad$end_position>=snp_pos,]
    return(lad)
  })
  
  output$tadBoundaries<-renderText({
    in_tad<-in_tad()
    if(nrow(in_tad)<1){return(paste0("Not in a TAD!"))}
    else({return(paste0("In a TAD! The TAD ranges from ",in_tad$start_position," to ",in_tad$end_position))})
  })
  
  output$eTable1<-renderTable({
    dat<-dat()
    etest<-unlist(strsplit(as.character(dat$eQTL),";"))
    etest <- etest[!etest %in% c(".")]
    etest2<-unlist(strsplit(etest,","))
    if(is.null(etest2)){
      Source<-NA
      Tissue<-NA
      Gene<-NA
      p<-NA
      empty_dat<-as.data.frame(cbind(Source,Tissue,Gene,p))
      return(empty_dat)
    } else{
      etest3<-matrix(etest2,nrow=length(etest), ncol=4,byrow=TRUE)
      etest3<-as.data.frame(etest3)
      colnames(etest3)<-c("Source","Tissue","Gene","p")
      return(etest3[etest3$Tissue %in% input$tissue,])
    }
    # 
    # etest3<-matrix(etest2,nrow=length(etest), ncol=4,byrow=TRUE)
    # etest3<-as.data.frame(etest3)
    # colnames(etest3)<-c("Source","Tissue","Gene","p")
    # return(etest3[etest3$Tissue %in% input$tissue,])
  })
  
  values <- reactiveValues(tmp_min = 0, tmp_max=999)
  
  total_min<-eventReactive(input$update1,{
    tad<-in_tad()
    dat<-dat()
    if(nrow(tad)>=1){
      total_min<-min(c(min(dat$pos_hg38,na.rm=TRUE), tad$start_position))
      return(as.numeric(total_min))
    }
    else if (nrow(tad)<1 & nrow(dat)>1){
      total_min<-min(dat$pos_hg38,na.rm=TRUE) 
      return(as.numeric(total_min))}
    else {
      total_min<-min(dat$pos_hg38,na.rm=TRUE)-53500 
      return(as.numeric(total_min))}
  })
  
  total_max<-eventReactive(input$update1,{
    tad<-in_tad()
    dat<-dat()
    if(nrow(tad)>=1){
      total_max<-max(c(max(dat$pos_hg38,na.rm=TRUE), tad$end_position))
      return(as.numeric(total_max))
    }
    else if (nrow(tad)<1 & nrow(dat)>1){
      total_max<-max(dat$pos_hg38,na.rm=TRUE) 
      return(as.numeric(total_max))}
    else {
      total_max<-max(dat$pos_hg38,na.rm=TRUE) + 53500
      return(as.numeric(total_max))}
  })
  
  observeEvent(input$updateBP, {
    values$tmp_min <- input$plotStartBP
    values$tmp_max <- input$plotEndBP
  })
  
  observeEvent(input$resetBP, {
    values$tmp_min <- 0
    values$tmp_max <- 999
  })
  
  output$hic1<-renderUI({
    x<-snps()
    y<-dat()
    total_min<-total_min()
    total_max<-total_max()
    if (length(x)>1){
      a("Take me to HIC Browser", href=paste0("http://promoter.bx.psu.edu/hi-c/view.php?species=human&assembly=hg19&source=inside&tissue=GM12878&type=Lieberman-raw&c_url=&transfer=&chr=chr",max(as.numeric(y$chr),na.rm=TRUE),"&start=",total_min,"&end=",total_max,"&sessionID=&browser=none"), target="_blank")
    } else if (length(x)==1){
      a("Take me to HIC Browser", href=paste0("http://promoter.bx.psu.edu/hi-c/view.php?species=human&assembly=hg19&source=inside&tissue=GM12878&type=Lieberman-raw&resolution=25&c_url=&transfer=&gene=",x,"&sessionID=&browser=none"), target="_blank")
    }
  })
  
  output$clinical1<-renderUI({
    x<-snps()
    y<-dat()
    total_min<-total_min()
    total_max<-total_max()
    a("Take me to ClinVar", href=paste0("https://www.ncbi.nlm.nih.gov/clinvar/?term=",max(as.numeric(y$chr),na.rm = TRUE),"%5Bchr%5D+AND+",total_min,"%3A",total_max,"%5Bchrpos37%5D"), target="_blank")
  })
  
  output$ucsc1<-renderUI({
    x<-snps()
    y<-dat()
    total_min<-total_min()
    total_max<-total_max()
    a("Take me to Genome Browser", href=paste0("https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr",max(as.numeric(y$chr),na.rm = TRUE),"%3A",total_min,"%2D",total_max,"&hgsid=598506407_cis2LZUJLabCsy1N2YPEuJv8vbBZ"), target="_blank")
  })
  
  output$eqtl1<-renderUI({
    x<-snps()
    y<-dat()
    total_min<-total_min()
    total_max<-total_max()
    if (length(x)>1){
      a("Take me to GTEx", href=paste0("https://www.gtexportal.org/home/browseEqtls?location=chr",max(as.numeric(y$chr),na.rm = TRUE),":",total_min,"-",total_max), target="_blank")
    } else if (length(x)==1){
      a("Take me to GTEx", href=paste0("https://www.gtexportal.org/home/snp/",x), target="_blank")
    }
  })
  
  
  
  output$LDtable1<-renderTable({
    x<-dat()
    #x<-x[,input$parameters]
    #x[,c("query_snp_rsid","rsID","pos_hg38","r2",input$parameters)]
    x[,c("rsID",input$parameters)]
    #return(x[,input$parameters])
  })
  
  output$LDtable2<-renderTable({
    x<-dat2()
    x[,c("rsid",input$parameters2)]
  })
  
  output$geneTable<-renderTable({
    ld<-dat()
    chr<-max(ld$chr,na.rm=TRUE)
    total_min<-total_min()
    total_max<-total_max()
    
    genes<-getBM(attributes = c("hgnc_symbol","start_position","end_position"),
                 filters=c("chromosomal_region"), values=paste0(chr,":",total_min,":",total_max),mart = ensembl54)
    return(genes)
  })
  
  output$geneDownload<-downloadHandler(
    filename = function() { paste("geneList", '.csv', sep='') },
    content = function(file) {
      ld<-dat()
      chr<-max(as.numeric(ld$chr),na.rm=TRUE)
      total_min<-total_min()
      total_max<-total_max()
      
      genes<-getBM(attributes = c("hgnc_symbol","start_position","end_position"),
                   filters=c("chromosomal_region"), values=paste0(chr,":",total_min,":",total_max),mart = ensembl54)
      
      write.csv(genes, file)
    }
  )
  
  output$oncoTable<-renderTable({
    ld<-dat()
    chr<-max(as.numeric(ld$chr),na.rm=TRUE)
    total_min<-total_min()
    total_max<-total_max()
    
    x<-fromJSON(paste0("http://portals.broadinstitute.org/oncotator/genes/",chr,"_",total_min,"_",total_max,"/"))
    
    genes<-as.data.frame(x[[1]])
    
    for (i in 2:length(x)){
      gene_dat<-as.data.frame(x[[i]])
      genes<-rbind(genes, gene_dat)
    }
    
    genes<-genes[,c("gene",input$oncoParameters1,input$oncoParameters2,input$oncoParameters3,input$oncoParameters4)]
    return(genes)
    
  })
  
  # output$plot1<-renderPlotly({
  #   # create plotting without having to search 
  #   ld<-dat()
  #   ld<-ld[ld$pos_hg38!="",]
  #   query_snps<-ld[ld$is_query_snp==1,]
  #   ld_snps<-ld[ld$is_query_snp==0,]
  #   
  #   tad<-in_tad()
  #   
  #   total_min<-total_min()
  #   total_max<-total_max()
  #   
  #   genes<-getBM(attributes = c("hgnc_symbol","start_position","end_position"),
  #                filters=c("chromosomal_region"), values=paste0(max(ld$chr,na.rm = TRUE),":",total_min,":",total_max),mart = ensembl54)
  #   colnames(genes)<-c("Symbol","Start","End")
  #   
  #   ldBlocks<-ggplot(ld)+
  #     geom_segment(data=ld[ld$is_query_snp==1,],aes(x=as.vector(tapply(ld$pos_hg38, ld$query_snp_rsid, min)),y=1,xend=as.vector(tapply(ld$pos_hg38, ld$query_snp_rsid, max)),yend=1,color=as.factor(query_snp_rsid), size=30, text=paste0("SNP: ",query_snp_rsid)), alpha=0.5)+
  #     geom_segment(data=genes,aes(x=Start,y=3,xend=End,yend=3,color=Symbol,size=30, text=paste0("Symbol: ",Symbol,"\n", "Start: ",Start,"\n", "End",End)),alpha=0.5)+
  #     geom_vline(data = query_snps, aes(xintercept=pos_hg38, text=paste0("SNP: ",query_snp_rsid))) +
  #     #geom_vline(data = ld_snps, aes(xintercept=pos_hg38, alpha=0.1, text=paste0("SNP: ",rsID)), color="grey")+ # add different color from query snps to make more visible?
  #     annotate("text",x=total_min, y=1.25, label="LD", color="purple", angle=90)+
  #     annotate("text",x=total_min, y=3.25, label="Genes", color="purple", angle=90)+
  #     theme(legend.position = "none",axis.text.y=element_blank(),axis.title.y=element_blank(),panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           panel.background = element_blank()) + 
  #     xlab("BP") + coord_cartesian(ylim=c(0.75,3.5), xlim = c(total_min-50000, total_max+50000))+
  #     scale_y_continuous(breaks = c(1,2,3))  
  #   
  #   if(nrow(tad)>=1){
  #     
  #     ldBlocks<-ldBlocks + 
  #       geom_segment(data=tad, aes(x=tad$start_position, y=2, xend=tad$end_position, yend=2, size=30, text=paste0("Start: ",start_position,"\n","End",end_position)))+
  #       annotate("text",x=total_min, y=2.25, label="TADs", color="purple", angle=90)
  #     
  #   } else {
  #     ldBlocks<-ldBlocks
  #   }
  #   
  #   if(nrow(ld_snps)>=1){
  #     ldBlocks<-ldBlocks + geom_vline(data = ld_snps, aes(xintercept=pos_hg38, alpha=0.1, text=paste0("SNP: ",rsID)), color="grey")
  #   }else {
  #     ldBlocks<-ldBlocks
  #   }
  #   #return(ldBlocks)
  #   ggplotly(ldBlocks, tooltip="text") %>% 
  #     layout(autosize=TRUE)
  # })
  
  ########### Other HIC plots if needed later 
  # output$hicPlot<-renderPlot({
  #   data(Dixon2012_IMR90)
  #   ld<-dat()
  #   chrX<-max(ld$chr,na.rm=TRUE)
  #   hic_dat <- extractRegion(hic_imr90_40[[paste0("chr",chrX,"chr",chrX)]],chr=paste0("chr",chrX),
  #                            from=total_min(), to=total_max())
  #   hic_matrix<-as.matrix(intdata(hic_dat))
  #   phic = plotHic(hic_matrix,chrom=paste0("chr",chrX),
  #           chromstart=min(as.numeric(colnames(hic_matrix))),
  #           chromend=max(as.numeric(colnames(hic_matrix))),
  #           max_y = 20,zrange=c(0,28),palette = topo.colors,flip=FALSE)
  #   labelgenome(chrom=paste0("chr",chrX),chromstart=total_min(),chromend=total_max(),
  #               side=1,scipen=40,n=1,scale="bp")
  #   addlegend(phic[[1]],palette=phic[[2]],title="score",side="right",bottominset=0.4,
  #             topinset=0,xoffset=-.035,labelside="left",width=0.025,title.offset=0.035)
  #   labelplot(title="Dixon IMR0 HIC",lettercex=4,titlecex=1.5,titlecol="blue",titleadj = 0.5)
  # })
  # 
  # output$hicPlot2<-renderPlot({
  #   data(Dixon2012_IMR90)
  #   ld<-dat()
  #   chrX<-max(ld$chr,na.rm=TRUE)
  #   hic_dat <- extractRegion(hic_imr90_40[[paste0("chr",chrX,"chr",chrX)]],chr=paste0("chr",chrX),
  #                            from=total_min(), to=total_max())
  #   plot(hic_dat, tracks=list(tads_imr90), maxrange=20)
  # })
  #
  # output$ideoPlot <- renderPlot({
  #   ld<-dat()
  #   chrX<-max(ld$chr,na.rm=TRUE)
  #   plotIdeogram(genome = "hg19", subchr = paste0("chr",chrX),
  #                which = GRanges(seqnames = paste0("chr",chrX),
  #                                IRanges(start = total_min(),end = total_max())),
  #                color = "cyan", aspect.ratio = 1/7)
  # })
  
  # output$plotStart<-renderUI({
  #   numericInput("plotStartBP",label = "Starting Coordinates (BP)", value=total_min())
  # })
  # 
  # output$plotEnd<-renderUI({
  #   numericInput("plotEndBP",label = "Ending Coordinates (BP)", value=total_max())
  # })
  
  output$megaPlot<-renderPlot({
    #data(Dixon2012_IMR90)
    ld<-dat()
    chrX<-max(ld$chr,na.rm=TRUE)
    
    minBP <- ifelse(values$tmp_min==0,total_min(),values$tmp_min)
    maxBP <- ifelse(values$tmp_max==999,total_max(),values$tmp_max)
    
    hic_dat <- extractRegion(hic_imr90_40[[paste0("chr",chrX,"chr",chrX)]],chr=paste0("chr",chrX),
                             from=minBP, to=maxBP)
    hic_matrix<-as.matrix(intdata(hic_dat))
    #rm(hic_imr90_40)
    
    genes<-getBM(attributes = c("hgnc_symbol","start_position","end_position"),
                 filters=c("chromosomal_region"), values=paste0(chrX,":",minBP,":",maxBP),mart = ensembl54)
    colnames(genes)<-c("Symbol","Start","End")
    
    tads <- as.data.frame(tads_imr90)
    #tads <- tads[tads$seqnames==paste0("chr",chrX) & tads$start>=minBP & tads$start<=maxBP,]

    #rm(hic_imr90_40,tads_imr90)
    ###########################################################################################
    
    mat_layout<-matrix(c(1,2,3,4,1,2,3,4),nrow=4,ncol=2)
    layout(mat_layout,c(4,4,4,4),c(2.25,1.25,0.5,0.5))
    par(mar=c(0.5, 4.5, 0.5, 0.5))
    
    phic = plotHic(hic_matrix,chrom=paste0("chr",chrX),
                   chromstart=min(as.numeric(colnames(hic_matrix))),
                   chromend=max(as.numeric(colnames(hic_matrix))),
                   max_y = 20,zrange=c(0,28),palette = ifelse(input$plotColor==1,topo.colors,
                                                              ifelse(input$plotColor==2,rainbow,
                                                                     ifelse(input$plotColor==3,heat.colors,
                                                                            ifelse(input$plotColor==4,terrain.colors,cm.colors)))),
                   flip=FALSE)
    labelgenome(chrom=paste0("chr",chrX),chromstart=minBP,chromend=maxBP,
                side=1,scipen=40,n=1,scale="bp")
    addlegend(phic[[1]],palette=phic[[2]],title="score",side="right",bottominset=0.4,
              topinset=0,xoffset=-.035,labelside="left",width=0.025,title.offset=0.035)
    mtext("HIC Intensities",side=2,line=1.75,cex=.75,font=2)
    
    plot(c(1,1),xlim=c(minBP,maxBP),ylim=c(0,1),type ='n',bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i")
    segments(x0=genes$Start,y0=0.5,x1=genes$End,y1=0.5,lwd=30, col = ifelse(input$plotColor==1,topo.colors(n=nrow(genes),alpha = 0.7),
                                                                            ifelse(input$plotColor==2,rainbow(n=nrow(genes),alpha = 0.7),
                                                                                   ifelse(input$plotColor==3,heat.colors(n=nrow(genes),alpha = 0.7),
                                                                                          ifelse(input$plotColor==4,terrain.colors(n=nrow(genes),alpha = 0.7),rev(cm.colors(n=nrow(genes),alpha = 0.7)))))), lend=1)
    text(x=(genes$Start+genes$End)/2,y=c(0.7,0.3,0.8,0.2),labels=genes$Symbol,col = ifelse(input$plotColor==1,topo.colors(n=nrow(genes),alpha = 0.7),
                                                                                           ifelse(input$plotColor==2,rainbow(n=nrow(genes),alpha = 0.7),
                                                                                                  ifelse(input$plotColor==3,heat.colors(n=nrow(genes),alpha = 0.7),
                                                                                                         ifelse(input$plotColor==4,terrain.colors(n=nrow(genes),alpha = 0.7),rev(cm.colors(n=nrow(genes),alpha = 0.7)))))))
    mtext("Genes",side=2,line=1.75,cex=.75,font=2)
    
    plot(c(1,1),xlim=c(minBP,maxBP),ylim=c(0,1),type ='n',bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i")
    abline(v=ld[ld$is_query_snp==0,]$pos_hg38, col="grey", lend=1) # lwd=6
    # abline(v=ld[ld$is_query_snp==1,]$pos_hg38,col=colorspace::rainbow_hcl(n=nrow(ld[ld$is_query_snp==1,])), lend=1)
    abline(v=ld[ld$is_query_snp==1,]$pos_hg38,col=ifelse(input$plotColor==1,topo.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7),
                                                         ifelse(input$plotColor==2,rainbow(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7),
                                                                ifelse(input$plotColor==3,heat.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7),
                                                                       ifelse(input$plotColor==4,rev(terrain.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7)),cm.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7))))), lend=1)
    mtext("LD",side=2,line=1.75,cex=.75,font=2)
    
    plot(c(1,1),xlim=c(minBP,maxBP),ylim=c(0,1),type ='n',bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i")
    # segments(x0=tads[tads$seqnames==paste0("chr",chrX) & tads$start>=minBP & tads$start<=maxBP,]$start,
    #          y0=0.5,
    #          x1=tads[tads$seqnames==paste0("chr",chrX) & tads$start>=minBP & tads$start<=maxBP,]$end,
    #          y1=0.5, lwd=30,
    #          col=topo.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX) & tads$start>=minBP & tads$start<=maxBP,])),
    #          lend=1)
    segments(x0=tads[tads$seqnames==paste0("chr",chrX),]$start,
             y0=0.5,
             x1=tads[tads$seqnames==paste0("chr",chrX),]$end,
             y1=0.5, lwd=30,
             # col=topo.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),])),
             col=ifelse(input$plotColor==1,topo.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),
                        ifelse(input$plotColor==2,rainbow(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),
                               ifelse(input$plotColor==3,heat.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),
                                      ifelse(input$plotColor==4,terrain.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),cm.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7))))),
             lend=1)
    #axis(1, at=c(input$plotStartBP,(input$plotStartBP+input$plotEndBP)/2,input$plotEndBP), labels=c(as.character(input$plotStartBP),as.character((input$plotStartBP+input$plotEndBP)/2,input$plotEndBP))
    mtext("TADs",side=2,line=1.75,cex=.75,font=2)
    
    
  })
  
  output$plotDownload<-downloadHandler(
    filename = function() { paste("episnpR", '.pdf', sep='') },
    content = function(file) {
      pdf(file)
      ld<-dat()
      chrX<-max(ld$chr,na.rm=TRUE)
      
      minBP <- ifelse(values$tmp_min==0,total_min(),values$tmp_min)
      maxBP <- ifelse(values$tmp_max==999,total_max(),values$tmp_max)
      
      hic_dat <- extractRegion(hic_imr90_40[[paste0("chr",chrX,"chr",chrX)]],chr=paste0("chr",chrX),
                               from=minBP, to=maxBP)
      hic_matrix<-as.matrix(intdata(hic_dat))
      
      genes<-getBM(attributes = c("hgnc_symbol","start_position","end_position"),
                   filters=c("chromosomal_region"), values=paste0(chrX,":",minBP,":",maxBP),mart = ensembl54)
      colnames(genes)<-c("Symbol","Start","End")
      
      tads <- as.data.frame(tads_imr90)

      ###########################################################################################
      
      mat_layout<-matrix(c(1,2,3,4,1,2,3,4),nrow=4,ncol=2)
      layout(mat_layout,c(4,4,4,4),c(2.25,1.25,0.5,0.5))
      par(mar=c(0.5, 4.5, 0.5, 0.5))
      
      phic = plotHic(hic_matrix,chrom=paste0("chr",chrX),
                     chromstart=min(as.numeric(colnames(hic_matrix))),
                     chromend=max(as.numeric(colnames(hic_matrix))),
                     max_y = 20,zrange=c(0,28),palette = ifelse(input$plotColor==1,topo.colors,
                                                                ifelse(input$plotColor==2,rainbow,
                                                                       ifelse(input$plotColor==3,heat.colors,
                                                                              ifelse(input$plotColor==4,terrain.colors,cm.colors)))),
                     flip=FALSE)
      labelgenome(chrom=paste0("chr",chrX),chromstart=minBP,chromend=maxBP,
                  side=1,scipen=40,n=1,scale="bp")
      addlegend(phic[[1]],palette=phic[[2]],title="score",side="right",bottominset=0.4,
                topinset=0,xoffset=-.035,labelside="left",width=0.025,title.offset=0.035)
      mtext("HIC Intensities",side=2,line=1.75,cex=.75,font=2)
      
      plot(c(1,1),xlim=c(minBP,maxBP),ylim=c(0,1),type ='n',bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i")
      segments(x0=genes$Start,y0=0.5,x1=genes$End,y1=0.5,lwd=30, col = ifelse(input$plotColor==1,topo.colors(n=nrow(genes),alpha = 0.7),
                                                                              ifelse(input$plotColor==2,rainbow(n=nrow(genes),alpha = 0.7),
                                                                                     ifelse(input$plotColor==3,heat.colors(n=nrow(genes),alpha = 0.7),
                                                                                            ifelse(input$plotColor==4,terrain.colors(n=nrow(genes),alpha = 0.7),rev(cm.colors(n=nrow(genes),alpha = 0.7)))))), lend=1)
      text(x=(genes$Start+genes$End)/2,y=c(0.7,0.3,0.8,0.2),labels=genes$Symbol,col = ifelse(input$plotColor==1,topo.colors(n=nrow(genes),alpha = 0.7),
                                                                                             ifelse(input$plotColor==2,rainbow(n=nrow(genes),alpha = 0.7),
                                                                                                    ifelse(input$plotColor==3,heat.colors(n=nrow(genes),alpha = 0.7),
                                                                                                           ifelse(input$plotColor==4,terrain.colors(n=nrow(genes),alpha = 0.7),rev(cm.colors(n=nrow(genes),alpha = 0.7)))))))
      mtext("Genes",side=2,line=1.75,cex=.75,font=2)
      
      plot(c(1,1),xlim=c(minBP,maxBP),ylim=c(0,1),type ='n',bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i")
      abline(v=ld[ld$is_query_snp==0,]$pos_hg38, col="grey", lend=1) # lwd=6
      # abline(v=ld[ld$is_query_snp==1,]$pos_hg38,col=colorspace::rainbow_hcl(n=nrow(ld[ld$is_query_snp==1,])), lend=1)
      abline(v=ld[ld$is_query_snp==1,]$pos_hg38,col=ifelse(input$plotColor==1,topo.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7),
                                                           ifelse(input$plotColor==2,rainbow(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7),
                                                                  ifelse(input$plotColor==3,heat.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7),
                                                                         ifelse(input$plotColor==4,rev(terrain.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7)),cm.colors(n=nrow(ld[ld$is_query_snp==1,]),alpha = 0.7))))), lend=1)
      mtext("LD",side=2,line=1.75,cex=.75,font=2)
      
      plot(c(1,1),xlim=c(minBP,maxBP),ylim=c(0,1),type ='n',bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i")
      segments(x0=tads[tads$seqnames==paste0("chr",chrX),]$start,
               y0=0.5,
               x1=tads[tads$seqnames==paste0("chr",chrX),]$end,
               y1=0.5, lwd=30,
               # col=topo.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),])),
               col=ifelse(input$plotColor==1,topo.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),
                          ifelse(input$plotColor==2,rainbow(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),
                                 ifelse(input$plotColor==3,heat.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),
                                        ifelse(input$plotColor==4,terrain.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7),cm.colors(n=nrow(tads[tads$seqnames==paste0("chr",chrX),]),alpha = 0.7))))),
               lend=1)
      mtext("TADs",side=2,line=1.75,cex=.75,font=2)
      
      dev.off()
    }
  )

  output$plotStart<-renderUI({
    numericInput("plotStartBP",label = "Starting Coordinates (BP)", value=total_min())
  })
  
  output$plotEnd<-renderUI({
    numericInput("plotEndBP",label = "Ending Coordinates (BP)", value=total_max())
  })
  
  
  
}


################################################################################################### 
shinyApp(ui = ui, server = server)

