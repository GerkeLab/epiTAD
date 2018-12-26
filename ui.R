function(request) {
  dashboardPage(
    dashboardHeader(title = "epiTAD"),
    dashboardSidebar(sidebarMenu(
      id = "tabs",
      menuItem("SNPs", tabName = "tab1"),
      menuItem("Info", tabName = "tab2")
    )),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "tab1",
          fluidRow(
            box(
              title = "Query SNPs",
              textInput("snpList", "Enter SNP rsIDs (comma separated)", value = ""),
              h5(helpText("Upload SNP List (one SNP per line)")),
              fileInput("file1", "Choose a file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              tags$hr(),
              actionButton("update1", "Perform query"),
              bookmarkButton(class = "pull-right")
            ),
            tabBox(
              title = "Select Output",
              tabPanel(
                "Source",
                selectInput("pop", "Population", c("EUR", "AFR", "AMR", "ASN"), selected = "EUR"),
                sliderInput("value", "LD threshold", min = 0, max = 1, value = 0.8)
              ),
              tabPanel(
                "HaploReg",
                checkboxGroupInput("parameters", "HaploR", c(
                  "Chromosome" = "chr", "Position" = "pos_hg38", "r2" = "r2", "Query SNP" = "query_snp_rsid",
                  "D'" = "D'", "Query SNP" = "is_query_snp",
                  "Reference allele" = "ref", "Alternative allele" = "alt", "LD(AFR)" = "AFR",
                  "LD(AMR)" = "AMR", "LD(ASN)" = "ASN", "LD(EUR)" = "EUR",
                  "GERP scores" = "GERP_cons", "SiPhy scores" = "SiPhy_cons", "Chromatin States" = "Chromatin_States",
                  "Imputed Chromatin States" = "Chromatin_States_Imputed", "Chromatin Marks" = "Chromatin_Marks", "DNAse" = "DNAse",
                  "Proteins", "eQTL", "GWAS study name" = "gwas",
                  "GRASP study name" = "grasp", "Motifs", "GENCODE transcript ID" = "GENCODE_id",
                  "GENCODE gene name" = "GENCODE_name", "GENCODE direction" = "GENCODE_direction", "GENCODE distance" = "GENCODE_distance",
                  "NCBI Reference Sequence Accession number" = "RefSeq_id", "NCBI Reference Sequence name" = "RefSeq_name", "NCBI Reference Sequence direction" = "RefSeq_direction",
                  "NCBI Reference Sequence distance" = "RefSeq_distance", "Annotated proteins" = "dbSNP_functional_annotation"
                ), inline = TRUE, selected = c("query_snp_rsid", "pos_hg38", "r2"))
              ),
              tabPanel(
                "RegulomeDB",
                checkboxGroupInput("parameters2", "Regulome", c(
                  "Chromosome" = "#chromosome",
                  "Coordinates" = "coordinate",
                  "Hits" = "hits", "Score" = "score_anno"
                ), selected = c("#chromosome", "coordinate", "hits", "score_anno"))
              ),
              tabPanel(
                "eQTL",
                h5(helpText("If no eQTLs, this section will be empty")),
                uiOutput("eTissues")
              ),
              tabPanel(
                "Oncotator",
                checkboxGroupInput("oncoParameters1", "General", c(
                  "ACHILLES_Lineage_Results_Top_Genes", "CCLE_By_Gene_total_mutations_in_gene",
                  "COSMIC_FusionGenes_fusion_genes",
                  "COSMIC_Tissue_tissue_types_affected", "COSMIC_Tissue_total_alterations_in_gene",
                  "Familial_Cancer_Genes_Reference", "Familial_Cancer_Genes_Syndrome",
                  "Familial_Cancer_Genes_Synonym", "HGNC_Accession.Numbers",
                  "HumanDNARepairGenes_Role", "MutSig.Published.Results_Published_Results",
                  "TCGAScape_Amplification_Peaks", "TCGAScape_Deletion_Peaks",
                  "TUMORScape_Amplification_Peaks", "TUMORScape_Deletion_Peaks",
                  "alt_allele",
                  "build", "chr",
                  "class", "end",
                  "gene", "protein_change",
                  "ref_allele", "start",
                  "strand", "transcripts"
                ), inline = TRUE),
                checkboxGroupInput("oncoParameters2", "Cancer Gene Census", c(
                  "CGC_Cancer.Germline.Mut", "CGC_Cancer.Molecular.Genetics",
                  "CGC_Cancer.Somatic.Mut", "CGC_Cancer.Syndrome",
                  "CGC_Chr", "CGC_Chr.Band",
                  "CGC_GeneID", "CGC_Mutation.Type",
                  "CGC_Name", "CGC_Other.Germline.Mut",
                  "CGC_Other.Syndrome.Disease", "CGC_Tissue.Type",
                  "CGC_Translocation.Partner", "CGC_Tumour.Types...Somatic.Mutations.",
                  "CGC_Tumour.Types..Germline.Mutations."
                ), inline = TRUE),
                checkboxGroupInput("oncoParameters3", "HUGO Gene Nomenclature Committee", c(
                  "HGNC_Approved.Name", "HGNC_CCDS.IDs",
                  "HGNC_Chromosome", "HGNC_Date.Modified",
                  "HGNC_Date.Name.Changed", "HGNC_Date.Symbol.Changed",
                  "HGNC_Ensembl.Gene.ID", "HGNC_Ensembl.ID.supplied.by.Ensembl.",
                  "HGNC_Entrez.Gene.ID", "HGNC_Entrez.Gene.ID.supplied.by.NCBI.",
                  "HGNC_Enzyme.IDs", "HGNC_Gene.family.description",
                  "HGNC_HGNC.ID", "HGNC_Locus.Group",
                  "HGNC_Locus.Type", "HGNC_Name.Synonyms",
                  "HGNC_OMIM.ID.supplied.by.NCBI.", "HGNC_Previous.Names",
                  "HGNC_Previous.Symbols", "HGNC_Primary.IDs",
                  "HGNC_Pubmed.IDs", "HGNC_Record.Type",
                  "HGNC_RefSeq.IDs", "HGNC_RefSeq.supplied.by.NCBI.",
                  "HGNC_Secondary.IDs", "HGNC_Status",
                  "HGNC_Synonyms", "HGNC_UCSC.ID.supplied.by.UCSC.",
                  "HGNC_UniProt.ID.supplied.by.UniProt.", "HGNC_VEGA.IDs"
                ), inline = TRUE),
                checkboxGroupInput("oncoParameters4", "UniProt", c(
                  "UniProt_AA_experimental_info", "UniProt_AA_natural_variation",
                  "UniProt_AA_region", "UniProt_AA_site",
                  "UniProt_DrugBank", "UniProt_GO_Biological_Process",
                  "UniProt_GO_Cellular_Component", "UniProt_GO_Molecular_Function",
                  "UniProt_alt_uniprot_accessions", "UniProt_uniprot_accession",
                  "UniProt_uniprot_entry_name"
                ), inline = TRUE)
              )
            )
          ),
          fluidRow(
            tabBox(
              title = "Variant Annotation",
              tabPanel(
                "HaploReg",
                withSpinner(tableOutput("LDtable1"), color = "#00ffff", type = 6)
              ),
              tabPanel(
                "RegulomeDB",
                withSpinner(tableOutput("LDtable2"), color = "#00ffff", type = 6)
              ),
              tabPanel(
                "TADs",
                withSpinner(textOutput("tadBoundaries"), color = "#00ffff", type = 6),
                uiOutput("hic1")
              )
            ),
            tabBox(
              title = "Gene Annotation",
              tabPanel(
                "ENSEMBL",
                h5(helpText("Genes spanned by the greater of the LD or TAD region")),
                withSpinner(tableOutput("geneTable"), color = "#00ffff", type = 6),
                downloadButton("geneDownload", "Download")
              ),
              tabPanel(
                "Oncotator",
                withSpinner(tableOutput("oncoTable"), color = "#00ffff", type = 6)
              ),
              tabPanel(
                "eQTL",
                tableOutput("eTable1"),
                uiOutput("eqtl1")
              )
            ),
            column(12,
              align = "center", offset = 3,
              tabBox(
                title = "Other",
                tabPanel(
                  "Links",
                  uiOutput("clinical1"),
                  uiOutput("ucsc1")
                ),
                tabPanel(
                  "Figure",
                  withSpinner(plotOutput("megaPlot", height = "450px"), color = "#00ffff", type = 6),
                  h5(helpText("Coordinates must be at least 200000 BP apart")),
                  uiOutput("plotStart"),
                  uiOutput("plotEnd"),
                  actionButton("updateBP", "Update Coordinates"),
                  actionButton("resetBP", "Reset Plot"),
                  downloadButton("plotDownload", "Download Plot"),
                  selectInput("plotColor", "Color Scheme",
                    choices = list(
                      "Topo" = 1,
                      "Rainbow" = 2,
                      "Heat" = 3,
                      "Terrain" = 4,
                      "CM" = 5
                    ), multiple = FALSE, selected = 1
                  )
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "tab2",
          fluidRow(
            box(
              title = "App Details",
              h5(helpText("LD is calculated from 1000 Genomes Phase 1 (http://www.internationalgenome.org), and queried from HaploReg (http://archive.broadinstitute.org/mammals/haploreg/haploreg.php)
                           To perform similar queries in R please check out the haploR package!
                           For TAD visualization check out the Yue Lab (http://promoter.bx.psu.edu/hi-c/).
                           TAD locations are based off of those defined by Dixon et al in 'Topological domains in mammalian genomes identified by analysis of chromatin interactions'."))
            ),
            box(
              title = "Development Team",
              h5(helpText("Programming: Jordan Creed, Travis Gerke")),
              h5(helpText("Scientific Input: Alvaro Monteiro")),
              h5(helpText("Website: http://travisgerke.com"))
            ),
            box(
              title = "Other resources",
              h5(helpText("Aiden Lab")),
              a("Juicebox", href = "http://www.aidenlab.org/juicebox/", target = "_blank")
            )
          ),
          fluidRow(
            box(
              title = "Notes",
              h5(helpText("If no SNPs are in LD above the specified threshold then a range of 53500 BP is applied to either side of the SNP.
                           If SNPs in LD exist, then the range is based on the minimum and maximum BP of all SNPs in LD and the TAD region.
                           This range is used for querying data from Oncotator, ENSEMBL, ClinVar and the Genome Browser."))
            )
          )
        )
      )
    )
  )
}
