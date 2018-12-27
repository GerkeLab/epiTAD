
# UI Options --------------------------------------------------------------

# Set default color and type for shinycssloaders::w
options(spinner.type = 6, spinner.color = "#357CA5")

INPUT_CHOICES <- list(
  population = c("EUR", "AFR", "AMR", "ASN"),
  HaploR = c(
    "Chromosome" = "chr",
    "Position" = "pos_hg38",
    "r2" = "r2",
    "Query SNP" = "query_snp_rsid",
    "D'" = "D'",
    "Query SNP" = "is_query_snp",
    "Reference allele" = "ref",
    "Alternative allele" = "alt",
    "LD(AFR)" = "AFR",
    "LD(AMR)" = "AMR",
    "LD(ASN)" = "ASN",
    "LD(EUR)" = "EUR",
    "GERP scores" = "GERP_cons",
    "SiPhy scores" = "SiPhy_cons",
    "Chromatin States" = "Chromatin_States",
    "Imputed Chromatin States" = "Chromatin_States_Imputed",
    "Chromatin Marks" = "Chromatin_Marks",
    "DNAse" = "DNAse",
    "Proteins",
    "eQTL",
    "GWAS study name" = "gwas",
    "GRASP study name" = "grasp",
    "Motifs",
    "GENCODE transcript ID" = "GENCODE_id",
    "GENCODE gene name" = "GENCODE_name",
    "GENCODE direction" = "GENCODE_direction",
    "GENCODE distance" = "GENCODE_distance",
    "NCBI Reference Sequence Accession number" = "RefSeq_id",
    "NCBI Reference Sequence name" = "RefSeq_name",
    "NCBI Reference Sequence direction" = "RefSeq_direction",
    "NCBI Reference Sequence distance" = "RefSeq_distance",
    "Annotated proteins" = "dbSNP_functional_annotation"
  ),
  onco = list(
    general = c(
      "Lineages in ACHILLES dataset" = "ACHILLES_Lineage_Results_Top_Genes",
      "Total number of mutations in CCLE Oncomap data" = "CCLE_By_Gene_total_mutations_in_gene",
      "Gene symbols of fusion events (COSMIC)" = "COSMIC_FusionGenes_fusion_genes",
      "Tissue type summary of tumor samples (COSMIC)" = "COSMIC_Tissue_tissue_types_affected",
      "Total numbers of records for gene (COSMIC)" = "COSMIC_Tissue_total_alterations_in_gene",
      "Familial cancer database reference used" = "Familial_Cancer_Genes_Reference",
      "Familial cancer syndromes" = "Familial_Cancer_Genes_Syndrome",
      "Familial cancer syndrome synonyms" = "Familial_Cancer_Genes_Synonym",
      "HGNC Accession" = "HGNC_Accession.Numbers",
      "Known DNA repair roles" = "HumanDNARepairGenes_Role",
      "Published MutSig analyses" = "MutSig.Published.Results_Published_Results",
      "Overlapping significant GISTIC amplification focal peaks from TCGAscape" = "TCGAScape_Amplification_Peaks",
      "Overlapping significant GISTIC deletion focal peaks from TCGAscape" = "TCGAScape_Deletion_Peaks",
      "Overlapping significant GISTIC aplification focal peaks from Tumorscape" = "TUMORScape_Amplification_Peaks",
      "Overlapping significant GISTIC deletion focal peaks from Tumorscape" = "TUMORScape_Deletion_Peaks",
      "Observed alternate allele" = "alt_allele",
      "Build" = "build",
      "Chromosome" = "chr",
      "Class" = "class",
      "End coordinate" = "end",
      "Gene" = "gene",
      "Protein change" = "protein_change",
      "Positive strand reference allele" = "ref_allele",
      "Start coordinate" = "start",
      "Strand orientation" =  "strand",
      "Transcripts" = "transcripts"
    ),
    cancer_gene_census = c(
      "Variant is in a gene that is mutated in the germline predisposing to cancer" = "CGC_Cancer.Germline.Mut",
      "Variants dominant/recesive" = "CGC_Cancer.Molecular.Genetics",
      "Variant is in a gene that is somatically mutated in cancer" = "CGC_Cancer.Somatic.Mut",
      "Cancer related syndromes" = "CGC_Cancer.Syndrome",
      "Chromosome" = "CGC_Chr",
      "Chromosome band" = "CGC_Chr.Band",
      "Entrez gene ID" = "CGC_GeneID",
      "Type of mutations" = "CGC_Mutation.Type",
      "Full gene name" = "CGC_Name",
      "Variant is in a gene that is germline mutated in other diseases/syndromes" = "CGC_Other.Germline.Mut",
      "Other diseases/syndromes" = "CGC_Other.Syndrome.Disease",
      "Tissue types with mutations" = "CGC_Tissue.Type",
      "Known translocation partner gene" = "CGC_Translocation.Partner",
      "Tumor types with somatic alterations" = "CGC_Tumour.Types...Somatic.Mutations.",
      "Tumor types with germline alterations" = "CGC_Tumour.Types..Germline.Mutations."
    ),
    uniprot = c(
      "Overlapping UniProt sites with experimental data" = "UniProt_AA_experimental_info",
      "Overlapping UniProt variants of interest" = "UniProt_AA_natural_variation",
      "Overlapping UniProt regions of interest " = "UniProt_AA_region",
      "Overlapping UniProt single amino acid sites" = "UniProt_AA_site",
      "Listing of compounds from DrugBank known to interact with genes" = "UniProt_DrugBank",
      "GO terms describing pathways and processes UniProt protein is involved in" = "UniProt_GO_Biological_Process",
      "GO terms describing localization of given UniProt protein" = "UniProt_GO_Cellular_Component",
      "GO terms describing molecular activity of given UniProt protein" = "UniProt_GO_Molecular_Function",
      "Alternative UniProt accession ID" = "UniProt_alt_uniprot_accessions",
      "Uniprot accession number" = "UniProt_uniprot_accession",
      "UniProt entry name" = "UniProt_uniprot_entry_name"
    )
  ),
  regulome = c(
    "Chromosome" = "#chromosome",
    "Coordinates" = "coordinate",
    "Hits" = "hits",
    "Score" = "score_anno"
  )
)


# Shiny UI ----------------------------------------------------------------
# The Shiny UI is wrapped in a function with argument `request` to enable bookmarking

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
              textInput("snpList", "Enter SNP rsIDs (comma separated)", value = "", placeholder = "rs123, rs5574"),
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
                selectInput("pop", "Population", INPUT_CHOICES$population, selected = "EUR"),
                sliderInput("value", "LD threshold", min = 0, max = 1, value = 0.8)
              ),
              tabPanel(
                "HaploReg",
                checkboxGroupInput("parameters", "HaploR", INPUT_CHOICES$HaploR, inline = TRUE, selected = c("query_snp_rsid", "pos_hg38", "r2"))
              ),
              tabPanel(
                "RegulomeDB",
                checkboxGroupInput("parameters2", "Regulome", INPUT_CHOICES$regulome, selected = c("#chromosome", "coordinate", "hits", "score_anno"))
              ),
              tabPanel(
                "eQTL",
                uiOutput("eTissues")
              ),
              tabPanel(
                "Oncotator",
                checkboxGroupInput("oncoParameters1", "General", INPUT_CHOICES$onco$general, inline = TRUE),
                checkboxGroupInput("oncoParameters2", "Cancer Gene Census", INPUT_CHOICES$onco$cancer_gene_census, inline = TRUE),
                checkboxGroupInput("oncoParameters3", "HUGO Gene Nomenclature Committee", INPUT_CHOICES$onco$hugo_gene_nomenclature, inline = TRUE),
                checkboxGroupInput("oncoParameters4", "UniProt", INPUT_CHOICES$onco$uniprot, inline = TRUE)
              )
            )
          ),
          fluidRow(
            tabBox(
              title = "Variant Annotation",
              tabPanel(
                "HaploReg",
                withSpinner(DT::dataTableOutput("LDtable1"))
              ),
              tabPanel(
                "RegulomeDB",
                withSpinner(DT::dataTableOutput("LDtable2"))
              ),
              tabPanel(
                "TADs",
                withSpinner(textOutput("tadBoundaries")),
                uiOutput("hic1")
              )
            ),
            tabBox(
              title = "Gene Annotation",
              tabPanel(
                "ENSEMBL",
                h5(helpText("Genes spanned by the greater of the LD or TAD region")),
                withSpinner(DT::dataTableOutput("geneTable"))
              ),
              tabPanel(
                "Oncotator",
                withSpinner(DT::dataTableOutput("oncoTable"))
              ),
              tabPanel(
                "eQTL",
                tags$div(
                  style = "min-height: 1.5em",
                  DT::dataTableOutput("eTable1")
                ),
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
                  withSpinner(plotOutput("megaPlot", height = "450px")),
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
