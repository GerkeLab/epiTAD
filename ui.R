# UI Options --------------------------------------------------------------

# Set default color and type for shinycssloaders
options(spinner.type = 7, spinner.color = "#357CA5")

example_url <- function(x) {
  has_tooltip <- !is.null(x$tooltip)

  if (has_tooltip) {
    if (!is.null(x$input_id)) {
      tags$li(actionLink(x$input_id, x$text), class = "dropdown-item",
              "data-toggle" = "tooltip", "data-placement" = "right",
              title = x$tooltip)
    } else {
      tags$li(tags$a(href = x$url, x$text), class = "dropdown-item",
              "data-toggle" = "tooltip", "data-placement" = "right",
              title = x$tooltip)
    }
  } else {
    if (!is.null(x$input_id)) {
      tags$li(actionLink(x$input_id, x$text), class = "dropdown-item")
    } else {
      tags$li(tags$a(href = x$url, x$text), class = "dropdown-item")
    }
  }
}

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
    # "Score" = "score"
  )
)


# Shiny UI ----------------------------------------------------------------
# The Shiny UI is wrapped in a function with argument `request` to enable bookmarking

function(request) {
  dashboardPage(
    skin = "black",
    dashboardHeader(title = "epiTAD"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tags$head(
        tags$style(
          HTML(
            ".box-shadow {
            box-shadow: 0 1px 3px rgba(0,0,0,.25);
            -webkit-box-shadow: 0 1px 3px rgba(0,0,0,.25);"
          )
        ),
        #<link rel="stylesheet" type="text/css" href="mystyles.css" media="screen" />
        tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.gerkelab.min.css", media = "screen"),
        tags$link(rel = "stylesheet", type = "text/css", href = "_all-skins.gerkelab.min.css", media = "screen"),
        tags$link(rel = "stylesheet", type = "text/css", href = "epitad.css", media = "screen")
      ),
      fluidRow(
        box(
          title = "Query SNPs", width = 4,
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

          fluidRow(
            column(6, selectInput("pop", "Population", INPUT_CHOICES$population, selected = "EUR")),
            column(6, sliderInput("value", "LD threshold", min = 0, max = 1, value = 0.8))
          ),
          tags$hr(),
          tags$div(
            class = "btn-toolbar",
            actionButton("update1", "Perform query", class = "btn-primary", style = "color: #FFFFFF"),
            tags$div(
              class = "btn-group dropup",
              tags$button(
                class = "btn btn-default dropdown-toggle",
                role = "button",
                id = "exampleDropdownMenu",
                "data-toggle" = "dropdown",
                "aria-haspopup" = "true",
                "aria-expanded" = "false",
                "Examples", tags$span(class = "caret")
              ),
              tags$ul(
                class = "dropdown-menu box-shadow",
                "aria-labelledby" = "exampleDropdownMenu",
                tags$li("A selection of SNPs that are...", class = "dropdown-header"),
                example_url(EXAMPLES$ancestry),
                example_url(EXAMPLES$`8q24`),
                example_url(EXAMPLES$protective)
              )
            ),
            actionButton("btn_info", "", icon = icon("info"),
                         "data-toggle" = "tooltip", "data-placement" = "right",
                         title = "Learn more about epiTAD"),
            bookmarkButton(class = "pull-right")
          )
        ),
        tabBox(
          title = "", width = 8,
          tabPanel(
            "Figure",
            withSpinner(plotlyOutput("megaPlot", height = "600px")),
            tags$br(),
            tags$div(
              # plot controls panel group ----
              class = "panel-group", id = "plot-controls", role = "tablist", "aria-multiselectable" = "true",
              tags$div(
                class = "panel panel-default",
                tags$div(
                  # plot controls panel heading ----
                  class = "panel-heading", role = "tab", id = "plot-controls-heading",
                  tags$h5(
                    class = "panel-title",
                    tags$a("Plot Options", href = "#plot-controls-body",
                           class = "collapsed", role = "button",
                           "data-toggle" = "collapse", "data-parent" = "#plot-controls",
                           "aria-expanded" = "false", "aria-controls" = "plot-controls-body")
                  )
                  # plot controls panel heading end ----
                ),
                tags$div(
                  # plot controls panel body wrapper ----
                  id = "plot-controls-body", role = "tabpanel", "aria-labelledby" = "plot-controls-heading",
                  class = "panel-collapse collapse",
                  tags$div(
                    # plot controls panel body ----
                    class = "panel-body",
                    helpText("Coordinates must be at least 200000 BP apart"),
                    fluidRow(
                      # plot controls input row ----
                      column(4, numericInput("plotStartBP", label = "Starting Coordinates (BP)", value = 0)),
                      column(4, numericInput("plotEndBP", label = "Ending Coordinates (BP)", value = 0)),
                      column(4,
                             selectizeInput("plotColor", "Color Scheme",
                                            choices = list(
                                              "Viridis" = list(
                                                "Viridis",
                                                "Magma",
                                                "Plasma",
                                                "Inferno",
                                                "Cividis"
                                              ),
                                              "Viridis Reversed" = list(
                                                "Viridis (Reverse)" = "viridis rev",
                                                "Magma (Reverse)" = "magma rev",
                                                "Plasma (Reverse)" = "plasma rev",
                                                "Inferno (Reverse)" = "inferno rev",
                                                "Cividis (Reverse)" = "cividis rev"
                                              ),
                                              "Other Palettes" = list(
                                                "Topo",
                                                "Rainbow",
                                                "Heat",
                                                "Terrain",
                                                "CM"
                                              )
                                            ), multiple = FALSE, selected = "Viridis"
                             )
                      )
                      # plot controls input row end ----
                    ),
                    tags$div(
                      # plot-controls button group ----
                      class = "btn-group",
                      actionButton("updateBP", "Update Coordinates"),
                      actionButton("resetBP", "Reset Plot"),
                      actionButton("showgenes","Show gene names"),
                      downloadButton("plotDownload", "Download Plot")
                      # plot controls button group end ----
                    )
                    # plot controls panel body end ----
                  )
                  # plot controls panel body wrapper end ----
                )
              )
              # plot controls panel group end ----
            )
          ),
          tabPanel(
            "Links",
            uiOutput("clinical1"),
            uiOutput("ucsc1")
          ),
          tabPanel(
            "Download",
            tags$p("Use the button below to download all tables in a single excel file"),
            downloadButton("download_all", "Download all")
          )
        )
      ),
      fluidRow(
        tabBox(
          title = "Variant Annotation",
          tabPanel(
            "HaploReg",
            tags$div(
              tags$div(
                # haploreg controls panel group ----
                class = "panel-group", id = "haploreg-controls", role = "tablist", "aria-multiselectable" = "true",
                tags$div(
                  class = "panel panel-default",
                  tags$div(
                    # haploreg controls panel heading ----
                    class = "panel-heading", role = "tab", id = "haploreg-controls-heading",
                    tags$h5(
                      class = "panel-title",
                      tags$a("Additional annotations", href = "#haploreg-controls-body",
                             class = "collapsed", role = "button",
                             "data-toggle" = "collapse", "data-parent" = "#haploreg-controls",
                             "aria-expanded" = "false", "aria-controls" = "haploreg-controls-body")
                    )
                    # haploreg controls panel heading end ----
                  ),
                  tags$div(
                    # haploreg controls panel body wrapper ----
                    id = "haploreg-controls-body", role = "tabpanel", "aria-labelledby" = "haploreg-controls-heading",
                    class = "panel-collapse collapse",
                    tags$div(
                      # haploreg controls panel body ----
                      class = "panel-body",
                      fluidRow(
                        # haploreg controls input row ----
                        column(12, checkboxGroupInput("parameters", "", INPUT_CHOICES$HaploR, inline = TRUE, selected = c("query_snp_rsid", "pos_hg38", "r2")))
                        # haploreg controls input row end ----
                      )
                      # haploreg controls panel body end ----
                    )
                    # haploreg controls panel body wrapper end ----
                  )
                )
                # haploreg controls panel group end ----
              )
            ),
            tags$hr(),
            withSpinner(DT::dataTableOutput("LDtable1"), proxy.height = "100px")
          ),
          tabPanel(
            "RegulomeDB",
            tags$div(
              tags$div(
                # regulomedb controls panel group ----
                class = "panel-group", id = "regulomedb-controls", role = "tablist", "aria-multiselectable" = "true",
                tags$div(
                  class = "panel panel-default",
                  tags$div(
                    # regulomedb controls panel heading ----
                    class = "panel-heading", role = "tab", id = "regulomedb-controls-heading",
                    tags$h5(
                      class = "panel-title",
                      tags$a("Remove annotations", href = "#regulomedb-controls-body",
                             class = "collapsed", role = "button",
                             "data-toggle" = "collapse", "data-parent" = "#regulomedb-controls",
                             "aria-expanded" = "false", "aria-controls" = "regulomedb-controls-body")
                    )
                    # regulomedb controls panel heading end ----
                  ),
                  tags$div(
                    # regulome controls panel body wrapper ----
                    id = "regulomedb-controls-body", role = "tabpanel", "aria-labelledby" = "regulomedb-controls-heading",
                    class = "panel-collapse collapse",
                    tags$div(
                      # regulome controls panel body ----
                      class = "panel-body",
                      fluidRow(
                        # regulome controls input row ----
                        column(12, checkboxGroupInput("parameters2", "", INPUT_CHOICES$regulome, selected = c("#chromosome", "coordinate","score")))
                        # regulomedb controls input row end ----
                      )
                      # regulomedb controls panel body end ----
                    )
                    # regulomedb controls panel body wrapper end ----
                  )
                )
                # regulomedb controls panel group end ----
              )
            ),
            tags$hr(),
            withSpinner(DT::dataTableOutput("LDtable2"), proxy.height = "100px")
          ),
          tabPanel(
            "TADs",
            withSpinner(textOutput("tadBoundaries"), proxy.height = "100px"),
            uiOutput("hic1")
          )
        ),
        tabBox(
          title = "Gene Annotation",
          tabPanel(
            "ENSEMBL",
            h5(helpText("Genes spanned by the greater of the LD or TAD region")),
            withSpinner(DT::dataTableOutput("geneTable"), proxy.height = "100px")
          ),
          tabPanel(
            "Oncotator",
            tags$div(
              tags$div(
                # oncotator controls panel group ----
                class = "panel-group", id = "oncotator-controls", role = "tablist", "aria-multiselectable" = "true",
                tags$div(
                  class = "panel panel-default",
                  tags$div(
                    # oncotator controls panel heading ----
                    class = "panel-heading", role = "tab", id = "oncotator-controls-heading",
                    tags$h5(
                      class = "panel-title",
                      tags$a("Additional annotations", href = "#oncotator-controls-body",
                             class = "collapsed", role = "button",
                             "data-toggle" = "collapse", "data-parent" = "#oncotator-controls",
                             "aria-expanded" = "false", "aria-controls" = "oncotator-controls-body")
                    )
                    # oncotator controls panel heading end ----
                  ),
                  tags$div(
                    # oncotator controls panel body wrapper ----
                    id = "oncotator-controls-body", role = "tabpanel", "aria-labelledby" = "oncotator-controls-heading",
                    class = "panel-collapse collapse",
                    tags$div(
                      # oncotator controls panel body ----
                      class = "panel-body",
                      checkboxGroupInput("oncoParameters1", "General", INPUT_CHOICES$onco$general, inline = TRUE),
                      checkboxGroupInput("oncoParameters2", "Cancer Gene Census", INPUT_CHOICES$onco$cancer_gene_census, inline = TRUE),
                      checkboxGroupInput("oncoParameters3", "HUGO Gene Nomenclature Committee", INPUT_CHOICES$onco$hugo_gene_nomenclature, inline = TRUE),
                      checkboxGroupInput("oncoParameters4", "UniProt", INPUT_CHOICES$onco$uniprot, inline = TRUE)
                      # oncotator controls panel body end ----
                    )
                    # oncotator controls panel body wrapper end ----
                  )
                )
                # oncotator controls panel group end ----
              )
            ),
            tags$hr(),
            withSpinner(DT::dataTableOutput("oncoTable"), proxy.height = "100px")
          ),
          tabPanel(
            "eQTL",
            tags$div(
              tags$div(
                # eqtl controls panel group ----
                class = "panel-group", id = "eqtl-controls", role = "tablist", "aria-multiselectable" = "true",
                tags$div(
                  class = "panel panel-default",
                  tags$div(
                    # eqtl controls panel heading ----
                    class = "panel-heading", role = "tab", id = "eqtl-controls-heading",
                    tags$h5(
                      class = "panel-title",
                      tags$a("Tissues", href = "#eqtl-controls-body",
                             class = "collapsed", role = "button",
                             "data-toggle" = "collapse", "data-parent" = "#eqtl-controls",
                             "aria-expanded" = "false", "aria-controls" = "eqtl-controls-body")
                    )
                    # eqtl controls panel heading end ----
                  ),
                  tags$div(
                    # eqtl controls panel body wrapper ----
                    id = "eqtl-controls-body", role = "tabpanel", "aria-labelledby" = "eqtl-controls-heading",
                    class = "panel-collapse collapse",
                    tags$div(
                      # eqtl controls panel body ----
                      class = "panel-body",
                      fluidRow(
                        # eqtl controls input row ----
                        column(12,uiOutput("eTissues"))
                        # eqtl controls input row end ----
                      )
                      # eqtl controls panel body end ----
                    )
                    # eqtl controls panel body wrapper end ----
                  )
                )
                # eqtl controls panel group end ----
              )
            ),
            tags$hr(),
            tags$div(
              style = "min-height: 1.5em",
              DT::dataTableOutput("eTable1")
            ),
            uiOutput("eqtl1")
          )
        )
      )
    )
  )
}
