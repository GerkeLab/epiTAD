
# epiTAD ------------------------------------------------------------------
#
# epiTAD Analysis Script
# Generated on {{timestamp}}
#
# Visit https://gerkelab.com/project/epiTAD for more information
#
# -------------------------------------------------------------------------


# Load Required Packages --------------------------------------------------
# Uses the pacman library to load or install packages as needed
if (!suppressPackageStartupMessages(require(pacman))) install.packages("pacman")
pacman::p_load(data.table, jsonlite, colorspace,
               haploR, biomaRt, Sushi, HiTC)


# Parameters --------------------------------------------------------------

# snpList... (describe required structure)
snpList <- {{{snpList}}}
# value...
value <- {{{value}}}
# pop...
pop <- {{{pop}}}
# tissue...
tissue <- {{{tissue}}}


## Onco Table Parameters
## TODO These need more descriptive names
oncoParameters1 <- {{{oncoParameters1}}}
oncoParameters2 <- {{{oncoParameters2}}}
oncoParameters3 <- {{{oncoParameters3}}}
oncoParameters4 <- {{{oncoParameters4}}}

## Other Table Parameters
## TODO: These need more descriptive names
parameters <- {{{parameters}}}
parameters2 <- {{{parameters2}}}

## Plot Parameters
possible_colors <- c("topo", "rainbow", "heat", "terrain", "cm",
                     "viridis", "viridis rev", "magma", "magma rev",
                     "plasma", "plasma rev", "inferno", "inferno rev",
                     "cividis", "cividis rev")

plotColor <- {{{plotColor}}}
# was values$tmp_min
plotStartBP <- {{{plotStartBP}}}
# was values$tmp_max
plotEndBP <- {{{plotEndBP}}}


# Download Needed Data ----------------------------------------------------

ensembl54 <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

hic_file <- "hicData.Rdata"
if (!file.exists(hic_file)) {
  download.file("https://github.com/GerkeLab/epiTAD/raw/master/data/hicData.Rdata", hic_file)
}

tad <- fread("http://compbio.med.harvard.edu/modencode/webpage/hic/IMR90_domains_hg19.bed")
colnames(tad) <- c("chr", "start_position", "end_position")
tad$chr <- gsub("chr", "", tad$chr)
tad$chr <- as.numeric(tad$chr)
tad <- tad[!is.na(tad$chr), ]

lad <- fread("http://compbio.med.harvard.edu/modencode/webpage/lad/human.fibroblast.DamID.hg19.bed")
colnames(lad) <- c("chr", "start", "end", "dunno")
lad$chr <- gsub("chr", "", lad$chr)
lad$chr <- as.numeric(lad$chr)
lad <- lad[!is.na(lad$chr), ]


# Prepare SNPs ------------------------------------------------------------

# reactive: dat
snps <- as.character(unlist(strsplit(snpList, ",")))
snps <- trimws(snps)
dat <- queryHaploreg(query = snps, ldThresh = value, ldPop = pop)
dat$chr <- as.numeric(as.character(dat$chr))
dat$pos_hg38 <- as.numeric(as.character(dat$pos_hg38))


# Choose Tissues ----------------------------------------------------------

etest <- unlist(strsplit(as.character(dat$eQTL), ";"))
etest <- etest[!etest %in% c(".")]
etest2 <- unlist(strsplit(etest, ","))
if (length(etest2)) {
  etest3 <- matrix(etest2, nrow = length(etest), ncol = 4, byrow = TRUE)
  etest3 <- as.data.frame(etest3)
  etest3 <- etest3[!duplicated(etest3$V2), ]
  tissues_avail <- etest3$V2
  tissue_missing <- setdiff(tissue, tissues_avail)
  if (length(tissue_missing)) {
    warning("Ignoring eQTLs ", paste(tissue_missing, collapse = ", "),
            " as these were not associated with the requested SNPs")
  }
  tissue <- intersect(tissue, tissues_avail)
} else {
  message("No statistically significant eQTLs were reported with these SNPs.")
}


# Dat2 --------------------------------------------------------------------
# TODO: dat2 and associated function need more descriptive names
dat2_function <- function(snpList) {
  snps <- as.character(unlist(strsplit(snpList, ",")))
  snps <- trimws(snps)
  x <- queryRegulome(query = snps)
  if (nrow(x$res.table) < 1) {
    stop("The queried SNP may not be valid. Please check your input.")
  }
  x <- as.data.frame(x$res.table)
  x$score <- as.character(x$score)
  x$score_anno <- NA
  for (i in 1:nrow(x)) {
    if (x$score[i] == "1a") {
      x$score_anno[i] <- "eQTL + TF binding + matched TF motif + matched DNase Footprint + DNase peak"
    }
    else if (x$score[i] == "1b") {
      x$score_anno[i] <- "eQTL + TF binding + any motif + DNase Footprint + DNase peak"
    }
    else if (x$score[i] == "1c") {
      x$score_anno[i] <- "eQTL + TF binding + matched TF motif + DNase peak"
    }
    else if (x$score[i] == "1d") {
      x$score_anno[i] <- "eQTL + TF binding + any motif + DNase peak"
    }
    else if (x$score[i] == "1e") {
      x$score_anno[i] <- "eQTL + TF binding + matched TF motif"
    }
    else if (x$score[i] == "1f") {
      x$score_anno[i] <- "eQTL + TF binding / DNase peak"
    }
    else if (x$score[i] == "2a") {
      x$score_anno[i] <- "TF binding + matched TF motif + matched DNase Footprint + DNase peak"
    }
    else if (x$score[i] == "2b") {
      x$score_anno[i] <- "TF binding + any motif + DNase Footprint + DNase peak"
    }
    else if (x$score[i] == "2c") {
      x$score_anno[i] <- "TF binding + matched TF motif + DNase peak"
    }
    else if (x$score[i] == "3a") {
      x$score_anno[i] <- "TF binding + any motif + DNase peak"
    }
    else if (x$score[i] == "3b") {
      x$score_anno[i] <- "TF binding + matched TF motif"
    }
    else if (x$score[i] == "4") {
      x$score_anno[i] <- "TF binding + DNase peak"
    }
    else if (x$score[i] == "5") {
      x$score_anno[i] <- "TF binding or DNase peak"
    }
    else {
      x$score_anno[i] <- "Other"
    }
  }
  x
}

dat2 <- dat2_function(snpList)
dat2


# In TAD or LAD -----------------------------------------------------------

in_tad <- function(dat, snps, tad) {
  dat <- dat[dat$rsID %in% snps, ]
  snp_pos <- dat$pos_hg38
  tad <- tad[tad$chr == max(dat$chr, na.rm = TRUE), ]
  tad[tad$start_position <= snp_pos & tad$end_position >= snp_pos, ]
}

in_lad <- function(dat, sps, lad) {
  dat <- dat[dat$rsID %in% snps, ]
  snp_pos <- dat$pos_hg38
  lad[lad$chr == max(dat$chr, na.rm = TRUE), ]
}

# TAD boundaries
tad_boundaries <- in_tad(dat, snps, tad)
if (nrow(tad_boundaries) < 1) {
  message("Not in a TAD!")
} else {
  message("In a TAD! The TAD ranges from ", tad_boundaries$start_position, " to ", tad_boundaries$end_position)
}


# eQTL table --------------------------------------------------------------
# TODO better descriptions
# output$eTable1 <-
eqtl_table <- function(dat, tissue) {
  etest <- unlist(strsplit(as.character(dat$eQTL), ";"))
  etest <- etest[!etest %in% c(".")]
  etest2 <- unlist(strsplit(etest, ","))

  # Check inputs and that there are eQTLs for these SNPs
  if (!length(etest2)) {
    warning("No statistically significant eQTLs were reported with these SNPs.")
    return(NULL)
  }

  # Return table
  etest3 <- matrix(etest2, nrow = length(etest), ncol = 4, byrow = TRUE)
  etest3 <- as.data.frame(etest3)
  colnames(etest3) <- c("Source", "Tissue", "Gene", "p")
  etest3[etest3$Tissue %in% tissue, ]
}

eqtl_table(dat, tissue)



# Total Min and Max -------------------------------------------------------

total_min <- function(dat, snps, tad) {
  tad <- in_tad(dat, snps, tad)
  if (nrow(tad) >= 1) {
    total_min <- min(c(min(dat$pos_hg38, na.rm = TRUE), tad$start_position))
    return(as.numeric(total_min))
  }
  else if (nrow(tad) < 1 & nrow(dat) > 1) {
    total_min <- min(dat$pos_hg38, na.rm = TRUE)
    return(as.numeric(total_min))
  }
  else {
    total_min <- min(dat$pos_hg38, na.rm = TRUE) - 53500
    return(as.numeric(total_min))
  }
}

total_max <- function(dat, snps, tad) {
  tad <- in_tad(dat, snps, tad)
  if (nrow(tad) >= 1) {
    total_max <- max(c(max(dat$pos_hg38, na.rm = TRUE), tad$end_position))
    return(as.numeric(total_max))
  }
  else if (nrow(tad) < 1 & nrow(dat) > 1) {
    total_max <- max(dat$pos_hg38, na.rm = TRUE)
    return(as.numeric(total_max))
  }
  else {
    total_max <- max(dat$pos_hg38, na.rm = TRUE) + 53500
    return(as.numeric(total_max))
  }
}


# Tables ------------------------------------------------------------------

#### LD Table 1 ####
dat[, c("rsID", parameters)]


#### LD Table 2 #####
dat2[, c("rsid", parameters2)]

#### Gene Table ####
chr <- max(dat$chr, na.rm = TRUE)
getBM(
  attributes = c("hgnc_symbol", "start_position", "end_position"),
  filters = c("chromosomal_region"),
  values = paste0(chr, ":", total_min(dat, snps, tad), ":", total_max(dat, snps, tad)),
  mart = ensembl54
)

#### oncoTable

chr <- max(as.numeric(dat$chr), na.rm = TRUE)
oncotable_res <- fromJSON(paste0(
  "http://portals.broadinstitute.org/oncotator/genes/", chr, "_",
  total_min(dat, snps, tad), "_", total_max(dat, snps, tad), "/"
))
if (!length(oncotable_res)) {
  warning("Oncotator did not return valid results")
} else {
  oncotable <- as.data.frame(oncotable_res[[1]])
  for (i in seq_along(oncotable_res)[-1]) {
    oncotable_dat <- as.data.frame(oncotable_res[[i]])
    oncotable <- rbind(oncotable, oncotable_dat)
  }

  oncotable_cols <- Reduce(union, c("gene", oncoParameters1, oncoParameters2, oncoParameters3, oncoParameters4))
  oncotable_cols <- oncotable_cols[!oncotable_cols == ""]
  oncotable <- oncotable[, oncotable_cols, drop = FALSE]
}

oncotable


# Plot --------------------------------------------------------------------

pick_plot_color <- function(plotColor) {
  switch(
    tolower(plotColor),
    "topo" = topo.colors,
    "rainbow" = rainbow,
    "heat" = heat.colors,
    "terrain" = terrain.colors,
    "cm" = cm.colors,
    "viridis" = viridisLite::viridis,
    "viridis rev" = function(n, ...) viridisLite::viridis(n, direction = -1, ...),
    "magma" = viridisLite::magma,
    "magma rev" = function(n, ...) viridisLite::magma(n, direction = -1, ...),
    "plasma" = viridisLite::plasma,
    "plasma rev" = function(n, ...) viridisLite::plasma(n, direction = -1, ...),
    "inferno" = viridisLite::inferno,
    "inferno rev" = function(n, ...) viridisLite::inferno(n, direction = -1, ...),
    "cividis" = viridisLite::cividis,
    "cividis rev" = function(n, ...) viridisLite::cividis(n, direction = -1, ...)
  )
}


megaplot <- function(ld, plot_color = pick_plot_color("topo"),
                     hic_file = "hicData.Rdata",
                     minBP = total_min(dat, snps, tad),
                     maxBP = total_max(dat, snps, tad)) {
  if (!exists("hiC")) load(hic_file)

  chrX <- max(ld$chr, na.rm = TRUE)

  hic_dat <- extractRegion(hiC[[paste0("chr", chrX, "chr", chrX)]],
                           chr = paste0("chr", chrX),
                           from = minBP, to = maxBP
  )
  hic_matrix <- as.matrix(intdata(hic_dat))

  genes <- getBM(
    attributes = c("hgnc_symbol", "start_position", "end_position"),
    filters = c("chromosomal_region"), values = paste0(chrX, ":", minBP, ":", maxBP), mart = ensembl54
  )
  colnames(genes) <- c("Symbol", "Start", "End")

  tads <- as.data.frame(tads_imr90)


  # Create Plot -----------------------------------------------------------

  mat_layout <- matrix(c(1, 2, 3, 4, 1, 2, 3, 4), nrow = 4, ncol = 2)
  layout(mat_layout, c(4, 4, 4, 4), c(2.25, 1.25, 0.5, 0.5))
  par(mar = c(0.5, 4.5, 0.5, 0.5))

  phic <- plotHic(hic_matrix,
                  chrom = paste0("chr", chrX),
                  chromstart = min(as.numeric(colnames(hic_matrix))),
                  chromend = max(as.numeric(colnames(hic_matrix))),
                  max_y = 20, zrange = c(0, 28),
                  palette = plot_color,
                  flip = FALSE
  )
  labelgenome(
    chrom = paste0("chr", chrX), chromstart = minBP, chromend = maxBP,
    side = 1, scipen = 40, n = 1, scale = "bp"
  )
  addlegend(phic[[1]],
            palette = phic[[2]], title = "score", side = "right", bottominset = 0.4,
            topinset = 0, xoffset = -.035, labelside = "left", width = 0.025, title.offset = 0.035
  )
  mtext("HIC Intensities", side = 2, line = 1.75, cex = .75, font = 2)

  plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
  segments(x0 = genes$Start, y0 = 0.5, x1 = genes$End, y1 = 0.5, lwd = 30, col = plot_color(n = nrow(genes), alpha = 0.7), lend = 1)
  text(x = (genes$Start + genes$End) / 2, y = c(0.7, 0.3, 0.8, 0.2), labels = genes$Symbol, col = plot_color(n = nrow(genes), alpha = 0.7))
  mtext("Genes", side = 2, line = 1.75, cex = .75, font = 2)

  plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
  abline(v = ld[ld$is_query_snp == 0, ]$pos_hg38, col = "grey", lend = 1) # lwd=6
  abline(v = ld[ld$is_query_snp == 1, ]$pos_hg38, col = plot_color(n = nrow(genes), alpha = 0.7), lend = 1)
  mtext("LD", side = 2, line = 1.75, cex = .75, font = 2)

  plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
  segments(
    x0 = tads[tads$seqnames == paste0("chr", chrX), ]$start,
    y0 = 0.5,
    x1 = tads[tads$seqnames == paste0("chr", chrX), ]$end,
    y1 = 0.5, lwd = 30,
    col = plot_color(n = nrow(genes), alpha = 0.7),
    lend = 1
  )
  mtext("TADs", side = 2, line = 1.75, cex = .75, font = 2)
}

megaplot(dat, pick_plot_color(plotColor))
