SNP_QUERY_ERROR <- "The queried SNP may not be valid. Please check your input."

epitad_datatable <- function(
  x,
  ...,
  filter = "top",
  rownames = FALSE,
  style = "bootstrap",
  autoHideNavigation = TRUE,
  selection = "none",
  extensions = "Buttons",
  options = list(
    dom = "t<'row'<'col-sm-4'B><'col-sm-8'p>>",
    buttons = c('copy', 'csv', 'print'),
    pageLength = 10
  )
) {
  # Worst case scenario, return nothing (just in case)
  if (!inherits(x, "data.frame") || !nrow(x)) return(NULL)

  DT::datatable(x, ...,
                filter = filter,
                rownames = rownames,
                style = style,
                autoHideNavigation = TRUE,
                selection = selection,
                extensions = extensions,
                options = options
  )
}

function(input, output, session) {
  # Enable bookmarking button and update URL on bookmark
  setBookmarkExclude("file1")
  onBookmarked(function(url) {
    showModal(urlModal(url, subtitle = "This link stores the current state of epiTAD."))
    updateQueryString(url)
  })

  DT:::DT2BSClass(c("stipe", "hover", "compact", "cell-border"))

  sample <- eventReactive(input$update1, {
    samplefile <- input$file1
    if (is.null(samplefile)) {
      return()
    }
    sample1 <- read.table(file = samplefile$datapath, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
  })

  safely <- function(.f, error_msg = NULL, quiet = FALSE) {
    function(...) {
      tryCatch(
        list(result = .f(...), error = NULL),
        error = function(e) {
          if (!quiet)
            message("Error: ", e$message)

          list(result = NULL, error = if (is.null(error_msg)) e$message else error_msg)
        },
        interrupt = function(e) {
          stop("Terminated by user", call. = FALSE)
        }
      )
    }
  }

  safe_queryHaploreg <- safely(queryHaploreg)

  dat <- eventReactive(input$update1, {
    if (input$snpList == "") {
      dat <- sample()
      snps <- dat[, 1]
      x <- safe_queryHaploreg(query = snps, ldThresh = as.numeric(input$value), ldPop = input$pop)
    } else {
      snps <- as.character(unlist(strsplit(input$snpList, ",")))
      snps <- trimws(snps)
      x <- safe_queryHaploreg(query = snps, ldThresh = input$value, ldPop = input$pop)
      if (is.null(x$error)) {
        x$chr <- as.numeric(as.character(x$chr))
        x$pos_hg38 <- as.numeric(as.character(x$pos_hg38))
      }
    }
    shiny::validate(need(is.null(x$error), SNP_QUERY_ERROR))
    if (is.null(x$error)) x$result
  })

  output$eTissues <- renderUI({
    dat <- dat()
    etest <- unlist(strsplit(as.character(dat$eQTL), ";"))
    etest <- etest[!etest %in% c(".")]
    etest2 <- unlist(strsplit(etest, ","))
    shiny::validate(need(etest2, "No statistically significant eQTLs were reported with these SNPs."))
    etest3 <- matrix(etest2, nrow = length(etest), ncol = 4, byrow = TRUE)
    etest3 <- as.data.frame(etest3)
    etest3 <- etest3[!duplicated(etest3$V2), ]
    opt <- etest3$V2
    checkboxGroupInput("tissue", "Tissues", choices = opt, selected = opt, inline = TRUE)
  })

  dat2 <- eventReactive(input$update1, {
    if (input$snpList == "") {
      dat <- sample()
      snps <- dat[, 1]
      x <- queryRegulome(query = snps)
      x <- as.data.frame(x$res.table)
      x$score <- as.character(x$score)
      x$score_anno <- NA
      for (i in nrow(x)) {
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
      return(x)
    }
    if (input$snpList != "") {
      snps <- as.character(unlist(strsplit(input$snpList, ",")))
      snps <- trimws(snps)
      x <- queryRegulome(query = snps)
      shiny::validate(need(nrow(x$res.table) > 0, SNP_QUERY_ERROR))
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
      return(x)
    }
  })

  snps <- eventReactive(input$update1, {
    if (input$snpList == "") {
      dat <- sample()
      snps <- dat[, 1]
      return(snps)
    }
    if (input$snpList != "") {
      snps <- as.character(unlist(strsplit(input$snpList, ",")))
      snps <- trimws(snps)
      return(snps)
    }
  })

  in_tad <- eventReactive(input$update1, {
    snps <- snps()
    dat <- dat()
    dat <- dat[dat$rsID %in% snps, ]
    snp_pos <- dat$pos_hg38
    tad <- tad[tad$chr == max(dat$chr, na.rm = TRUE), ]
    in_tad <- tad[tad$start_position <= snp_pos & tad$end_position >= snp_pos, ]
    return(in_tad)
  })

  in_lad <- eventReactive(input$update1, {
    snps <- snps()
    dat <- dat()
    dat <- dat[dat$rsID %in% snps, ]
    snp_pos <- dat$pos_hg38
    lad <- lad[lad$chr == max(dat$chr, na.rm = TRUE), ]
    return(lad)
  })

  output$tadBoundaries <- renderText({
    in_tad <- in_tad()
    if (nrow(in_tad) < 1) {
      return(paste0("Not in a TAD!"))
    }
    else {
      ({
        return(paste0("In a TAD! The TAD ranges from ", in_tad$start_position, " to ", in_tad$end_position))
      })
    }
  })

  output$eTable1 <- DT::renderDataTable({
    dat <- dat()
    etest <- unlist(strsplit(as.character(dat$eQTL), ";"))
    etest <- etest[!etest %in% c(".")]
    etest2 <- unlist(strsplit(etest, ","))

    # Check inputs and that there are eQTLs for these SNPs
    shiny::validate(need(etest2, "No statistically significant eQTLs were reported with these SNPs."))
    shiny::validate(need(input$tissue, "Please select desired Tissues from the eQTL tab in the 'Select Output' box."))

    # Return table
    etest3 <- matrix(etest2, nrow = length(etest), ncol = 4, byrow = TRUE)
    etest3 <- as.data.frame(etest3)
    colnames(etest3) <- c("Source", "Tissue", "Gene", "p")
    epitad_datatable(etest3[etest3$Tissue %in% input$tissue, ])
  }, server = FALSE)

  values <- reactiveValues(tmp_min = 0, tmp_max = 999)

  total_min <- eventReactive(input$update1, {
    tad <- in_tad()
    dat <- dat()
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
  })

  total_max <- eventReactive(input$update1, {
    tad <- in_tad()
    dat <- dat()
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
  })

  observeEvent(input$updateBP, {
    values$tmp_min <- input$plotStartBP
    values$tmp_max <- input$plotEndBP
  })

  observeEvent(input$resetBP, {
    values$tmp_min <- 0
    values$tmp_max <- 999
  })

  output$hic1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    if (length(x) > 1) {
      a("Take me to HIC Browser", href = paste0("http://promoter.bx.psu.edu/hi-c/view.php?species=human&assembly=hg19&source=inside&tissue=GM12878&type=Lieberman-raw&c_url=&transfer=&chr=chr", max(as.numeric(y$chr), na.rm = TRUE), "&start=", total_min, "&end=", total_max, "&sessionID=&browser=none"), target = "_blank")
    } else if (length(x) == 1) {
      a("Take me to HIC Browser", href = paste0("http://promoter.bx.psu.edu/hi-c/view.php?species=human&assembly=hg19&source=inside&tissue=GM12878&type=Lieberman-raw&resolution=25&c_url=&transfer=&gene=", x, "&sessionID=&browser=none"), target = "_blank")
    }
  })

  output$clinical1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    a("Take me to ClinVar", href = paste0("https://www.ncbi.nlm.nih.gov/clinvar/?term=", max(as.numeric(y$chr), na.rm = TRUE), "%5Bchr%5D+AND+", total_min, "%3A", total_max, "%5Bchrpos37%5D"), target = "_blank")
  })

  output$ucsc1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    a("Take me to Genome Browser", href = paste0("https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr", max(as.numeric(y$chr), na.rm = TRUE), "%3A", total_min, "%2D", total_max, "&hgsid=598506407_cis2LZUJLabCsy1N2YPEuJv8vbBZ"), target = "_blank")
  })

  output$eqtl1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    link <- if (length(x) > 1) {
      a("Take me to GTEx", href = paste0("https://www.gtexportal.org/home/browseEqtls?location=chr", max(as.numeric(y$chr), na.rm = TRUE), ":", total_min, "-", total_max), target = "_blank")
    } else if (length(x) == 1) {
      a("Take me to GTEx", href = paste0("https://www.gtexportal.org/home/snp/", x), target = "_blank")
    }
    tagList(tags$p(link))
  })



  output$LDtable1 <- DT::renderDataTable({
    x <- dat()
    epitad_datatable(x[, c("rsID", input$parameters)])
  }, server = FALSE)

  output$LDtable2 <- DT::renderDataTable({
    x <- dat2()
    epitad_datatable(x[, c("rsid", input$parameters2)])
  }, server = FALSE)

  output$geneTable <- DT::renderDataTable({
    ld <- dat()
    chr <- max(ld$chr, na.rm = TRUE)
    total_min <- total_min()
    total_max <- total_max()

    genes <- getBM(
      attributes = c("hgnc_symbol", "start_position", "end_position"),
      filters = c("chromosomal_region"), values = paste0(chr, ":", total_min, ":", total_max), mart = ensembl54
    )
    epitad_datatable(genes)
  }, server = FALSE)

  output$oncoTable <- DT::renderDataTable({
    ld <- dat()
    chr <- max(as.numeric(ld$chr), na.rm = TRUE)
    total_min <- total_min()
    total_max <- total_max()

    x <- fromJSON(paste0("http://portals.broadinstitute.org/oncotator/genes/", chr, "_", total_min, "_", total_max, "/"))
    shiny::validate(need(length(x) > 0, "Oncotator did not return valid results"))

    genes <- as.data.frame(x[[1]])

    for (i in seq_along(x)[-1]) {
      gene_dat <- as.data.frame(x[[i]])
      genes <- rbind(genes, gene_dat)
    }

    genes <- genes[, c("gene", input$oncoParameters1, input$oncoParameters2, input$oncoParameters3, input$oncoParameters4),
                   drop = FALSE]
    epitad_datatable(genes)
  }, server = FALSE)

  output$megaPlot <- renderPlot({
    ld <- dat()
    chrX <- max(ld$chr, na.rm = TRUE)

    minBP <- ifelse(values$tmp_min == 0, total_min(), values$tmp_min)
    maxBP <- ifelse(values$tmp_max == 999, total_max(), values$tmp_max)

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

    ###########################################################################################

    mat_layout <- matrix(c(1, 2, 3, 4, 1, 2, 3, 4), nrow = 4, ncol = 2)
    layout(mat_layout, c(4, 4, 4, 4), c(2.25, 1.25, 0.5, 0.5))
    par(mar = c(0.5, 4.5, 0.5, 0.5))

    phic <- plotHic(hic_matrix,
      chrom = paste0("chr", chrX),
      chromstart = min(as.numeric(colnames(hic_matrix))),
      chromend = max(as.numeric(colnames(hic_matrix))),
      max_y = 20, zrange = c(0, 28), palette = ifelse(input$plotColor == 1, topo.colors,
        ifelse(input$plotColor == 2, rainbow,
          ifelse(input$plotColor == 3, heat.colors,
            ifelse(input$plotColor == 4, terrain.colors, cm.colors)
          )
        )
      ),
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
    segments(x0 = genes$Start, y0 = 0.5, x1 = genes$End, y1 = 0.5, lwd = 30, col = ifelse(input$plotColor == 1, topo.colors(n = nrow(genes), alpha = 0.7),
      ifelse(input$plotColor == 2, rainbow(n = nrow(genes), alpha = 0.7),
        ifelse(input$plotColor == 3, heat.colors(n = nrow(genes), alpha = 0.7),
          ifelse(input$plotColor == 4, terrain.colors(n = nrow(genes), alpha = 0.7), rev(cm.colors(n = nrow(genes), alpha = 0.7)))
        )
      )
    ), lend = 1)
    text(x = (genes$Start + genes$End) / 2, y = c(0.7, 0.3, 0.8, 0.2), labels = genes$Symbol, col = ifelse(input$plotColor == 1, topo.colors(n = nrow(genes), alpha = 0.7),
      ifelse(input$plotColor == 2, rainbow(n = nrow(genes), alpha = 0.7),
        ifelse(input$plotColor == 3, heat.colors(n = nrow(genes), alpha = 0.7),
          ifelse(input$plotColor == 4, terrain.colors(n = nrow(genes), alpha = 0.7), rev(cm.colors(n = nrow(genes), alpha = 0.7)))
        )
      )
    ))
    mtext("Genes", side = 2, line = 1.75, cex = .75, font = 2)

    plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
    abline(v = ld[ld$is_query_snp == 0, ]$pos_hg38, col = "grey", lend = 1) # lwd=6
    abline(v = ld[ld$is_query_snp == 1, ]$pos_hg38, col = ifelse(input$plotColor == 1, topo.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7),
      ifelse(input$plotColor == 2, rainbow(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7),
        ifelse(input$plotColor == 3, heat.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7),
          ifelse(input$plotColor == 4, rev(terrain.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7)), cm.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7))
        )
      )
    ), lend = 1)
    mtext("LD", side = 2, line = 1.75, cex = .75, font = 2)

    plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
    segments(
      x0 = tads[tads$seqnames == paste0("chr", chrX), ]$start,
      y0 = 0.5,
      x1 = tads[tads$seqnames == paste0("chr", chrX), ]$end,
      y1 = 0.5, lwd = 30,
      col = ifelse(input$plotColor == 1, topo.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7),
        ifelse(input$plotColor == 2, rainbow(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7),
          ifelse(input$plotColor == 3, heat.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7),
            ifelse(input$plotColor == 4, terrain.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7), cm.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7))
          )
        )
      ),
      lend = 1
    )
    mtext("TADs", side = 2, line = 1.75, cex = .75, font = 2)
  })

  output$plotDownload <- downloadHandler(
    filename = function() {
      paste("episnpR", ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      ld <- dat()
      chrX <- max(ld$chr, na.rm = TRUE)

      minBP <- ifelse(values$tmp_min == 0, total_min(), values$tmp_min)
      maxBP <- ifelse(values$tmp_max == 999, total_max(), values$tmp_max)

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

      ###########################################################################################

      mat_layout <- matrix(c(1, 2, 3, 4, 1, 2, 3, 4), nrow = 4, ncol = 2)
      layout(mat_layout, c(4, 4, 4, 4), c(2.25, 1.25, 0.5, 0.5))
      par(mar = c(0.5, 4.5, 0.5, 0.5))

      phic <- plotHic(hic_matrix,
        chrom = paste0("chr", chrX),
        chromstart = min(as.numeric(colnames(hic_matrix))),
        chromend = max(as.numeric(colnames(hic_matrix))),
        max_y = 20, zrange = c(0, 28), palette = ifelse(input$plotColor == 1, topo.colors,
          ifelse(input$plotColor == 2, rainbow,
            ifelse(input$plotColor == 3, heat.colors,
              ifelse(input$plotColor == 4, terrain.colors, cm.colors)
            )
          )
        ),
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
      segments(x0 = genes$Start, y0 = 0.5, x1 = genes$End, y1 = 0.5, lwd = 30, col = ifelse(input$plotColor == 1, topo.colors(n = nrow(genes), alpha = 0.7),
        ifelse(input$plotColor == 2, rainbow(n = nrow(genes), alpha = 0.7),
          ifelse(input$plotColor == 3, heat.colors(n = nrow(genes), alpha = 0.7),
            ifelse(input$plotColor == 4, terrain.colors(n = nrow(genes), alpha = 0.7), rev(cm.colors(n = nrow(genes), alpha = 0.7)))
          )
        )
      ), lend = 1)
      text(x = (genes$Start + genes$End) / 2, y = c(0.7, 0.3, 0.8, 0.2), labels = genes$Symbol, col = ifelse(input$plotColor == 1, topo.colors(n = nrow(genes), alpha = 0.7),
        ifelse(input$plotColor == 2, rainbow(n = nrow(genes), alpha = 0.7),
          ifelse(input$plotColor == 3, heat.colors(n = nrow(genes), alpha = 0.7),
            ifelse(input$plotColor == 4, terrain.colors(n = nrow(genes), alpha = 0.7), rev(cm.colors(n = nrow(genes), alpha = 0.7)))
          )
        )
      ))
      mtext("Genes", side = 2, line = 1.75, cex = .75, font = 2)

      plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      abline(v = ld[ld$is_query_snp == 0, ]$pos_hg38, col = "grey", lend = 1) # lwd=6
      abline(v = ld[ld$is_query_snp == 1, ]$pos_hg38, col = ifelse(input$plotColor == 1, topo.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7),
        ifelse(input$plotColor == 2, rainbow(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7),
          ifelse(input$plotColor == 3, heat.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7),
            ifelse(input$plotColor == 4, rev(terrain.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7)), cm.colors(n = nrow(ld[ld$is_query_snp == 1, ]), alpha = 0.7))
          )
        )
      ), lend = 1)
      mtext("LD", side = 2, line = 1.75, cex = .75, font = 2)

      plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      segments(
        x0 = tads[tads$seqnames == paste0("chr", chrX), ]$start,
        y0 = 0.5,
        x1 = tads[tads$seqnames == paste0("chr", chrX), ]$end,
        y1 = 0.5, lwd = 30,
        col = ifelse(input$plotColor == 1, topo.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7),
          ifelse(input$plotColor == 2, rainbow(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7),
            ifelse(input$plotColor == 3, heat.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7),
              ifelse(input$plotColor == 4, terrain.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7), cm.colors(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]), alpha = 0.7))
            )
          )
        ),
        lend = 1
      )
      mtext("TADs", side = 2, line = 1.75, cex = .75, font = 2)

      dev.off()
    }
  )

  output$plotStart <- renderUI({
    numericInput("plotStartBP", label = "Starting Coordinates (BP)", value = total_min())
  })

  output$plotEnd <- renderUI({
    numericInput("plotEndBP", label = "Ending Coordinates (BP)", value = total_max())
  })
}
