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

  DT:::DT2BSClass(c("stripe", "hover", "compact", "cell-border"))

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
          if (!quiet) message("Error: ", e$message)

          list(result = NULL, error = if (is.null(error_msg)) e$message else error_msg)
        },
        interrupt = function(e) {
          stop("Terminated by user", call. = FALSE)
        }
      )
    }
  }

  safe_queryHaploreg <- safely(queryHaploreg)

  # A flag to trigger recalc of other UI elements when query button is hit
  r_trigger_queried <- reactiveVal(FALSE)

  dat <- eventReactive(input$update1, {
    if (input$snpList == "") {
      dat <- sample()
      snps <- dat[, 1]
      x <- safe_queryHaploreg(query = snps, ldThresh = as.numeric(input$value), ldPop = input$pop)
    } else {
      snps <- as.character(unlist(strsplit(input$snpList, ",")))
      snps <- trimws(snps)
      x <- safe_queryHaploreg(query = snps, ldThresh = input$value, ldPop = input$pop)
    }
    shiny::validate(need(is.null(x$error), SNP_QUERY_ERROR))
    r_trigger_queried(!r_trigger_queried())
    if (is.null(x$error)) {
      x <- x$result
      x$chr <- as.numeric(as.character(x$chr))
      x$pos_hg38 <- as.numeric(as.character(x$pos_hg38))
    }
    x
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
    } else {
      snps <- as.character(unlist(strsplit(input$snpList, ",")))
      snps <- trimws(snps)
      x <- queryRegulome(query = snps)
      shiny::validate(need(nrow(x$res.table) > 0, SNP_QUERY_ERROR))
      x <- as.data.frame(x$res.table)
      x$score <- as.character(x$score)
      x$score_anno <- NA
    }

    for (i in seq_len(nrow(x))) {
      x$score_anno[i] <- switch(
        x$score[i],
        "1a" = "eQTL + TF binding + matched TF motif + matched DNase Footprint + DNase peak",
        "1b" = "eQTL + TF binding + any motif + DNase Footprint + DNase peak",
        "1c" = "eQTL + TF binding + matched TF motif + DNase peak",
        "1d" = "eQTL + TF binding + any motif + DNase peak",
        "1e" = "eQTL + TF binding + matched TF motif",
        "1f" = "eQTL + TF binding / DNase peak",
        "2a" = "TF binding + matched TF motif + matched DNase Footprint + DNase peak",
        "2b" = "TF binding + any motif + DNase Footprint + DNase peak",
        "2c" = "TF binding + matched TF motif + DNase peak",
        "3a" = "TF binding + any motif + DNase peak",
        "3b" = "TF binding + matched TF motif",
        "4"  = "TF binding + DNase peak",
        "5"  = "TF binding or DNase peak",
        "Other"
      )
    }
    x
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

  output$tadBoundaries <- renderText({
    in_tad <- in_tad()
    if (nrow(in_tad) < 1) {
      paste0("Not in a TAD!")
    } else {
      paste0("In a TAD! The TAD ranges from ", in_tad$start_position, " to ", in_tad$end_position)
    }
  })

  output$eTable1 <- DT::renderDataTable({
    dat <- dat()
    dat <- dat[dat$eQTL != ".", ]
    etest <- strsplit(as.character(dat$eQTL), ";")
    names(etest) <- dat$rsID
    etest2 <- unlist(strsplit(unlist(etest), ","))

    # Check inputs and that there are eQTLs for these SNPs
    shiny::validate(need(etest2, "No statistically significant eQTLs were reported with these SNPs."))
    shiny::validate(need(input$tissue, "Please select desired Tissues from the above dropdown."))

    # Return table
    etest3 <- matrix(etest2, nrow = length(names(unlist(etest))), ncol = 4, byrow = TRUE)
    etest3 <- as.data.frame(etest3)
    etest3 <- cbind(names(unlist(etest)), etest3)
    colnames(etest3) <- c("SNP", "Source", "Tissue", "Gene", "p")
    etest3 <- etest3[!duplicated(etest3), ]
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
    values$tmp_min <- total_min()
    values$tmp_max <- total_max()
    updateNumericInput(session, "plotStartBP", value = total_min())
    updateNumericInput(session, "plotEndBP", value = total_max())
  })

  output$hic1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    if (length(x) > 1) {
      a("Yue Lab HIC Browser for selected region", href = paste0("http://promoter.bx.psu.edu/hi-c/view.php?species=human&assembly=hg19&source=inside&tissue=GM12878&type=Lieberman-raw&c_url=&transfer=&chr=chr", max(as.numeric(y$chr), na.rm = TRUE), "&start=", total_min, "&end=", total_max, "&sessionID=&browser=none"), target = "_blank")
    } else if (length(x) == 1) {
      a("Yue Lab HIC Browser for SNP", href = paste0("http://promoter.bx.psu.edu/hi-c/view.php?species=human&assembly=hg19&source=inside&tissue=GM12878&type=Lieberman-raw&resolution=25&c_url=&transfer=&gene=", x, "&sessionID=&browser=none"), target = "_blank")
    }
  })

  output$clinical1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    a("NCDB ClinVar for selected region", href = paste0("https://www.ncbi.nlm.nih.gov/clinvar/?term=", max(as.numeric(y$chr), na.rm = TRUE), "%5Bchr%5D+AND+", total_min, "%3A", total_max, "%5Bchrpos37%5D"), target = "_blank")
  })

  output$ucsc1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    a("UCSC Genome Browser for selected region", href = paste0("https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr", max(as.numeric(y$chr), na.rm = TRUE), "%3A", total_min, "%2D", total_max, "&hgsid=598506407_cis2LZUJLabCsy1N2YPEuJv8vbBZ"), target = "_blank")
  })

  output$eqtl1 <- renderUI({
    x <- snps()
    y <- dat()
    total_min <- total_min()
    total_max <- total_max()
    link <- if (length(x) > 1) {
      a("GTEx eQTL browser for selected region", href = paste0("https://www.gtexportal.org/home/browseEqtls?location=chr", max(as.numeric(y$chr), na.rm = TRUE), ":", total_min, "-", total_max), target = "_blank")
    } else if (length(x) == 1) {
      a("GTEx eQTL browser for SNP", href = paste0("https://www.gtexportal.org/home/snp/", x), target = "_blank")
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

  genes <- reactive({
    ld <- dat()
    chr <- max(ld$chr, na.rm = TRUE)
    total_min <- total_min()
    total_max <- total_max()

    getBM(
      attributes = c("hgnc_symbol", "start_position", "end_position"),
      filters = c("chromosomal_region"),
      values = paste0(chr, ":", total_min, ":", total_max),
      mart = ensembl54
    )
  })

  output$geneTable <- DT::renderDataTable({
    epitad_datatable(genes())
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

  plot_color <- reactive({
    req(input$plotColor)
    switch(
      tolower(input$plotColor),
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
  })


  # Mega Plot ---------------------------------------------------------------
  output$megaPlot <- renderPlotly({
    # ---- Mega Plot: pull in needed data pieces ----
    ld <- dat()
    chrX <- max(ld$chr, na.rm = TRUE)

    minBP <- values$tmp_min
    maxBP <- values$tmp_max

    hic_dat <- extractRegion(hiC[[paste0("chr", chrX, "chr", chrX)]],
                             chr = paste0("chr", chrX),
                             from = minBP, to = maxBP
    )
    hic_matrix <- as.matrix(intdata(hic_dat))

    genes <- genes()
    colnames(genes) <- c("Symbol", "Start", "End")

    tads <- as.data.frame(tads_imr90)

    # ---- Mega Plot: create plot ----

    ## create dataframe for plotting triangular heatmap
    # determine number of bins
    nbins <- nrow(hic_matrix)
    stepsize <- abs(minBP - maxBP) / (2 * nbins)

    # scale
    vec <- hic_matrix
    vec[which(vec < 0)] <- 0
    vec[which(vec > 28)] <- 28
    breaks <- seq(0, 28, length.out = 100)
    cols_num <- c(0:length(breaks) + 1)
    cols_vec <- cut(vec, c(-Inf, breaks, Inf), labels = cols_num)
    hicmcol <- matrix(as.numeric(as.character(cols_vec)), nrow = nrow(hic_matrix))

    # make an empty tibble
    tmp <- tibble(x = numeric(), y = numeric(), f = numeric(), g = character(), v = numeric())

    for (i in (1:nrow(hic_matrix))) {
      y <- -.5

      x <- minBP + (i * 2 * stepsize) - (stepsize * 2)
      for (j in (i:ncol(hic_matrix))) {
        x <- x + stepsize
        y <- y + .5

        poly_dat <- tibble(
          x = c(x - stepsize, x, x + stepsize, x),
          y = c(y, y + .5, y, y - .5),
          f = hicmcol[i, j],
          g = paste0("bin_", i, "_", j),
          v = hic_matrix[i, j]
        )

        tmp <- bind_rows(tmp, poly_dat)
      }
    }
    rm(i, j)

    # Mega Plot: Base plot themes ----
    theme_blank <-
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )

    theme_blank_no_legend <- theme_blank + theme(legend.position = "none")

    # Mega Plot: HiC Plot ----
    phic <- ggplot(tmp, aes(x = x, y = y, text = paste0("Raw value: ", v))) +
      geom_polygon(aes(fill = f, group = g)) +
      scale_fill_gradientn(colors = plot_color()(n = 100), name = "Score") +
      coord_cartesian(xlim = c(minBP, maxBP)) +
      ylim(0, (nbins * 0.5) + 1) +
      ylab("HIC Intensities") +
      theme_blank +
      theme(
        legend.justification = c(1, 1), legend.position = c(1, 1),
      )

    # Mega Plot: Gene Plot ----
    pgene <- ggplot(genes) +
      geom_rect(
        mapping = aes(xmin = Start, xmax = End,
                      ymin = 0.1, ymax = 0.9,
                      lwd = 30,
                      text = paste0(
                        "Symbol: ", Symbol, "<br />",
                        "Start: ", Start, "<br />",
                        "End: ", End
                      )),
        fill = plot_color()(n = nrow(genes)),
        alpha = 0.7
      )

    if (input$showgenes %% 2 == 0) {
      pgene <- pgene +
        geom_text(
          aes(x = (Start + End) / 2,
              y = rep(c(1.05, -0.05), length.out = nrow(genes)),
              label = Symbol
          ),
          color = plot_color()(n = nrow(genes)), size = 3
        )
    }

    pgene <- pgene +
      coord_cartesian(xlim = c(minBP, maxBP)) +
      ylim(-0.2, 1.2) +
      guides(fill = FALSE, alpha = FALSE, size = FALSE) +
      ylab("Genes") +
      theme_blank_no_legend

    # Mega Plot: SNP Plot ----
    psnp <- ggplot(ld)

    if (nrow(ld[ld$is_query_snp == 0, ]) >= 1) {
      psnp <- psnp +
        geom_segment(
          aes(x = pos_hg38, y = 0,
              xend = pos_hg38, yend = 1,
              text = paste0(
                "rsID: ", rsID, "<br />",
                "Position: ", pos_hg38, "<br />",
                "Ref/Alt: ", ref, "/", alt
              )
          ),
          subset(ld, is_query_snp == 0),
          color = "grey"
        )
    }

    psnp <- psnp +
      geom_segment(aes(
        x = pos_hg38, y = 0,
        xend = pos_hg38, yend = 1,
        text = paste0(
          "rsID: ", rsID, "<br />",
          "Position: ", pos_hg38, "<br />",
          "Ref/Alt: ", ref, "/", alt
        )
      ),
      subset(ld, is_query_snp == 1),
      color = plot_color()(n = nrow(ld[ld$is_query_snp == 1, ]))
      ) +
      coord_cartesian(xlim = c(minBP, maxBP)) +
      ylim(0, 1) +
      guides(colour = FALSE, size = FALSE) +
      ylab("SNPs") +
      theme_blank_no_legend

    # Mega Plot: TAD Plot ----
    ptad <- ggplot(tads) +
      geom_rect(
        aes(xmin = start, xmax = end,
            ymin = 0.1, ymax = 0.9,
            alpha = 0.7,
            lwd = 30,
            text = paste0(chrX, ":", start, "-", end)
        ),
        subset(tads, tads$seqnames == paste0("chr", chrX)),
        fill = plot_color()(n = nrow(tads[tads$seqnames == paste0("chr", chrX), ]))
      ) +
      coord_cartesian(xlim = c(minBP, maxBP)) +
      ylim(0, 1) +
      guides(fill = FALSE, alpha = FALSE, size = FALSE) +
      labs(x = "BP", y = "TADs") +
      theme_blank_no_legend +
      theme(
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
      )

    # Mega Plot: Compose Final Plot
    p1 <- ggplotly(phic, tooltip = "text")
    p2 <- hide_legend(ggplotly(pgene, tooltip = "text"))
    p3 <- hide_legend(ggplotly(psnp, tooltip = "text"))
    p4 <- hide_legend(ggplotly(ptad, tooltip = "text"))

    megap <- subplot(p1, p2, p3, p4,
                     nrows = 4, heights = c(0.65, 0.15, 0.1, 0.1),
                     shareY = TRUE, shareX = TRUE
    )
  })

  output$plotDownload <- downloadHandler(
    filename = function() {
      paste("episnpR", ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      ld <- dat()
      chrX <- max(ld$chr, na.rm = TRUE)

      minBP <- values$tmp_min
      maxBP <- values$tmp_max

      hic_dat <- extractRegion(hiC[[paste0("chr", chrX, "chr", chrX)]],
                               chr = paste0("chr", chrX),
                               from = minBP, to = maxBP
      )
      hic_matrix <- as.matrix(intdata(hic_dat))

      genes <- genes()
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
                      max_y = 20, zrange = c(0, 28), palette = plot_color(),
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
      segments(x0 = genes$Start, y0 = 0.5, x1 = genes$End, y1 = 0.5, lwd = 30, col = plot_color()(n = nrow(genes), alpha = 0.7), lend = 1)
      text(x = (genes$Start + genes$End) / 2, y = c(0.7, 0.3, 0.8, 0.2), labels = genes$Symbol, col = plot_color()(n = nrow(genes), alpha = 0.7))
      mtext("Genes", side = 2, line = 1.75, cex = .75, font = 2)

      plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      abline(v = ld[ld$is_query_snp == 0, ]$pos_hg38, col = "grey", lend = 1) # lwd=6
      abline(v = ld[ld$is_query_snp == 1, ]$pos_hg38, col = plot_color()(n = nrow(genes), alpha = 0.7), lend = 1)
      mtext("LD", side = 2, line = 1.75, cex = .75, font = 2)

      plot(c(1, 1), xlim = c(minBP, maxBP), ylim = c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      segments(
        x0 = tads[tads$seqnames == paste0("chr", chrX), ]$start,
        y0 = 0.5,
        x1 = tads[tads$seqnames == paste0("chr", chrX), ]$end,
        y1 = 0.5, lwd = 30,
        col = plot_color()(n = nrow(genes), alpha = 0.7),
        lend = 1
      )
      mtext("TADs", side = 2, line = 1.75, cex = .75, font = 2)

      dev.off()
    }
  )

  # Update plot start when min changes
  observe({
    req(total_max())
    s_plotStartBP <- isolate(input$plotStartBP)
    if (s_plotStartBP < total_min()) {
      updateNumericInput(session, "plotStartBP", value = total_min())
    }
  })

  # Update plot end when max changes
  observe({
    req(total_max())
    s_plotEndBP <- isolate(input$plotEndBP)
    if (s_plotEndBP > total_max()) {
      updateNumericInput(session, "plotEndBP", value = total_max())
    }
  })

  observeEvent(input$btn_info, {
    modal_ui <- tagList(
      tags$h4("App Details"),
      tags$p(
        "LD is calculated from", tags$a("1000 Genomes Phase 1", href = "http://www.internationalgenome.org"),
        "and queried from the", tags$a("HaploR", href = "https://cran.r-project.org/web/packages/haploR/index.html"),
        "interface to", tags$a("HaploReg.", href = "http://archive.broadinstitute.org/mammals/haploreg/haploreg.php"),
        "TAD locations are based off of those defined by Dixon et al in 'Topological domains in mammalian genomes identified by analysis of chromatin interactions'.",
        "eQTLs are taken from", tags$a("GTEx", href = "https://gtexportal.org/home/"),
        "and IMR90 Hi-C values are available from ", tags$a("GSE35156", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE35156")
      ),
      tags$h4("Development Team"),
      tags$p(
        tags$strong("Programming:"), "Jordan Creed, Garrick Aden-Buie and Travis Gerke", tags$br(),
        tags$strong("Scientific Input:"), "Alvaro Monteiro", tags$br(),
        tags$strong("Website:"), tags$a(href = "https://gerkelab.com/project/epiTAD", "https://gerkelab.com/project/epiTAD"), tags$br(),
        tags$strong("Github:"), tags$a(href = "https://github.com/GerkeLab/epiTAD", "https://github.com/GerkeLab/epiTAD")
      ),
      tags$h4("Other resources"),
      tags$ul(
        tags$li(tags$a("Aiden Lab: Juicebox", href = "http://www.aidenlab.org/juicebox/", target = "_blank")),
        tags$li(tags$a("Yue Lab 3D Genome Browser", href = "http://promoter.bx.psu.edu", target = "_blank")),
        tags$li(tags$a("CHiCP", href = "https://www.chicp.org", target = "_blank")),
        tags$li(tags$a("HiGlass", href = "http://gehlenborglab.org/research/projects/higlass/", target = "_blank"))
      ),
      tags$h4("Notes"),
      tags$p(
        "If no SNPs are in LD above the specified threshold then a range of 53500 BP is applied to",
        "either side of the SNP. If SNPs in LD exist, then the range is set to the smallest region",
        "which covers of all genomic locations in LD with the queried SNP(s) and the TAD region.",
        "This range is used for querying data from Oncotator, ENSEMBL, ClinVar and the Genome Browser."
      )
    )

    showModal(modalDialog(
      title = "App Information", modal_ui,
      footer = modalButton("OK"), easyClose = TRUE
    ))
  })

  observe({
    req(r_trigger_queried())
    r_trigger_queried()
    values$tmp_min <- total_min()
    values$tmp_max <- total_max()
    updateNumericInput(session, "plotStartBP", value = total_min())
    updateNumericInput(session, "plotEndBP", value = total_max())
  }, priority = 1000)

  output$download_all <- downloadHandler(
    filename = function() {
      paste("epiTAD", ".xlsx", sep = "")
    },
    content = function(file) {
      # files <- NULL

      x <- dat()
      z <- dat2()

      haploReg_table <- x[, c("rsID", input$parameters)]
      regulomeDB_table <- z[, c("rsid", input$parameters2)]

      chr <- max(x$chr, na.rm = TRUE)
      total_min <- total_min()
      total_max <- total_max()

      genes <- genes()

      y <- fromJSON(paste0("http://portals.broadinstitute.org/oncotator/genes/", chr, "_", total_min, "_", total_max, "/"))

      onco <- as.data.frame(y[[1]])

      for (i in seq_along(y)[-1]) {
        gene_dat <- as.data.frame(y[[i]])
        onco <- rbind(onco, gene_dat)
      }

      onco <- onco[, c("gene", input$oncoParameters1, input$oncoParameters2, input$oncoParameters3, input$oncoParameters4),
                     drop = FALSE]

      etest <- unlist(strsplit(as.character(x$eQTL), ";"))
      etest <- etest[!etest %in% c(".")]
      etest2 <- unlist(strsplit(etest, ","))

      # Return table
      etest3 <- matrix(etest2, nrow = length(etest), ncol = 4, byrow = TRUE)
      etest3 <- as.data.frame(etest3)
      etest3 <- cbind(x[x$eQTL != ".", ]$rsID, etest3)
      colnames(etest3) <- c("SNP", "Source", "Tissue", "Gene", "p")
      etest3 <- etest3[!duplicated(etest3), ]
      etest3 <- etest3[etest3$Tissue %in% input$tissue, ]

      writexl::write_xlsx(list(
        "haploreg" = haploReg_table,
        "regulome" = regulomeDB_table,
        "ENSEMBL" = genes,
        "ONCOTATOR" = onco,
        "eQTL" = etest3
      ),
      path = file
      )
    }
  )
}
