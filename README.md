# epiTAD
epiTAD is a shiny app for visualizing Hi-C data and comparing genomic annotations from multiple public databases to facilitate *in silico* discovery. The application can be accessed at [https://apps.gerkelab.com/epiTAD/](https://apps.gerkelab.com/epiTAD/).

<!-- README start -->

## Key Operations 

### Searching by SNP

![Simple query](https://github.com/tgerke/epiTAD/raw/master/figures/snp_query.gif)

Users can search SNPs in two ways 1) by typing rs IDs in the text box (comma separated if multiple) or 2) uploading a text file with one rs ID per line. 

### Variant annotations

![Changing displayed SNP level data](https://github.com/tgerke/epiTAD/raw/master/figures/snp_anno.gif)

### Gene annotations

![Changing displayed gene level data](https://github.com/tgerke/epiTAD/raw/master/figures/gene_anno.gif)

### Visualizations

![Main visual](https://github.com/tgerke/epiTAD/raw/master/figures/visual.gif)

By selecting plot options above the produced figure, users can alter the displayed region and color scheme and download the figure. 

### Examples

![Pre-loaded examples](https://github.com/tgerke/epiTAD/raw/master/figures/preloaded_examples.gif)

## Additional Features

Users can download the figure and tables produced and bookmark any querys performed. 

A window providing further information about the application can be accessed by pressing the "i" button to the right of the "Perform Query" button.

epiTAD should work in most modern web browsers, however, optimal performance has been observed in Chrome.

## Data Sources

LD is calculated from 1000 Genomes Phase 1 and queried from the HaploR interface to HaploReg. 

TAD locations are based off of those defined by Dixon et al in 'Topological domains in mammalian genomes identified by analysis of chromatin interactions'.

The provided eQTLs were queried from GTEx.

The Hi-C values are from Human Fibroblast IMR90 Hi-C experiments by Dixon et al (GSE35156). 

If no SNPs are in LD above the specified threshold then a range of 53500 BP is applied to either side of the SNP. If SNPs in LD exist, then the range is set to the smallest region which covers of all genomic locations in LD with the queried SNP(s) and the TAD region. This range is used for querying data from Oncotator, ENSEMBL, ClinVar and the Genome Browser.

## Citing epiTAD

epiTAD was developed by Jordan Creed, Garrick Aden-Buie and Travis Gerke with scientific input from Alvaro Monteiro.


