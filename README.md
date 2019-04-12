# epiTAD
epiTAD is a shiny app for visualizing Hi-C data and comparing genomic annotations from multiple public databases to facilitate *in silico* discovery. The application can be accessed at [https://apps.gerkelab.com/epiTAD/](https://apps.gerkelab.com/epiTAD/), or [run locally using Docker](#run-epitad-locally-with-docker).

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

## Run epiTAD locally with Docker

[epitad-docker-hub]: https://hub.docker.com/r/gerkelab/epitad
[ropensci-docker-tutorial]: https://ropenscilabs.github.io/r-docker-tutorial/
[rocker]: https://www.rocker-project.org/
[docker]: https://www.docker.com/
[docker-install-linux]: https://docs.docker.com/linux/step_one/
[docker-install-mac]: https://docs.docker.com/mac/step_one/
[docker-install-windows]: https://docs.docker.com/windows/step_one/
[shiny]: https://shiny.rstudio.com

epiTAD is a web application built using [Shiny], a framework for building interactive applications using R. We have prepared a [pre-configured and self-contained Docker image][epitad-docker-hub] that can be used to run the application locally on a user's machine. 

[Docker] is a tool for creating, deploying, and running applications inside _containers_, which are similar to virtual machines. Once users have installed docker on their machines (available for [Linux][docker-install-linux], [MacOS][docker-install-mac], and [Windows][docker-install-windows]), they can run epiTAD with a single terminal command:

```bash
docker run -d --rm -p 3838:3838 --name epiTAD gerkelab/epitad:latest
```

The first time epiTAD is launched, docker will first download and extract the application container. When the process is complete, users can navigate to `127.0.0.1:3838/epiTAD` to use the same epiTAD application interface as the one available online at <https://apps.gerkelab.com/epiTAD/>. (Note that an active internet connection is still required for epiTAD to function properly.)

To stop the docker container and shut down epiTAD, run

```bash
docker stop epiTAD
```

For users interested in learning more about Docker, we recommend [rOpenSci Labs' Docker Tutorial][ropensci-docker-tutorial]. We also thank the [Rocker Project][rocker] for creating and maintaining the R-specific Docker containers upon which epiTAD was built.

## Citing epiTAD

epiTAD was developed by Jordan Creed, Garrick Aden-Buie and Travis Gerke with scientific input from Alvaro Monteiro.


