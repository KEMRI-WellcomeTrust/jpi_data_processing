#!/bin/bash
Rscript /home/nngao/jpi_data_processing/data_extraction.R >> /home/nngao/jpi_data_processing/logs/data_extraction.log 2>&1
Rscript -e "rmarkdown::render('/home/nngao/jpi_data_processing/jpi_data_processing.Rmd', output_dir='/home/nngao/jpi_data_processing/outputs')" >> /home/nngao/jpi_data_processing/logs/data_processing.log 2>&1
