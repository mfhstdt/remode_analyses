# Remode Analyses

This repository contains all code required to replicate the analyses of the paper "ReMoDe - Recursive mode detection in distributions of ordinal data"

### Replication of the simulation study
The `simulation_study` folder contains analysis scripts to apply modality detection methods to a benchmark collection of 172 distributions (see script `01_apply_methods.R`) and to analyze accuracies per method in discriminating uni- from multimodality and in detecting the true number of modes (see script `02_analyse_accuracies.R`). The benchmark distributions are stored as an RData file. Code to replicate these distributions can be found at https://github.com/mfhstdt/benchmark_ordinal_distributions. 
This folder furthermode contains a subfolder containing scripts to replicate or test of the effect of resampling reeatedly from the benchmark on our results. 

### Replication of testing different alpha correction methods 
Remode is repeatedly applied to each benchmark distribution, each time implementing a different alpha correction method. All code to replicate this simulation can be found in the `check_alpha_correction` folder. 

### Replication of applying remode to empirical datasets 
The `application_to_emp_data` folder contains a script to apply remode to openly available datasets on attitudes, coma incidence in patients (Di Saverio et al., 2014) and on a continuous distribution of U-Pb ages of detrital zircon. The variables of interest are stored as rda files in `application_to_emp_data/open_datasets`. The datasets can be accessed as follows: 

- *LISS Panel*: Centerdata. (2021). Longitudinal internet studies for the social sciences, core panel [dataset and documentation]. www.lissdata.nl/
- *Eurobarometer*: European Commission. (2022). Eurobarometer 97.1 [dataset and documentation.GESIS, Cologne. ZA7886 Data file Version 2.0.0]. https://doi.org/10.4232/1.14101
- *European Value Study & World Value Survey*: European Value Systems Study Group & World Values Survey Association. (2021). European values study and world values survey: Joint evs/wvs 2017-2022 dataset [dataset and documentation]. https://europeanvaluesstudy.eu/methodology-data-documentation/survey-2017/joint-evs-wvs/
- *Dataset on attitudes towards COVID-19 vaccines*:  see https://osf.io/357h4/ for codebook and data; Chambon, M., Kammeraad, W. G., van Harreveld, F., Dalege, J., Elberse, J. E., & van der Maas, H. L. J. (2022). Understanding change in COVID-19 vaccination intention with  network analysis of longitudinal data from dutch adults. npj Vaccines, 7 (1), 114
- *Global U-Pb database 2019*: download dataset here: https://www.nature.com/articles/s41597-023-02902-9; published in Puetz, S. J., Spencer, C. J., Condie, K. C., & Roberts, N. M. W. (2024). Enhanced u-pb detrital zircon, lu-hf zircon, 18o zircon, and sm-nd whole rock global databases. Scientific Data, 11 (1), 56. https://doi.org/https://doi.org/10.1038/s41597-023-029

