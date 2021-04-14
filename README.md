# Scalable and Uniform SAT-Sampling for Configurable Systems

Material of the experiments reported in the following paper published in the 24TH ACM INTERNATIONAL SYSTEMS AND SOFTWARE PRODUCT LINE CONFERENCE (SPLC 2020):

*Ruben Heradio, David Fernandez-Amoros, José Galindo, and David Benavides*. 
**Scalable and Uniform SAT-Sampling for Configurable Systems.** 

## Abstract

Some relevant analyses on configurable software systems remain intractable because they require examining vast and highly-constrained configuration spaces. Those analyses could be addressed through statistical inference, i.e., working with a much more tractable sample that later supports generalizing the results obtained to the entire configuration space. To make this possible, the laws of statistical inference impose an indispensable requirement: each member of the population must be equally likely to be included in the sample, i.e., the sampling process needs to be ``uniform''. Various SAT-samplers have been developed for generating uniform random samples at a reasonable computational cost. However, there is a lack of experimental validation over large configuration models that shows if the samplers indeed produce genuine uniform samples. This paper (i) presents a statistical goodness-of-fit test to verify to what extent samplers accomplish uniformity and (ii) reports the evaluation of four state-of-the-art samplers: Spur, QuickSampler, Unigen2, and Smarch.  According to our experimental results, only Spur satisfies both scalability and uniformity.

## Acknowledgements

This work has been partially funded by the Spanish Ministry of Science, Innovation and Universities (projects VITAL-3D DPI2016-77677-P, and OPHELIA RTI2018-101204-B-C22); the Community of Madrid (research network CAM RoboCity2030 S2013/MIT-2748);  the TASOVA network (MCIU-AEI TIN2017-90644-REDT); and the Junta de Andalucia (METAMORFOSIS project).

## Summary

This repository is organized into two directories:

* [experimental_results](https://github.com/rheradio/sat_sampling/tree/master/experimental_results), includes our experimental results.
* [scripts](https://github.com/rheradio/sat_sampling/tree/master/scripts), includes the R scripts we wrote to run the experiments.


## Experimental results

The following figure sketches the method we propose for verifying if a sampler generates uniform random samples of a model encoded as a Boolean formula. It compares *empirical information* about a sample with *theoretical information* about the whole population of SAT solutions that the model represents.

![Schema summarizing the goodness-of-fit test](https://github.com/rheradio/sat_sampling/blob/master/scripts/goodness_of_fit_schema.png)

* [population_desc_stats.csv](https://github.com/rheradio/sat_sampling/blob/master/experimental_results/population_desc_stats.csv) describes the time required to characterize the SAT solution population concerning the variable probability distribution.
* [samplers_stats.csv](https://github.com/rheradio/sat_sampling/blob/master/experimental_results/samplers_stats.csv) describes the time each sampler required to generate the samples.
* [sampler_pvalues.csv](https://github.com/rheradio/sat_sampling/blob/master/experimental_results/sampler_pvalues.csv) describes the results of the goodness of fit tests.

The benchmark we used, and all the samples generated are available at [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3757091.svg)](https://doi.org/10.5281/zenodo.3757091)
. There is a zip file per model, which is organized into the following directories:

* `bool_formula`: includes the model's Boolean encoding as a BDD (`.dddmp`) and a CNF (`.dimacs`).
* `goodness_of_fit`: includes a graphical analysis of the model's goodness-of-fit.
* `population_desc`: population variable probabilities.
* `samples`: samples generated in each sampler's original format.
* `std_samples`: standardized samples. Each sample is characterized by the model variable frequencies.

## Script code

Our scripts are written in the [R language](https://cran.r-project.org/). In addition to R,
you'll need to install several R packages. To do so, run the following R code:

```R
list.of.packages <- c("forcats", "gmp", "gridExtra", "philentropy", "philentropy", "psych",
                      "tictoc", "tidyverse", "rapport", "Rmpfr", "R.utils", "pwr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

The following figure summarizes the scripts' workflow.

![Schema summarizing the scripts' workflow](https://github.com/rheradio/sat_sampling/blob/master/scripts/scripts_workflow_schema.png)

The syntax to run all scripts is:

`Rscript script_name.r directory_name`

Where `directory_name` is the folder that stores the models. Models can be downloaded at  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3757091.svg)](https://doi.org/10.5281/zenodo.3757091).

[run_samplers.r](https://github.com/rheradio/sat_sampling/blob/master/scripts/run_samplers.r) requires to have installed the following programs:

* [Unigen2](https://bitbucket.org/kuldeepmeel/unigen)
* [QuickSampler](https://github.com/RafaelTupynamba/quicksampler)
* [Spur](https://github.com/ZaydH/spur)
* [Smarch](https://github.com/jeho-oh/Kclause_Smarch)
* [PicoSAT](http://fmv.jku.at/picosat/)
* [probability](https://github.com/rheradio/VMStatAnal)

Also, at the beginning of [run_samplers.r](https://github.com/rheradio/sat_sampling/blob/master/scripts/run_samplers.r) you'll have to configure the constants QUICK_SAMPLER, QUICK_SAMPLER_VALID, SMARCH, SPUR, UNIGEN2_dir, and UNIGEN2 according to the locations where you have installed the samplers.
