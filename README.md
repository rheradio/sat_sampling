# Scalable and Uniform SAT-Sampling for Configurable Systems

Material of the experiments reported in:

*Ruben Heradio, David Fernandez-Amoros, Jose Galindo, and David Benavides*. 
**Scalable and Uniform SAT-Sampling for Configurable Systems.** 

*This paper has been submitted for publication to the 24TH ACM INTERNATIONAL SYSTEMS AND SOFTWARE PRODUCT LINE CONFERENCE (SPLC 2020).*

## Abstract

Some relevant analyses on configurable software systems remain intractable because they require examining vast and highly-constrained configuration spaces. Those analyses could be addressed through statistical inference, i.e., working with a much more tractable sample that later supports generalizing the results obtained to the entire configuration space. To make this possible, the laws of statistical inference impose an indispensable requirement: each member of the population must be equally likely to be included in the sample, i.e., the sampling process needs to be ``uniform''. Various SAT-samplers have been developed for generating uniform random samples at a reasonable computational cost. However, there is a lack of experimental validation over large configuration models that shows if the samplers indeed produce genuine uniform samples. This paper (i) presents a statistical goodness-of-fit test to verify to what extent samplers accomplish uniformity and (ii) reports the evaluation of four state-of-the-art samplers: Spur, QuickSampler, Unigen2, and Smarch.  According to our experimental results, only Spur satisfies both scalability and uniformity.

## Summary

This repository is organized in two directories:

* [experimental_results](https://github.com/rheradio/sat_sampling/tree/master/experimental_results), includes our experimental results.
* [scripts](https://github.com/rheradio/sat_sampling/tree/master/scripts), includes the R scripts we wrote to run the experiments.


## Experimental results

The next figure sketches the method we propose for verifying if a sampler generates uniform random samples of a model encoded as a Boolean formula. It compares *empirical information* about a sample with *theoretical information* about the whole population of SAT solutions that the model represents
![Scheme summarizing the scripts](https://github.com/rheradio/sat_sampling/blob/master/scripts/goodness_of_fit_schema.png)

## Scripts

![Scheme summarizing the scripts](https://github.com/rheradio/sat_sampling/blob/master/scripts/scripts_workflow_schema.png)

