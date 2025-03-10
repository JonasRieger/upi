# Vladimir vs. the Virus - a Tale of two Shocks
## An Update on our Uncertainty Perception Indicator (UPI) to April 2022
### a Research Note
The repository provides selected data and scripts related to the working papers

* [Müller, H., Rieger. J. & Hornig, N. (2022).](http://dx.doi.org/10.17877/DE290R-22780) Vladimir vs. the Virus - a Tale of two Shocks. An Update on our Uncertainty Perception Indicator (UPI) to April 2022 - a Research Note. *DoCMA Working Paper #11*.
* [Müller, H., Rieger. J. & Hornig, N. (2021).](http://dx.doi.org/10.17877/DE290R-22177) "Riders on the Storm". The Uncertainty Perception Indicator (UPI) in Q1 2021. *DoCMA Working Paper #7*.
* [Müller, H., Rieger. J. & Hornig, N. (2021).](http://dx.doi.org/10.17877/DE290R-21974) "We’re rolling". Our Uncertainty Perception Indicator (UPI) in Q4 2020: introducing RollingLDA, a new Method for the Measurement of Evolving Economic Narratives. *DoCMA Working Paper #6*.

## Citation
If you're using data from this repository or want to refer to the methodology of the UPI, please cite our paper
* [Rieger, J., Hornig, N., Schmidt, T. and Müller, H. (2023).](https://github.com/JonasRieger/mufin23/blob/master/paper.pdf) Early Warning Systems? Building Time Consistent Perception Indicators for Economic Uncertainty and Inflation Using Efficient Dynamic Modeling. Accepted for [MUFin’23](https://sites.google.com/view/w-mufin).

Instead, if you wish to refer to a particular interpretation, please cite the corresponding working paper.

For bug reports, comments and questions please use the [issue tracker](https://github.com/JonasRieger/upi/issues).

## Related Software
* [rollinglda](https://github.com/JonasRieger/rollinglda) to model the rolling version of LDA.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) to determine a prototype from a number of runs of Latent Dirichlet Allocation.
* [tosca](https://github.com/Docma-TU/tosca) to manage and manipulate the corpora to a structure requested by ``ldaPrototype`` and to plot the corpora.
* [tmT](https://github.com/Docma-TU/tmT) to read the raw XML files of the articles and create the ``textmeta`` objects of the corpus.
* [tm](https://CRAN.R-project.org/package=tm) to preprocess the text data.
* [data.table](https://github.com/Rdatatable/data.table) to manage data tables.
* [lubridate](https://lubridate.tidyverse.org/) to handle dates.
* [ggplot2](https://ggplot2.tidyverse.org/) and
* [GGally](https://github.com/ggobi/ggally) to visualize some statistics.

## Usage
Please note: For legal reasons the repository cannot provide all data. Please [let us know](https://github.com/JonasRieger/upi/issues) if you feel that there is anything missing that we could add.
