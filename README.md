# Title
### Subtitle
The repository provides selected data and scripts related to the working paper

* INSERT CITATION WP5 (2021). Title - Subtitle. [*DoCMA Working Paper #5*](https://docma.tu-dortmund.de/working-papers/).

For bug reports, comments and questions please use the [issue tracker](https://github.com/JonasRieger/upi/issues).

## Related Software
* [tmT](https://github.com/Docma-TU/tmT) to read the raw XML files of the articles and create the ``textmeta`` objects of the corpus.
* [tosca](https://github.com/Docma-TU/tosca) to manage and manipulate the corpora to a structure requested by ``ldaPrototype`` and to plot the corpora.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) to determine a prototype from a number of runs of Latent Dirichlet Allocation.
* [ldaGibbs](https://github.com/JonasRieger/ldaGibbs) to model Latent Dirichlet Allocations with a subset of articles assignments fixed.
* [tm](https://CRAN.R-project.org/package=tm) to preprocess the text data.
* [data.table](https://github.com/Rdatatable/data.table) to manage data tables.
* [lubridate](https://lubridate.tidyverse.org/) to handle dates.
* [ggplot2](https://ggplot2.tidyverse.org/) and
* [GGally](https://github.com/ggobi/ggally) to visualize some statistics.

## Usage
Please note: For legal reasons the repository cannot provide all data. Please [let us know](https://github.com/JonasRieger/upi/issues) if you feel that there is anything missing that we could add. 

The scripts ``data_init.R``, ``data_update.R`` and ``data_prep.R`` show the workflow of corpus creation and preprocessing, while ``fwrite.R`` shows how to create a CSV file from a ``textmeta`` object. The script ``lda.R`` contains the code for modeling the *RollingLDA* and ``analysis.R`` for analyzing it.
