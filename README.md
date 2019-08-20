
<!-- README.md is generated from README.Rmd. Please edit that file -->

# measles.katanga

This repository contains the data and code for our paper:

> Sebastian Funk, Saki Takahashi, Joel Hellewell, Kartini Gadroen,
> Isidro Carrion-Martin, Marit van Lenthe, Katiana Rivette, Sebastian
> Dietrich, W. John Edmunds, M. Ruby Siddiqui and V. Bhargavi Rao
> (2019). *The impact of reactive mass vaccination campaigns on measles
> outbreaks in the Katanga region, Democratic Republic of Congo*.
> medRxiv 19003434 <https://doi.org/10.1101/19003434>

### How to download or install

You can download the compendium as a zip from from this URL:
<https://github.com/sbfnk/measles.katanga/archive/master.zip>

Or you can install this compendium as an R package, measles.katanga,
from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("sbfnk/measles.katanga")
```

## Figures and analysis

Figures 1-4 can be re-created using

``` r
figure1()
figure2()
figure3()
figure4()
```

Figure 5 uses the outputs from the prediction model, which can be run
using

``` r
p <- prediction_model()
figure5(p)
```

Figure 6 uses the outputs from the dynamic model. These are included in
the model as `libbi` objects which can be retrieved using.

``` r
posterior <- rbi::read_libbi(system.file(package="measles.katanga", file.path("bi", "inst/bi/posterior.rds")))
posterior_no_mvc <- rbi::read_libbi(system.file(package="measles.katanga", file.path("bi", "inst/bi/posterior_no_mvc.rds")))
```

The two variables `posterior` and `posterior_no_mvc` then contain
results from posterior sampling and using these to resimulate without
mass vaccination campaigns, respectively. These can be re-created using

``` r
model <- posterior$model
posterior <- fit_dynamic_model(model, nbdata=10)
posterior_no_mvc <- remove_mvc(posterior)
```

The `fit_dynamic_model` command may take a few hours to run, depending
on the hardware available (especially subject to availability of a fast
Graphical Processing Unit, GPU).

From these two variables, Figure 6 can be re-created using

``` r
figure6(posterior, posterior_no_mvc)
```
