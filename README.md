ROMIC standardizes the formatting of genomic data to open up general visualizations approaches which can be used for exploratory data analysis (EDA).

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/romic)](https://cran.r-project.org/package=romic)
[![R build status](https://github.com/calico/romic/workflows/R-CMD-check/badge.svg)](https://github.com/calico/romic/actions)
<!-- badges: end -->

## Package Setup

To install *romic* from CRAN, run the following code in R:

```r
install.packages("romic")
```

If you'd like to use the most current version of the package, run the following instead:

```r 
install.packages("remotes")

remotes::install_github(
  "calico/romic",
  dependencies = TRUE
  )
```

And, check out romic's [pkgdown site](https://calico.github.io/romic/index.html) for organized documentation.

# Concept

![Romic Logo](https://github.com/calico/romic/blob/main/assets/romic_art/romic.png)

Romic structures high-dimensional 'omic datasets using a flexible format that can easily be modified using tidyverse-like verbs and visualized using ggplot. These operations can be dynamically applied using romic's shiny applications and modules to support exploratory data analysis and summarize results.

## Data Model

`Omic datasets are constructed by measuring a common set of features (transcripts, metabolites, ) across a set of samples. With such data, we could represent the same data using several different format:

- Wide :warning:: 
  - A matrix of I features x J samples including extra columns for feature-level attributes and extra rows for sample-level attributes.
  - While 'omic data is often shared in this format, it is problematic for three reasons.
    1. Each column of a dataframe has the same classes, and all values of a matrix have a common class. If sample-level variables are provided (such as the experimental design) then they would need to be tied to column attributes which are difficult to work with, although this is easier with specialized data structures such as H5AD.
    2. Matrix data is difficult to deal with. We would have to track which columns are measurements and which are feature attributes, and if we wanted to say select a subset of features and color them by a sample attribute, it would be colossal pain. 
    3. If we have two or more observation-level attributes, such as raw, normalized and log-transformed measurements then the information cannot nicely be formatted as a matrix.
- Tidy :star::
  - A table with one row for each measurement (I x J)
  - Each measurement is associated with the attributes of its corresponding features and samples. This is helpful for measurement-level operations such as plotting informed by the experimental design.
  - A couple of downsides of this representation are that:
    1. Feature- and sample-level attributes are highly duplicated so the representation is not particularly compact.
    2. Because feature- and sample attributes are duplicated, its difficult apply feature- or sample-level manipulation. For example, adding a gene's GO category would involve separately adding it for each measurement of that gene. This invariably adds extra complexity and time to the code and there is a risk that an inconsistent output is produced (such as a gene being associated with different GO categories depending on which measurement it is).
- Triple :sparkles::
  - Represent a dataset as three tables:
    - Features (I rows) - All unique features and feature-level attributes
    - Samples (J rows) - All unique samples and sample-level attributes
    - Measurements (I x J rows) - One row per observation. This will look like the tidy table above except the only feature- and sample-level attributes are the feature primary key (a variable which uniquely defines a feature) and the sample primary key (a variable which uniquely defines a sample)
  - This representation is powerful because feature- or sample-level attributes can be directly manipulated, and attributes of interest can be added to measurements on demand.
  - The major downside of this representation is the need for a more complex list data structure and the need to perform joins to pull in relevant information.

![Romic Functions](https://github.com/calico/romic/blob/main/assets/romic_summary.png)

Romic harnesses the tidy and the triple omic representations through the **tidy_omic** and **triple_omic** S3 classes. These formats each have their own pros and cons, and one is generally better than the other depending on the task. Taking advantage of this fact, tidy and triple omic objects can readily be interconverted by tracking a dataset's **design**.

The design reflects the schema of a triple_omic object, and as a result, how it can be naturally rearranged to- and from- a tidy_omic. It is stored as simple list:

- feature_pk (str): variable which uniquely defines a feature
- sample_pk (str): variable which uniquely defines a sample
- features (table): variables and types of feature attributes
- samples (table): variables and types of sample attributes
- measurements (table): variables and types of measurement attributes

Since tidy_omic and triple_omic representation can readily be inter-converted, many functions can use a tidy_omic or triple_omic input, converting between the formats as needed and returning the same type of object as the input if desired. This T* Omic abstraction is referred to through the **tomic** S3 class. 

## Modifying Tomics

Tidy and triple omic objects' core data are tables that can be directly manipulated and updated using conventional means (as long as the design is kept up to date). But, romic also includes methods which simplify working with this format and applying some common manipulations of high-dimensional data. Tidy and triple omics' core data are "tall data", so romic takes advantage of the tidyverse suite of packages for working with tall tabular data. Two common operations for manipulating tidy data are filtering and mutating results.

**filter_tomic** filters any table in a triple_omic to a range of values, values of interest, or based on a quosure (**filter_tomic**). Mutates are more varied, and include centering measurements (**center_tomic**), ordering features or samples as factors (**sort_tomic**) and adding lower-dimensional sample embedding (**add_pcs**)

## Visualizations

Romic provides several methods which can provide both a high-level summary of a dataset as well as interrogate specific features.

- **plot_heatmap** creates a ggplot-based heatmap to visualize a complete data matrix
- **plot_univariate** create a histogram of a numeric feature present in features, samples or measurements
- **plot_bivariate** creates a bivariate visualization of features, samples or measurements (including feature and sample attributes). If two numeric features are used then a scatter plot is created, while if the x-axis variables is categorical, a boxplot is created. Many types of visualizations can be created using this approach:
  - plotting PC1 ~ PC2 in the samples tables
  - creating a volcano plot of feature significance and effect sizes
  - plotting measurement magnitudes for specific features with elements of the experimental design on the y-axis.
  
The univariate and bivariate plots are simple, but they can do a lot when combined with tomics flexible data manipulation and shiny interactivity.

## Interactive Analysis with Shiny

Taking advantage of Romic's flexible representation and manipulation of high-dimensional datasets, romic bundles a number of
R Shiny Modules which can be composed into powerful Shiny Apps.

The main two apps are:

- **app_heatmap** Filter a tomic to data of interest, separate features or samples based on facets and organize results based on categories or hierarchical clustering.
- **app_flow** Create a bivariate or univariate plot of a features, samples or measurements, selected points of interest and use this to filter or tag the tomic data your working with. Then you can use this object going forward. This makes it easy to selected features of interest and then look at their patterns of variation.
