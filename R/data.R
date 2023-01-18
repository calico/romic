#' Brauer 2008
#'
#' An RNA expression (microarray) dataset looking at how yeast gene expression
#'   changes as nutrient sources and nutrient richness changes.
#'
#' @details This version of the dataset contains only 500 genes randomly
#'   selected from the ~6K genes in the complete dataset.
#'
#' @format A tibble with 18,000 rows and 8 columns:
#' \describe{
#'   \item{name}{Common gene name}
#'   \item{BP}{Gene ontology biological process of the gene}
#'   \item{MF}{Gene ontology molecular function of the gene}
#'   \item{sample}{Sample name}
#'   \item{nutrient}{Which nutrient limits growth (Glucose, Nitrogen,
#'     Phosphorous, Sulfur, Uracil, Leucine}
#'   \item{DR}{Dilution rate of the culture - basically how fast the cells
#'     are growing}
#'   \item{expression}{Expression level of the gene, log2 observation relative
#'     to a replicate of G0.3}
#' }
#' @source \url{https://pubmed.ncbi.nlm.nih.gov/17959824/}
"brauer_2008"

#' Brauer 2008 Tidy
#'
#' \code{\link{brauer_2008}} formatted as a tidy_omic object
#'
#' @rdname  brauer_2008
"brauer_2008_tidy"

#' Brauer 2008 Triple
#'
#' \code{\link{brauer_2008}} formatted as a triple_omic object
#'
#' @rdname brauer_2008
"brauer_2008_triple"

#' Prepare Example Datasets
#'
#' Format example datasets and add them to the package.
#'
#' @param seed a seed value used to reproducibly sample random genes.
#'
#' @returns None; used for side-effects.
prepare_example_datasets <- function(seed = 1234) {
  # microarray dataset

  brauer_2008 <- readr::read_delim(
    "http://varianceexplained.org/files/Brauer2008_DataSet1.tds",
    delim = "\t"
  ) %>%
    tidyr::separate(
      NAME,
      c("name", "BP", "MF", "systematic_name", "number"),
      sep = "\\|\\|"
    ) %>%
    dplyr::mutate_each(dplyr::funs(trimws), name:systematic_name) %>%
    dplyr::select(-number, -GID, -YORF, -GWEIGHT) %>%
    tidyr::gather(sample, expression, G0.05:U0.3) %>%
    tidyr::separate(
      sample,
      c("nutrient", "DR"),
      sep = 1,
      remove = FALSE,
      convert = TRUE
    ) %>%
    dplyr::mutate(name = ifelse(name == "", systematic_name, name))

  # shrink the dataset so it'll fit in the <1Mb limit

  set.seed(seed)
  reduced_genes <- brauer_2008 %>%
    dplyr::distinct(name) %>%
    dplyr::sample_n(500)

  brauer_2008 <- brauer_2008 %>%
    dplyr::semi_join(reduced_genes, by = "name")

  # tidy version

  brauer_2008_tidy <- create_tidy_omic(
    df = brauer_2008,
    feature_pk = "name",
    feature_vars = c("systematic_name", "BP", "MF"),
    sample_pk = "sample",
    sample_vars = c("nutrient", "DR")
  )

  # triple version

  brauer_2008_triple <- tidy_to_triple(brauer_2008_tidy)

  usethis::use_data(brauer_2008, overwrite = TRUE)
  usethis::use_data(brauer_2008_tidy, overwrite = TRUE)
  usethis::use_data(brauer_2008_triple, overwrite = TRUE)

  return(invisible(0))
}
