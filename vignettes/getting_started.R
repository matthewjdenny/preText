## ----eval=FALSE----------------------------------------------------------
#  install.packages("devtools")

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("matthewjdenny/preText")

## ----eval=FALSE----------------------------------------------------------
#  library(preText)

## ----eval=TRUE, fig.width=6, fig.height=6, fig.align ='center'-----------
library(preText)
library(quanteda)

# load in U.S. presidential inaugural speeches from Quanteda example data.
corp <- corpus(inaugTexts)
# use first 10 documents for example
documents <- texts(corp)[1:10]
# take a look at the document names
print(names(documents))

## ----eval=TRUE, fig.width=6, fig.height=6, fig.align ='center'-----------
preprocessed_documents <- factorial_preprocessing(
    documents,
    use_ngrams = TRUE,
    infrequent_term_threshold = 0.2,
    verbose = FALSE)

## ----eval=TRUE, fig.width=6, fig.height=6, fig.align ='center'-----------
names(preprocessed_documents)
head(preprocessed_documents$choices)

## ----eval=TRUE, fig.width=6, fig.height=6, fig.align ='center'-----------
preText_results <- preText(
    preprocessed_documents,
    dataset_name = "Inaugural Speeches",
    distance_method = "cosine",
    num_comparisons = 20,
    verbose = FALSE)

## ----eval=TRUE, fig.width=6, fig.height=12, fig.align ='center'----------
preText_score_plot(preText_results)

## ----eval=TRUE, fig.width=5, fig.height=3, fig.align ='center'-----------
regression_coefficient_plot(preText_results,
                            remove_intercept = TRUE)

