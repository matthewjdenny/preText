library(preText)
library(quanteda)

context("Factorial Preprocessing")
test_that("Small single-core example works", {
    # load data
    documents <- quanteda::data_corpus_inaugural
    # use first 3 documents for example
    corp <- documents[1:10,]
    # run preprocessor
    factorial_prep <- factorial_preprocessing(corp,
                                              use_ngrams = FALSE, 
                                              verbose = FALSE)

    # check object properties
    testthat::expect_is(factorial_prep, 'list')
    testthat::expect_length(factorial_prep, 3)
    testthat::expect_named(factorial_prep, c("choices", "dfm_list", "labels"))
})

context("Factorial Preprocessing")
test_that("Small dual-core example works", {
    # load data
    documents <- quanteda::data_corpus_inaugural
    # use first 3 documents for example
    corp <- documents[1:10,]
    # run preprocessor
    factorial_prep <- factorial_preprocessing(corp,
                                              cores = 2,
                                              use_ngrams = FALSE, 
                                              verbose = FALSE)

    # check object properties
    testthat::expect_is(factorial_prep, 'list')
    testthat::expect_length(factorial_prep, 3)
    testthat::expect_named(factorial_prep, c("choices", "dfm_list", "labels"))
})
