context("Factorial Preprocessing")
test_that("Small example works", {
    # load data
    documents <- quanteda::data_corpus_inaugural
    # use first 3 documents for example
    corp <- documents[1:3,]

    # run preprocessor
    factorial_prep <- factorial_preprocessing(corp)


})
