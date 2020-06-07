context("Factorial Preprocessing")
test_that("Small example works", {
    # load data
    documents <- quanteda::data_corpus_inaugural
    # use last 10 documents for example
    documents <- corpus_subset(documents,Year > 1980)
    # pull out the text
    documents <- quanteda::texts(documents)

    # run preprocessor
    factorial_prep <- factorial_preprocessing(documents,
                                              use_ngrams = FALSE)


})
