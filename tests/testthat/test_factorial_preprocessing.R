context("Factorial Preprocessing")
test_that("Small example works", {
    # load data
    corp <- quanteda::corpus(quanteda::inaugTexts)
    corp <- quanteda::texts(corp)[1:50]

    # run preprocessor
    factorial_prep <- factorial_preprocessing(corp)
    expect_equal(length(factorial_prep), 2)

    # mantel(dist(factorial_prep$dfm_list[[1]]),
    #        dist(factorial_prep$dfm_list[[2]]))
})
