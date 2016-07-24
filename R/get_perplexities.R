get_perplexities <- function(topics,
                             cur_dfm,
                             test_dfm){

    #now calculate perplexities
    perplexities <- rep(0,length(topics))
    for (i in 1:length(topics)){
        cat("fitting model with",topics[i],"topics...\n")
        fit <- topicmodels::LDA(quanteda::convert(cur_dfm, to = "topicmodels"),
                                k = topics[i])
        perplexities[i] <- topicmodels::perplexity(
            object = fit,
            newdata = quanteda::convert(test_dfm, to = "topicmodels"))
    }
    return(perplexities)
}
