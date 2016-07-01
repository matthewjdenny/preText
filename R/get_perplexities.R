get_perplexities <- function(topics,
                             cur_dfm){

    # round all num topic values to whole numbers (allows for function to pass in
    # multiple num topic's)
    topics <- round(topics)

    # now calculate perplexities
    perplexities <- rep(0,length(topics))
    for (i in 1:length(topics)){
        fit <- topicmodels::LDA(quanteda::convert(cur_dfm, to = "topicmodels"),
                                k = topics[i])
        perplexities[i] <- topicmodels::perplexity(fit)
    }
    return(perplexities)
}
