find_optimal_number_of_topics <- function(i,
                                          dfm_list,
                                          interval = c(2,500)) {

    # find the (constrained) optimum using the optimize function
    optimum_k <- stats::optimize(f = get_perplexities,
                                 cur_dfm = dfm_list[[i]],
                                 interval = interval,
                                 maximum = FALSE)

    return(optimum_k)
}
