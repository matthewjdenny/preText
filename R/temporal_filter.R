temporal_filter <- function(dfm_object,
                            years,
                            proportion_threshold = 0.5){

    # create a temporary dfm object
    temp_dfm <- dfm_object

    #set word counts to 1
    temp_dfm@x <- rep(1,length(temp_dfm@x))

    temp_dfm <- as.matrix(temp_dfm)

    # now create a matrix with one row for each year
    unique_years <- unique(years)
    num_years <- length(unique_years)
    year_counts <- matrix(0,nrow = num_years, ncol = ncol(temp_dfm))

    # determine the number of years a term must appear in to be kept
    threshold <- ceiling(proportion_threshold * num_years)

    cat("threshold",threshold,"\n")
    # loop over years
    for(i in 1:num_years) {
        inds <- which(years == unique_years[i])
        if(length(inds) > 0) {
            temp <- temp_dfm[inds,]
            counts <- colSums(temp)

            # figure out which counts are greater than 0 and set them to 1
            gz <- which(counts > 0 )
            if (length(gz) > 0) {
                counts[gz] <- 1
            }

            #store
            year_counts[i,] <- counts
        }
    }

    # get column sums
    year_totals <- colSums(year_counts)

    cat("year total sum",sum(year_totals),"\n")

    # determine which column
    remove <- as.numeric(which(year_totals < threshold))
    cat("Removing",length(remove),"of",ncol(dfm_object),
        "total terms that appeared in less than",threshold,"years.\n")

    if (length(remove) > 0) {
        # remove the low frequency terms
        dfm_object <- dfm_object[,-remove]
    }

    #return the dfm_object
    return(dfm_object)
}
