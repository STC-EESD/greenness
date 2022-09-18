
get.pcpuids.to.plot <- function(
    list.Sens.slopes = NULL
    ) {

    thisFunctionName <- "get.pcpuids.to.plot";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    vector.output <- c();
    for ( temp.variable in names(list.Sens.slopes) ) {
        vector.output <- c(
            vector.output,
            get.pcpuids.to.plot_inner(
                DF.input = list.Sens.slopes[[temp.variable]]
                )
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    vector.output <- sort(unique(vector.output));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( vector.output );

    }

##################################################
get.pcpuids.to.plot_inner <- function(
    DF.input = NULL
    ) {

    DF.input[,'diff.sum.sqs'] <- DF.input[,'litteR.sum.sqs'] - DF.input[,'sum.sqs.0'];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.stats <- c(
        'trend.slope.pv',
        'litteR.slope',
        'litteR.R.squared',
        'diff.sum.sqs'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    vector.output <- c();
    for ( temp.stat in temp.stats ) {
        vector.output <- c(
            vector.output,
            get.pcpuids.to.plot_by.statistic(
                DF.input   = DF.input,
                input.stat = temp.stat
                )
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( vector.output );

    }

get.pcpuids.to.plot_by.statistic <- function(
    DF.input   = NULL,
    input.stat = NULL
    ) {

    vector.output <- c();

    is.selected <- is.na(DF.input[,input.stat]);
    if ( sum(is.selected) > 0 ) {
        temp.pcpuids  <- DF.input[is.selected,'pcpuid'];
        temp.pcpuids  <- sample(x = temp.pcpuids, size = 5, replace = TRUE);
        vector.output <- c(vector.output,temp.pcpuids);
        }

    temp.threshold <- quantile(x = DF.input[,input.stat], probs = 0.01, na.rm = TRUE);
    is.selected    <- (DF.input[,input.stat] < temp.threshold);
    temp.pcpuids   <- DF.input[is.selected,'pcpuid'];
    vector.output  <- c(vector.output,temp.pcpuids);

    temp.threshold <- quantile(x = DF.input[,input.stat], probs = 0.99, na.rm = TRUE);
    is.selected    <- (DF.input[,input.stat] > temp.threshold);
    temp.pcpuids   <- DF.input[is.selected,'pcpuid'];
    vector.output  <- c(vector.output,temp.pcpuids);

    return( vector.output );

    }
