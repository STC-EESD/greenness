
permutation.test.Sens.slope <- function(
    DF.input       = NULL,
    DGUID.to.test  = NULL,
    n.permutations = 1000L
    ) {

    thisFunctionName <- "permutation.test.Sens.slope";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(gtools);
    require(trend);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames.years <- grep(x = colnames(DF.input), pattern = "^[0-9]+$", value = TRUE);
    min.year <- min(as.integer(colnames.years));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    input.vector <- DF.input[DF.input[,'DGUID'] == DGUID.to.test,colnames.years];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.ts <- stats::ts(
        data      = input.vector,
        start     = c(min.year,1),
        frequency = 1
        );

    results.trend.sens.slope <- trend::sens.slope(x = temp.ts);
    observed.Sens.slope      <- results.trend.sens.slope[['estimates']];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    permuted.Sens.slopes <- numeric(n.permutations);
    for ( index in seq(1,n.permutations) ) {
        temp.ts <- stats::ts(
            data      = gtools::permute(input.vector),
            start     = c(min.year,1),
            frequency = 1
            );
        results.trend.sens.slope    <- trend::sens.slope(x = temp.ts);
        permuted.Sens.slopes[index] <- results.trend.sens.slope[['estimates']];
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.output <- list(
        observed.Sens.slope  = observed.Sens.slope,
        permuted.Sens.slopes = permuted.Sens.slopes
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nsum( permuted.Sens.slopes < observed.Sens.slope )\n");
    print( sum( permuted.Sens.slopes < observed.Sens.slope )   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() exits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

##################################################
